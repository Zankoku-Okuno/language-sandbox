{-#LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, RankNTypes #-}
module Tc where

import AST

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef


newtype Tc s a = Tc { unTc :: ReaderT Env (ST s) a }
    deriving (Functor, Applicative, Monad)
runTc :: (forall s. Tc s a) -> Env -> a
runTc action env = runST $ runReaderT (unTc action) env


data Env = Env
    { vars :: [(VarName, Type)]
    }

typeOfVar :: VarName -> Tc s (Maybe Type)
typeOfVar x = do
    env <- Tc $ ask
    pure $ lookup x (vars env)

withVar :: (VarName, Type) -> Tc s a -> Tc s a
withVar binding (Tc action) = Tc $ withReaderT addVar action
    where
    addVar e = e { vars = binding : vars e }

newMetaVar :: Tc s (TcType s)
newMetaVar = Tc $ TcMetaVar <$> lift (newSTRef Nothing)



tcExpr :: TcType s -> Expr -> Tc s (TcType s, TcExpr)
tcExpr expect (NumLit i Nothing) = do
    tc <- unify expect TcU8
    t <- concretize tc >>= \case
            Nothing -> error "ambiguous type for numeral"
            Just t -> pure t
    pure (tc, TcNumLit i t)
tcExpr expect (NumLit i (Just annotatedType)) = do
    let annotatedTcType = toTcType annotatedType
    tc <- unify expect annotatedTcType
    tc' <- unify TcU8 annotatedTcType -- TODO make sure the annotation is accurate for the literal
    t <- concretize tc >>= \case
            Nothing -> error "ambiguous type for numeral"
            Just t -> pure t
    pure (tc, TcNumLit i t)
tcExpr expect (Var x) = do
    t <- lookup x . vars <$> Tc ask >>= \case
            Nothing -> error $ "undefined variable `" ++ x ++ "'"
            Just t -> pure t
    tc <- unify expect (toTcType t)
    pure (tc, TcVar x)
tcExpr expect (Block stmts) = do
    (tc, tcStmts) <- tcBlock expect stmts
    pure (tc, TcBlock tcStmts)
tcExpr expect (Call f args) = do
    args' <- tcArgs args
    let expectF = TcFunArgsType_ args' expect -- NOTE [TcFunArgsType]
    (TcFunType argSigs retType, f') <- tcExpr expectF f
    let argList = mapArgs args' argSigs
    pure (retType, TcCall f' argList)
    where
    mapArgs :: TcArgs s -> [TcArgSig s] -> [TcExpr]
    mapArgs (TcArgs ((_, e):poss) kws) (_ : cdr) =
        e : mapArgs (TcArgs poss kws) cdr
    mapArgs (TcArgs [] kws) ((TcArgSig name _) : cdr) =
        e : mapArgs (TcArgs [] kws') cdr
        where
        (e, kws') = go kws
        go [] = error "INTERNAL ERROR: tcExpr(Call).mapArgs.go"
        go (car@(kw, (_, e)):cdr)
            | kw == name = (e, cdr)
            | otherwise = let (e, kws') = go cdr in (e, car:kws')
    mapArgs (TcArgs [] []) [] = []
    mapArgs _ _ = error "INTERNAL ERROR: tcExpr(Call).mapArgs"


tcBlock :: TcType s -> [Stmt] -> Tc s (TcType s, [TcStmt])
tcBlock expect [] = error "empty block"
tcBlock expect [Expr e] = do
    (tc, tcExpr) <- tcExpr expect e
    pure (tc, [TcExpr tcExpr])
tcBlock expect [_] = error "block does not terminate with expression"
tcBlock expect (Expr e:stmts) = do
    mvar <- newMetaVar
    (_, tcExpr) <- tcExpr mvar e
    concretize mvar >>= \case
        Nothing -> error "ambiguous type for expression in block"
        -- TODO if not void-typed warn about unused result
        Just t -> pure ()
    (tc, tcStmts) <- tcBlock expect stmts
    pure (tc, TcExpr tcExpr : tcStmts)
tcBlock expect (VarStmt x e t : stmts) = do
    (varDef, varType) <- tcVarStmt t e
    (tc, tcStmts) <- withVar (x, varType) $ tcBlock expect stmts
    pure (tc, TcVarStmt x varDef varType : tcStmts)

tcVarStmt :: Maybe Type -> Maybe Expr -> Tc s (Maybe TcExpr, Type)
tcVarStmt Nothing Nothing = error "INTERNAL ERROR: var without type or expression"
tcVarStmt Nothing (Just e) = do
    mvar <- newMetaVar
    (tc, tcExpr) <- tcExpr mvar e
    t <- concretize mvar >>= \case
            Nothing -> error "ambiguous type for variable declaration"
            Just t -> pure t
    pure (Just tcExpr, t)
tcVarStmt (Just t) Nothing = pure (Nothing, t)
tcVarStmt (Just t) (Just e) = do
    (tc, tcExpr) <- tcExpr (toTcType t) e
    let t = fromTcType tc
    pure (Just tcExpr, t)

tcArgs :: Args -> Tc s (TcArgs s)
tcArgs (Args { posArgs = poss, kwArgs = kws }) = do
    tcPoss <- mapM tcPosArg poss
    tcKws <- mapM tcKwArg kws
    pure $ TcArgs
        { posTcArgs = tcPoss
        , kwTcArgs = tcKws
        }
    where
    tcPosArg e = do
        mvar <- newMetaVar
        tcExpr mvar e
    tcKwArg (kw, e) = do
        mvar <- newMetaVar
        tc <- tcExpr mvar e
        pure (kw, tc)


-- NOTE [TcFunArgsType]
unify :: TcType s -> TcType s -> Tc s (TcType s)
unify t1@(TcMetaVar s1) t2@(TcMetaVar s2) = do
    t1_m <- Tc . lift $ readSTRef s1
    t2_m <- Tc . lift $ readSTRef s2
    case (t1_m, t2_m) of
        (Just t1', Just t2') -> unify t1' t2'
        (Nothing, Nothing) -> do
            when (s1 /= s2) $ Tc . lift $ writeSTRef s1 (Just t2)
            pure t1
        (Just t1', Nothing) -> unify t1' t2
        (Nothing, Just t2') -> unify t1 t2'
unify (TcMetaVar s1) t2 = do
    t1_m <- Tc . lift $ readSTRef s1
    case t1_m of
        Nothing -> do
            Tc . lift $ writeSTRef s1 (Just t2)
            pure t2
        Just t1@(TcMetaVar _) -> do
            tc <- unify t1 t2
            Tc . lift $ writeSTRef s1 (Just tc)
            pure tc
        Just t1 -> unify t1 t2
unify t1 t2@(TcMetaVar _) = unify t2 t1
unify t1 t2@(TcFunArgsType_ _ _) = unify t2 t1
unify TcU8 TcU8 = pure TcU8
unify TcU8 _ = error "can't match U8 with whatever"
unify (TcFunArgsType_ args1 ret1) fType@(TcFunType args2 ret2) = do
    unifyArgs args1 args2
    unify ret1 ret2
    pure fType
    where
    unifyArgs :: TcArgs s -> [TcArgSig s] -> Tc s ()
    unifyArgs (TcArgs (pos:poss) kwArgs) args2 = do
        restArgs <- unifyPosArg pos args2
        unifyArgs (TcArgs poss kwArgs) restArgs
        where
        unifyPosArg :: (TcType s, TcExpr) -> [TcArgSig s] -> Tc s [TcArgSig s]
        unifyPosArg (t1, _) ((TcArgSig _ t2):rest) = do
            unify t1 t2
            pure rest
        unifyPosArg _ [] = error "can't match FunArgs with whatever (pos)"
    unifyArgs (TcArgs [] (kwArg:kwArgs)) args2 = do
        restArgs <- unifyKwArg kwArg args2
        unifyArgs (TcArgs [] kwArgs) restArgs
        where
        unifyKwArg :: (ArgName, (TcType s, TcExpr)) -> [TcArgSig s] -> Tc s [TcArgSig s]
        unifyKwArg kwArg@(argName1, (t1, _)) (sig@(TcArgSig argName2 t2):rest)
            | argName1 == argName2 = do
                unify t1 t2
                pure rest
                --TODO sanity check
                --case lookup argName1 rest of
                --    Nothing -> pure rest
                --    Just _ -> error "INTERNAL ERROR: duplicate kwArgs in function signature"
            | otherwise = do
                rest' <- unifyKwArg kwArg rest
                pure $ sig : rest'
        unifyKwArg _ [] = error "can't match FunArgs with whatever (kw)"
    unifyArgs (TcArgs [] []) [] = pure ()
    unifyArgs _ _ = error "can't match FunArgs with whatever"

unify (TcFunArgsType_ _ _) _ = error "can't match FunType with whatever"

concretize :: TcType s -> Tc s (Maybe Type)
concretize (TcMetaVar cell) = do
    t_m <- Tc . lift $ readSTRef cell
    case t_m of
        Nothing -> pure Nothing
        Just t -> concretize t
concretize t = pure . Just $ fromTcType t


------ FIXME move to its own module ------

data TcType s
    = TcU8 -- TODO replace with PrimType, make u8 available as a nominal type
    | TcFunType [TcArgSig s] (TcType s)
    | TcFunArgsType_ (TcArgs s) (TcType s) -- NOTE [TcFunArgsType]
    | TcMetaVar (STRef s (Maybe (TcType s)))
    -- TODO TcOverloaded [Type]
data TcArgSig s = TcArgSig
    { tcArgName :: VarName
    , tcArgType :: TcType s
    -- TODO arg modifiers, like implicit
    }
data TcArgs s = TcArgs
    { posTcArgs :: [(TcType s, TcExpr)]
    , kwTcArgs :: [(ArgName, (TcType s, TcExpr))]
    }


data TcExpr
    = TcNumLit Integer Type -- TODO use Rational instead of Integer
    | TcVar AbiName
    | TcCall TcExpr [TcExpr]
    | TcBlock [TcStmt]
    deriving (Eq, Show)
data TcStmt
    = TcExpr TcExpr
    | TcVarStmt VarName (Maybe TcExpr) Type
    deriving (Eq, Show)

data TcDirective
    = TcFunDef [TypeVar] AbiName Type TcExpr


toTcType :: Type -> TcType s
toTcType U8 = TcU8
toTcType (FunType args ret) = TcFunType (map toTcArgs args) (toTcType ret)
    where
    toTcArgs (ArgSig n t) = TcArgSig n (toTcType t)

fromTcType :: TcType s -> Type
fromTcType TcU8 = U8



-- NOTE [TcFunArgsType]
-- --------------------
-- This is only for use when type-checking funciton calls.
-- By the time you're done checking a call, the TcFunArgsType_ should disappear.
-- 
-- invariant: `unify` never returns a TcFunArgsType_
-- invariant: the `tc*` functions never return a TcFunArgsType_
-- invariant: `unify` is never called with two TcFunArgsType_

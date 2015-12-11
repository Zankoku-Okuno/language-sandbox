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
    { abi :: [(AbiName, Type)]
    , vars :: [(VarName, Type)]
    }

typeOfVar :: VarName -> Tc s (Maybe Type)
typeOfVar x = do
    env <- Tc $ ask
    let fromAbi = lookup x (abi env)
        fromVar = lookup x (vars env)
    pure $ case (fromAbi, fromVar) of
        (Just _, Just _) -> error "INTERNAL ERROR: name defined in both abi and vars"
        (Nothing, Nothing) -> Nothing
        (t, Nothing) -> t
        (Nothing, t) -> t

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
tcExpr expect (Block stmts) = do
    (tc, tcStmts) <- tcBlock expect stmts
    pure (tc, TcBlock tcStmts)

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
tcBlock expect (Var x e t : stmts) = do
    (varDef, varType) <- tcVar t e
    (tc, tcStmts) <- withVar (x, varType) $ tcBlock expect stmts
    pure (tc, TcVar x varDef varType : tcStmts)

tcVar :: Maybe Type -> Maybe Expr -> Tc s (Maybe TcExpr, Type)
tcVar Nothing Nothing = error "INTERNAL ERROR: var without type or expression"
tcVar Nothing (Just e) = do
    mvar <- newMetaVar
    (tc, tcExpr) <- tcExpr mvar e
    t <- concretize mvar >>= \case
            Nothing -> error "ambiguous type for variable declaration"
            Just t -> pure t
    pure (Just tcExpr, t)
tcVar (Just t) Nothing = pure (Nothing, t)
tcVar (Just t) (Just e) = do
    (tc, tcExpr) <- tcExpr (toTcType t) e
    let t = fromTcType tc
    pure (Just tcExpr, t)


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
unify TcU8 TcU8 = pure TcU8
unify TcU8 _ = error "TODO can't match U8 with whatever"

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
    | TcMetaVar (STRef s (Maybe (TcType s)))
data TcArgSig s = TcArgSig
    { tcArgName :: VarName
    , tcArgType :: TcType s
    -- TODO arg modifiers, like implicit
    }

data TcExpr
    = TcNumLit Integer Type -- TODO use Rational instead of Integer
    | TcBlock [TcStmt]
    deriving (Eq, Show)
data TcStmt
    = TcExpr TcExpr
    | TcVar VarName (Maybe TcExpr) Type
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
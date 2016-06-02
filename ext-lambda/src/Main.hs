{-#LANGUAGE TupleSections, LambdaCase, GeneralizedNewtypeDeriving #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Control.Applicative
import Control.Monad.Reader

{-
Aggregate Data Types:
    record, list, stream, array (memory buffer)
    maybe an associative array (but that's also just a `Map Int a`)
    maybe an object as a blend of callable, record, indexable
        object {foo = 3, call a b = a + b, [ix] = impl[ix], [low .. high] = impl[low-1 .. high-1]}
Operations on Aggregates
    project name: obj.foo
    project index: lst[i]
    update: obj{foo = bar}  obj{foo, bar = 9, ..}  obj{.., foo = .bar, bar =}  obj{ [3] = 9, .. }
    slice: lst[i .. j]  lst[.. -1]  lst[-3 ..]
-}

type Location = StartStop (FilePath, Line, Col)
type StartStop a = (a, a)
type Line = Int
type Col = Int

type Name = String

data Syntax
    = IntLit Location Integer
    | StrLit Location String
    | Var Location Name
    | Lam Location [Name] Syntax
    | Rec Location [(Name, Syntax)]
    -- TODO lists
    -- TODO recursive records (\sigma self {...} === let self = {...} in self)
    | Lzy Location Syntax
    | Exc Location Syntax
    -- TODO mutable cells, arrays

    | App Location Syntax [Syntax]
    | Prj Location Syntax [Into]
    | Upd Location Syntax [Into] (Bool, [Update])
    | Frc Location Syntax

    | Seq Location [(Maybe Name, Syntax)]
    -- TODO where
    deriving(Show)

data Into
    = IntoField Location String
    | IntoIndex Location Syntax
    | IntoSlice Location (Maybe Syntax) (Maybe Syntax)
    deriving(Show)

data Update
    = UpdField Location String UpdAction
    | UpdIndex Location Syntax UpdAction
    | UpdSlice Location (Maybe Syntax) (Maybe Syntax) UpdAction
    | UpdRename Location String String
    deriving(Show)

data UpdAction
    = Delete
    | Set Syntax
    | Modify Syntax
    deriving(Show)

-----------------------------

data Env = Env (IORef (Map Name Value)) (Maybe Env)
mkEnv :: Env -> [(Name, Value)] -> Machine Env
mkEnv parent scope = do
    cell <- M $ liftIO $ newIORef (Map.fromList scope)
    pure $ Env cell (Just parent)

data Value
    = Closure Env [(Name, Value)] [Name] Syntax
    | Record (Map Name Value)
-- FIXME an object has fields, getters, and setters. it combines function and object. Perhaps it even has a 'type' operator defined for it.
    | Lazy (IORef (Either (Env, Syntax) Value))
    | Execute (Env, Syntax)
    | IntVal Integer
    | StrVal String
    | Primitive Int [Value] ([Value] -> Machine Value)
instance Show Value where
    show (Closure _ _ params body) = "(Closure " ++ show params ++ " " ++ show body ++ ")"
    show (Record kvs) = "(Record " ++ show kvs ++ ")"
    show (IntVal i) = "(IntVal " ++ show i ++ ")"
    show (StrVal s) = "(StrVal " ++ show s ++ ")"
    show (Primitive _ _ _) = "Primitive"
data Index
    = IntIx Integer
    | StrIx String
    deriving(Eq, Ord, Show)

newtype Machine a = M { unMachine :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad)
runMachine :: Syntax -> IO Value
runMachine code = do
    cell <- newIORef env0
    runReaderT (unMachine $ eval code) (Env cell Nothing)
env0 = Map.fromList
    [ ("add", Primitive 2 [] (\[IntVal a, IntVal b] -> pure . IntVal $ a + b))
    ]

eval (IntLit _ i) = pure $ IntVal i
eval (StrLit _ s) = pure $ StrVal s
eval (Var loc var) = subst var >>= \case
    Nothing -> error $ show (fst loc) ++ " undefined variable " ++ show var
    Just val -> pure val
eval (Lam _ params body) = do
    env <- M ask
    pure $ Closure env [] params body
eval (Rec loc kvs) = do
    kvs <- mapM (\(k, v) -> (k,) <$> eval v) kvs
    pure $ Record (Map.fromList kvs)
eval (Lzy _ thunk) = do
    env <- M ask
    cell <- M $ liftIO $ newIORef (Left (env, thunk))
    pure $ Lazy cell
eval (Exc _ block) = do
    env <- M ask
    pure $ Execute (env, block)
eval (App loc f args) = do
    f <- eval f
    apply f args
eval (Prj loc obj ixs) = do
    obj <- eval obj
    project obj ixs
eval (Upd loc obj ixs upd) = do
    obj <- eval obj
    update obj ixs upd
eval (Frc loc e) = eval e >>= force
eval (Seq loc es) = do
    env' <- M ask >>= (`mkEnv` [])
    swapEnv env' $ last <$> mapM seq es
    where
    seq (Nothing, code) = eval code
    seq (Just var, code) = (var `define`) =<< eval code

apply :: Value -> [Syntax] -> Machine Value
apply (Closure env args0 params body) args | length args < length params = do
    let (params1, params2) = splitAt (length args) params
    args <- mapM eval args
    pure $ Closure env (args0 ++ zip params1 args) params2 body
apply f@(Closure _ _ params _) args | length args == length params = do
    args <- mapM eval args
    call f args
apply f@(Closure _ _ params _) args | length args > length params = do
    let (args1, args2) = splitAt (length params) args
    args1 <- mapM eval args1
    f <- call f args1
    apply f args2
apply (Primitive remaining args0 action) args | length args < remaining = do
    args <- mapM eval args
    pure $ Primitive (remaining - length args) (args0 ++ args) action
apply f@(Primitive remaining _ _) args | length args == remaining = do
    args <- mapM eval args
    call f args
apply f@(Primitive remaining _ _) args | length args > remaining = do
    let (args1, args2) = splitAt remaining args
    args1 <- mapM eval args1
    f <- call f args1
    apply f args2
apply it _ = error $ "not callable: " ++ show it

project :: Value -> [Into] -> Machine Value
project obj [] = pure obj
project obj (IntoField _ name:rest) = do
    v <- obj `getField` name
    project v rest
project obj (IntoIndex _ ix:rest) = do
    ix <- eval ix
    v <- obj `getIndex` ix
    project v rest
project it _ = error $ "not indexable: " ++ show it

update :: Value -> [Into] -> (Bool, [Update]) -> Machine Value
update obj [] (True, upds) = foldM alter obj upds
update (Record _) [] (False, upds) = foldM alter (Record Map.empty) upds
update obj [] (False, upds) = error "TODO choose a zero accumulator based on the type of obj"
update obj (IntoField _ name : rest) upds = do
    v <- obj `getField` name
    v <- update v rest upds
    (obj `setField` name) v

alter :: Value -> Update -> Machine Value
alter obj (UpdField _ k Delete) = do
    obj `delField` k
alter obj (UpdField _ k (Set v)) = do
    v <- eval v
    (obj `setField` k) v
alter obj (UpdField _ k (Modify f)) = do
    f <- eval f
    v <- obj `getField` k
    v <- call f [v]
    (obj `setField` k) v
alter obj (UpdRename _ k0 k') = do
    v <- obj `getField` k0
    obj <- obj `delField` k0
    (obj `setField` k') v

subst :: Name -> Machine (Maybe Value)
subst var = M $ ask >>= go
    where
    go (Env cell parent) = do
        kvs <- liftIO $ readIORef cell
        case var `Map.lookup` kvs of
            Just v -> pure (Just v)
            Nothing -> maybe (pure Nothing) go parent

define :: Name -> Value -> Machine Value
define var val = M $ do
    (Env cell _) <- ask
    liftIO $ modifyIORef cell (Map.insert var val)
    pure val

call :: Value -> [Value] -> Machine Value
call (Closure env args0 params body) args | length args == length params = do
    env' <- mkEnv env (args0 ++ zip params args)
    swapEnv env' (eval body)
call (Primitive remaining args0 action) args | length args == remaining =
    action (args0 ++ args)

force :: Value -> Machine Value
force (Lazy cell) = do
    (M $ liftIO $ readIORef cell) >>= \case
        Right val -> pure $ val
        Left (env, code) -> do
            val <- swapEnv env $ eval code
            M $ liftIO $ writeIORef cell (Right val)
            pure val
force (Execute (env, code)) = swapEnv env $ eval code

getField :: Value -> String -> Machine Value
getField obj@(Record kvs) k = case Map.lookup k kvs of
    Just v -> pure v
    Nothing -> error $ "no such field " ++ show k ++ " in: " ++ show obj

getIndex :: Value -> Value -> Machine Value
getIndex _ _ = error "TODO getIndex"

setField :: Value -> String -> Value -> Machine Value
setField (Record kvs) k v = pure $ Record (Map.insert k v kvs)
setField _ _ _ = error "TODO setField"

delField :: Value -> String -> Machine Value
delField (Record kvs) k = pure $ Record (Map.delete k kvs)
delField _ _ = error $ "TODO delField"

swapEnv :: Env -> Machine Value -> Machine Value
swapEnv env action = M $ local (const env) $ unMachine $ action

----------------------------

l0 = (("", 0, 0), ("", 0, 0))

expr1 = Prj l0 ( Rec l0 [("Dave", App l0 (Lam l0 ["x"] $ Var l0 "x") [StrLit l0 "I'm afraid I can't do that"])] ) [IntoField l0 "Dave"]
expr2 = Seq l0
    [ (Just "id", Lam l0 ["x"] $ Var l0 "x")
    , (Just "foo", Seq l0 [ (Just "foo", IntLit l0 0) ])
    , (Nothing, App l0 (Var l0 "add") [IntLit l0 1, App l0 (Var l0 "id") [Var l0 "foo"]])
    ]

main :: IO ()
main = do
    print =<< runMachine expr2


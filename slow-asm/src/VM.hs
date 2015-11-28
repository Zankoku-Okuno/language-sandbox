{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module VM (
      Name
    , Value(..), Addr(..)
    , Atom(..)
    , Block(..), Instr(..), Opcode
    , Jump(..), Arg(..), RetTo(..)
    , VM, runVM
    , getMem, putMem, delMem
    ) where

import Prelude hiding (cycle)

import Data.Maybe
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.State

type Name = String

data Value =
      Addr Addr
    | IntVal Integer

data Addr =
      Local Name
    | Global Name
    | Code Name



data Block = Block [Instr] Jump

type Opcode = [Value] -> VM ()
data Instr = Instr Opcode [Atom]

data Jump =
      Goto Atom [Arg]
    | Call Atom [Arg] RetTo
    | TailCall Atom [Arg]
    | Return [Atom]
    | Exit Atom [Arg]
    | Branch Atom Jump Jump

data Atom =
      Imm Value
    | Mem Addr

data Arg = Arg Name Atom

data RetTo = RetTo Name [Name]



data Machine = MState {
      stack :: [Frame]
    , globals :: Map Name Value
    , instrPtr :: (Block, Int)
    , code :: Map Name Block
}

data Frame = Frame {
      locals :: Map Name Value
    , returnPtr :: Maybe RetTo
}



newtype VM a = VM { unVM :: StateT Machine IO a }
    deriving(Functor, Applicative, Monad, MonadIO)

runVM :: [(Name, Block)] -> IO ([Value], Machine)
runVM code = runStateT (unVM loop) $ MState {
      stack = [Frame { locals = Map.empty, returnPtr = Nothing }]
    , globals = Map.empty
    , instrPtr = (fromJust $ lookup "main" code, 0)
    , code = Map.fromList code
}

loop :: VM [Value]
loop = do
    result <- cycle
    case result of
        Nothing -> loop
        Just values -> return values

cycle :: VM (Maybe [Value])
cycle = do
    (Block here next, ip) <- VM $ gets instrPtr
    if ip /= length here
        then const Nothing <$> execute (here !! ip)
        else case next of
            Branch p c a -> do
                p <- isTruthy <$> resolveAtom p
                jump $ if p then c else a
            _ -> jump next
    where
    isTruthy (IntVal 0) = False
    isTruthy (IntVal _) = True

execute :: Instr -> VM ()
execute (Instr opcode preargs) = do
    (block0, ip0) <- VM $ gets instrPtr
    VM $ modify $ \s -> s { instrPtr = (block0, ip0 + 1) }
    opcode =<< mapM resolveAtom preargs

jump :: Jump -> VM (Maybe [Value])
jump (Goto pretarget preargs) = do
    (block', args) <- prepGotoArgs pretarget preargs
    goto block' (Just args)
jump (Call pretarget preargs ret) = do
    (block', args) <- prepGotoArgs pretarget preargs
    let frame' = Frame { locals = Map.empty, returnPtr = Just ret }
    VM $ modify $ \s -> s { stack = frame' : stack s }
    goto block' Nothing
jump (TailCall pretarget preargs) = do
    (block', args) <- prepGotoArgs pretarget preargs
    (frame0:stack0) <- VM $ gets stack
    let frame' = frame0 { locals = args }
    VM $ modify $ \s -> s { stack = frame' : stack0 }
    goto block' Nothing
jump (Return prerets) = do
    target_m <- VM $ gets $ returnPtr . head . stack
    case target_m of
        Nothing -> Just <$> mapM resolveAtom prerets
        Just (RetTo target retparams) -> do
            (Just block') <- VM $ gets $ Map.lookup target . code
            rets <- mapM resolveAtom prerets
            when (length retparams /= length rets) $ error "wrong number of return values"
            let args = Map.fromList $ zip retparams rets
            VM $ modify $ \s -> s { stack = tail $ stack s }
            goto block' (Just args)
jump (Exit pretarget preargs) = do
    (block', args) <- prepGotoArgs pretarget preargs
    VM $ modify $ \s -> s { stack = tail $ stack s }
    goto block' (Just args) --FIXME the callee can't know whether it's clobbering the caller's namespace

resolveAtom :: Atom -> VM Value
resolveAtom (Mem (Local x)) = VM $ gets $ fromJust . Map.lookup x . locals . head . stack
resolveAtom (Mem (Global x)) = VM $ gets $ fromJust . Map.lookup x . globals
resolveAtom (Imm v) = return v

prepGotoArgs :: Atom -> [Arg] -> VM (Block, Map Name Value)
prepGotoArgs pretarget preargs = do
    (Addr (Code target)) <- resolveAtom pretarget
    (Just block') <- VM $ gets $ Map.lookup target . code
    args <- Map.fromList <$> mapM resolveArg preargs
    return (block', args)
    where
    resolveArg :: Arg -> VM (Name, Value)
    resolveArg (Arg x u) = resolveAtom u >>= \v -> return (x, v)

goto :: Block -> Maybe (Map Name Value) -> VM (Maybe a)
goto block' args = do
    case args of
        Nothing -> return ()
        Just args -> modLocals $ Map.union args
    VM $ modify $ \s -> s { instrPtr = (block', 0) }
    return Nothing



getMem :: Addr -> VM Value
getMem (Local x) = VM $ gets $ fromJust . Map.lookup x . locals . head . stack
getMem (Global x) = VM $ gets $ fromJust . Map.lookup x . globals

putMem :: Addr -> Value -> VM ()
putMem (Local x) v = modLocals (Map.insert x v)
putMem (Global x) v = VM $ modify $ \s -> s { globals = Map.insert x v (globals s) }

delMem :: Addr -> VM ()
delMem (Local x) = modLocals (Map.delete x)
delMem (Global x) = VM $ modify $ \s -> s { globals = Map.delete x (globals s) }



modLocals :: (Map Name Value -> Map Name Value) -> VM ()
modLocals f = VM $ do
    (frame0:stack0) <- gets stack
    let frame' = frame0 { locals = f $ locals frame0 }
    modify $ \s -> s { stack = frame':stack0 }
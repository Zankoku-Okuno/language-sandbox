{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State





data TopStmt = Value Value
data Value = Literal (Maybe String) String

instance Show Value where
	show (Literal Nothing v) = show v
	show (Literal (Just t) v) = t ++ show v


type Alist k v = [(k, v)]
data Cfg = BoolCfg Bool | StrCfg String


data Output = Output {
	  staticData :: [String]
	, staticAlloc :: [()]
	} deriving (Show)
output0 :: Output
output0 = Output {
	  staticData = []
	, staticAlloc = []
	}




newtype Compiler a = Compiler { unCompiler :: ReaderT (Alist String Cfg) (State Output) a }
	deriving (Functor, Applicative, Monad)
runCompiler :: Alist String Cfg -> Compiler a -> Output
runCompiler cfg = flip execState output0 . flip runReaderT cfg . unCompiler

emitStaticData :: Value -> Compiler ()
emitStaticData v = Compiler $ modify $ \s ->
	s { staticData = staticData s ++ [show v] }





compile :: [TopStmt] -> Compiler ()
compile [] = return ()
compile (Value v:rest) = emitStaticData v >> compile rest

main :: IO ()
main = print . runCompiler [] $ compile
	[ Value (Literal Nothing "hello")
	, Value (Literal (Just "i32") "123456")
	]
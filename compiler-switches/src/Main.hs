{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.State





data TopStmt = Value Value
data Value = Literal (Maybe String) String

instance Show Value where
	show (Literal Nothing v) = show v
	show (Literal (Just t) v) = t ++ show v


data Output = Output {
	  staticData :: [String]
	, staticAlloc :: [()]
	} deriving (Show)
output0 :: Output
output0 = Output {
	  staticData = []
	, staticAlloc = []
	}




newtype Compiler a = Compiler { unCompiler :: State Output a }
	deriving (Functor, Applicative, Monad)
runCompiler :: Compiler a -> Output
runCompiler = flip execState output0 . unCompiler

emitStaticData :: Value -> Compiler ()
emitStaticData v = Compiler $ modify $ \s ->
	s { staticData = staticData s ++ [show v] }





compile :: [TopStmt] -> Compiler ()
compile [] = return ()
compile (Value v:rest) = emitStaticData v >> compile rest

main :: IO ()
main = print . runCompiler $ compile
	[ Value (Literal Nothing "hello")
	, Value (Literal (Just "i32") "123456")
	]
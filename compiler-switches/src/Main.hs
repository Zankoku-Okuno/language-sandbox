{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State





data TopStmt =
	  Value Value
	| StaticIf CfgExpr [TopStmt] [TopStmt]
data Value = Literal (Maybe String) String
data CfgExpr =
	  CfgIsSet String
	| CfgEq String [String]
	| CfgNot CfgExpr
	| CfgAll [CfgExpr]
	| CfgAny [CfgExpr]

instance Show Value where
	show (Literal Nothing v) = show v
	show (Literal (Just t) v) = t ++ show v


type Alist k v = [(k, v)]
type Cfg = Alist String String


data Output = Output {
	  staticData :: [String]
	, staticAlloc :: [()]
	} deriving (Show)
output0 :: Output
output0 = Output {
	  staticData = []
	, staticAlloc = []
	}




newtype Compiler a = Compiler { unCompiler :: ReaderT Cfg (State Output) a }
	deriving (Functor, Applicative, Monad)
runCompiler :: Alist String String -> Compiler a -> Output
runCompiler cfg = flip execState output0 . flip runReaderT cfg . unCompiler

staticIf :: CfgExpr -> Compiler Bool
staticIf (CfgIsSet cfg) = Compiler $ do
	str <- asks $ lookup cfg
	return $ case str of
		Nothing -> False
		Just _ -> True
staticIf (CfgEq cfg cmps) = Compiler $ do
	str <- asks $ lookup cfg
	return $ case str of
		Nothing -> False
		Just str -> str `elem` cmps
staticIf (CfgNot expr) = not <$> staticIf expr
staticIf (CfgAll exprs) = and <$> mapM staticIf exprs
staticIf (CfgAny exprs) = or <$> mapM staticIf exprs

emitStaticData :: Value -> Compiler ()
emitStaticData v = Compiler $ modify $ \s ->
	s { staticData = staticData s ++ [show v] }





compile :: TopStmt -> Compiler ()
compile (Value v) = emitStaticData v
compile (StaticIf p c a) = do
	branch <- staticIf p
	mapM_ compile $ if branch then c else a

main :: IO ()
main = print . runCompiler [("?", "Y")] $ mapM_ compile
	[ StaticIf (CfgAny [CfgNot $ CfgNot $ CfgIsSet "no", CfgEq "?" ["yes", "Y"]])
		[ Value (Literal Nothing "hello") ]
		[ Value (Literal Nothing "goodbye") ]
	, Value (Literal (Just "i32") "123456")
	]
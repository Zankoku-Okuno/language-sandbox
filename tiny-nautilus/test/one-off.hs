{-#LANGUAGE LambdaCase #-}
import AST
import Tc

main :: IO ()
main = do
    print $ tc `all` goodterms
    return ()


tc :: (Expr, (Type, TcExpr)) -> Bool
tc (expr, expect) = runTc go env == expect
    where
    go = do
        mvar <- newMetaVar
        (tc, tcExpr) <- tcExpr mvar expr
        t <- concretize mvar >>= \case
            Nothing -> error $ "ambiguous type for: " ++ show expr
            Just t -> pure t
        return (t, tcExpr)





env :: Env
env = Env
    { abi = []
    , vars = []
    }

badterm :: Expr
badterm = NumLit 4 (Just $ FunType [] U8)


goodterms =
    [ (NumLit 4 Nothing, (U8, TcNumLit 4 U8))
    , (NumLit 4 (Just $ U8), (U8, TcNumLit 4 U8))
    , (Block [ Var "x" (Just $ NumLit 255 Nothing) Nothing
             , Expr $ NumLit 4 Nothing ]
      , (U8, TcBlock [ TcVar "x" (Just $ TcNumLit 255 U8) U8
                     , TcExpr $ TcNumLit 4 U8 ]))
    ]
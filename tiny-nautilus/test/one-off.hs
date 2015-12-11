{-#LANGUAGE LambdaCase #-}
import AST
import Tc

main :: IO ()
main = do
    print $ tc `all` goodterms
    --print tcBad
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
    { vars = [("(input: u8)->u8", FunType [ArgSig "input" U8] U8)]
    }

badterm :: Expr
badterm = Var "x"

tcBad = runTc go env
    where
    go = do
        mvar <- newMetaVar
        (tc, tcExpr) <- tcExpr mvar badterm
        t <- concretize mvar >>= \case
            Nothing -> error $ "ambiguous type for: " ++ show badterm
            Just t -> pure t
        return (t, tcExpr)


goodterms =
    [ (NumLit 4 Nothing, (U8, TcNumLit 4 U8))
    , (NumLit 4 (Just $ U8), (U8, TcNumLit 4 U8))
    , (Block [ VarStmt "x" (Just $ NumLit 255 Nothing) Nothing
             , Expr $ Var "x" ]
      , (U8, TcBlock [ TcVarStmt "x" (Just $ TcNumLit 255 U8) U8
                     , TcExpr $ TcVar "x" ]))
    , (Call (Var "(input: u8)->u8") (Args [NumLit 4 (Just U8)] [])
      , (U8, TcCall (TcVar "(input: u8)->u8") [TcNumLit 4 U8]))
    , (Call (Var "(input: u8)->u8") (Args [] [("input", NumLit 4 (Just U8))])
      , (U8, TcCall (TcVar "(input: u8)->u8") [TcNumLit 4 U8]))
    ]
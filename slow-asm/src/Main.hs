import VM
import Control.Monad
import Control.Monad.IO.Class

vmprint, assign, dec, whenZero :: Opcode

vmprint [IntVal n] = liftIO $ print n
vmprint [Addr (Local x)] = liftIO $ putStrLn $ "Addr-Local " ++ show x

assign [Addr x, v] = putMem x v

dec [dest, IntVal n] = assign [dest, IntVal (n - 1)]

whenZero [IntVal 0, x, v] = assign [x, v]
whenZero [IntVal _, _, _] = return ()



main :: IO ()
main = do
    putStrLn "Mesdames, messieurres, bon soir!"
    putStrLn "=== countdown ===" >> runVM countdown
    putStrLn "=== fixedtree ===" >> runVM fixedtree
    putStrLn "Goodbyte, and good luck!"

countdown =
    [ ("main", Block [
          Instr assign [Imm $ Addr $ Local "i", Imm $ Addr $ Local "i"]
        , Instr assign [Mem $ Local "i", Imm $ IntVal 6]
        ] $ Goto (Imm $ Addr $ Code "@main__loop1-top") [])
    , ("@main__loop1-top", Block [
          Instr dec [Imm $ Addr $ Local "i", Mem $ Local "i"]
        , Instr vmprint [Mem $ Local "i"]
        ] $ Branch (Mem $ Local "i")
                   (Goto (Imm $ Addr $ Code "@main__loop1-top") [])
                   (Goto (Imm $ Addr $ Code "@main__loop1-bot") []))
    , ("@main__loop1-bot", Block [] (Return []))
    ]

fixedtree =
    [ ("main", Block [
        ] $ Call (Imm $ Addr $ Code "topnode") [] (RetTo "@main__ret1" []))
    , ("@main__ret1", Block [
        ] $ Return [])

    , ("topnode", Block [
        ] $ Call (Imm $ Addr $ Code "leftnode") [] (RetTo "@topnode__ret1" []))
    , ("@topnode__ret1", Block [
          Instr vmprint [Imm $ IntVal 2]
        ] $ TailCall (Imm $ Addr $ Code "rightnode") [])
    
    , ("leftnode", Block [
          Instr vmprint [Imm $ IntVal 1]
        ] $ Return [])
    
    , ("rightnode", Block [
          Instr vmprint [Imm $ IntVal 3]
        ] $ Return [])
    ]
import VM
import Asm
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class

vmprint, assign, dec, whenZero :: Opcode

vmprint [IntVal n] = liftIO $ print n
vmprint [Addr (Local x)] = liftIO $ putStrLn $ "Addr-Local " ++ show x

assign [Addr x, v] = putMem x v

dec [dest, IntVal n] = assign [dest, IntVal (n - 1)]
mul [dest, IntVal a, IntVal b] = assign [dest, IntVal (a * b)]

whenZero [IntVal 0, x, v] = assign [x, v]
whenZero [IntVal _, _, _] = return ()

printgood [] = liftIO $ putStrLn "good"
printbad [] = liftIO $ putStrLn "bad"
gt10 [dest, IntVal n] = assign [dest, IntVal $ if n > 10 then 1 else 0]



main :: IO ()
main = do
    putStrLn "Mesdames, messieurres, bon soir!"
    putStrLn "=== countdown ===" >> runVM "main" countdown
    putStrLn "=== fixedtree ===" >> runVM "main" (Map.fromList fixedtree)
    putStrLn "=== exitwhenchain ===" >> runVM "main" (Map.fromList exitwhenchain)
    putStrLn "Goodbyte, and good luck!"

countdown = runAsm theisa $ do
    procedure "main" [
          instr ":=" [Imm $ Addr $ Local "i", Imm $ Addr $ Local "i"]
        , instr ":=" [Mem $ Local "i", Imm $ IntVal 6]
        , while [] (Mem $ Local "i") [
              instr "--" [Imm $ Addr $ Local "i", Mem $ Local "i"]
            , instr "print" [Mem $ Local "i"]
            ]
        ]

theisa = Map.fromList [
      (":=", assign)
    , ("--", dec)
    , ("print", vmprint)
    ]

fixedtree =
    [ ("main", Block [
        ] $ Call (Imm $ Addr $ Code "topnode") []
            $ RetTo ("@main__ret1", []) Map.empty)
    , ("@main__ret1", Block [
        ] $ Return [])

    , ("topnode", Block [
        ] $ Call (Imm $ Addr $ Code "leftnode") []
            $ RetTo ("@topnode__ret1", []) Map.empty)
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

exitwhenchain =
    [ ("main", Block [
        ] $ Call (Imm $ Addr $ Code "mulTen") [Arg "a" (Imm $ IntVal 3), Arg "b" (Imm $ IntVal 2)]
            $ RetTo ("@main__ret1", ["res"])
                  $ Map.fromList [("overflow", ("@main__exit1", ["overflow"]))])
    , ("@main__ret1", Block [
          Instr printgood []
        , Instr vmprint [Mem $ Local "res"]
        ] $ Call (Imm $ Addr $ Code "mulTen") [Arg "a" (Imm $ IntVal 3), Arg "b" (Imm $ IntVal 5)]
            $ RetTo ("@main__ret2", ["res"])
                  $ Map.fromList [("overflow", ("@main__exit1", ["overflow"]))])
    , ("@main__ret2", Block [
          Instr printgood []
        , Instr vmprint [Mem $ Local "res"]
        ] $ Return [])
    , ("@main__exit1", Block [
          Instr printbad []
        , Instr vmprint [Mem $ Local "overflow"]
        ] $ Return [])
    
    , ("mulTen", Block [
          Instr vmprint [Mem $ Local "a"]
        , Instr vmprint [Mem $ Local "b"]
        , Instr mul [Imm $ Addr $ Local "res", Mem $ Local "a", Mem $ Local "b"]
        , Instr gt10 [Imm $ Addr $ Local "overflow", Mem $ Local "res"]
        ] $ Branch (Mem $ Local "overflow")
                   (Exit "overflow" [Mem $ Local "res"])
                   (Return [Mem $ Local "res"]))
    ]
{-#LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module Asm where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State

import VM


newtype Asm a = Asm { unAsm :: State St a }
    deriving (Functor, Applicative, Monad)
data St = St {
      emitted :: Map Name Block
    , currProc :: Name
    , gensyms :: Map Name Integer
    , currBlock :: (Name, [Instr])
    , isa :: Map Name Opcode
}

runAsm :: Map Name Opcode -> Asm () -> Map Name Block
runAsm isa action = emitted $ execState (unAsm action) st0
    where
    st0 = St {
          emitted = Map.empty
        , currProc = undefined
        , gensyms = Map.empty
        , currBlock = undefined
        , isa = isa
        }


gensym :: String -> Asm Name
gensym affix = Asm $ do
    procname <- gets currProc
    gens <- gets gensyms
    let gens' = Map.alter incAffix affix gens
        n = fromJust $ Map.lookup affix gens'
    return $ "@" ++ procname ++ "__" ++ affix ++ show n
    where
    incAffix Nothing = Just 1
    incAffix (Just n) = Just (n + 1)

instr :: String -> [Atom] -> Asm Emit
instr opcode args = Asm $ do
    opcode <- gets $ fromJust . Map.lookup opcode . isa
    return $ EmitInstr $ Instr opcode args

while :: [Asm Emit] -> Atom -> [Asm Emit] -> Asm Emit
while cond br body = return $ EmitLoop cond br body

data Emit = EmitInstr Instr
          | EmitLabel Name
          | EmitLoop [Asm Emit] Atom [Asm Emit]

procedure :: Name -> [Asm Emit] -> Asm ()
procedure name body = do
    Asm $ modify $ \s -> s { currProc = name
                           , gensyms = Map.empty
                           , currBlock = (name, [])
                           }
    mapM_ emit body
    Asm $ modify $ addBlock (Return []) undefined

emit :: Asm Emit -> Asm ()
emit action = do
    action >>= \case
        EmitInstr it -> Asm $ modify $ addInstr it
        EmitLabel name' ->
            Asm $ modify $ addBlock (Goto (Imm $ Addr $ Code name') []) name'
        EmitLoop cond br body -> do
            test <- gensym "loop-test"
            top <- gensym "loop-top"
            bot <- gensym "loop-bot"
            Asm $ modify $ addBlock (Goto (Imm $ Addr $ Code test) []) test
            mapM_ emit cond
            Asm $ modify $ addBlock (Branch br (Goto (Imm $ Addr $ Code top) [])
                                               (Goto (Imm $ Addr $ Code bot) [])) top
            mapM_ emit body
            Asm $ modify $ addBlock (Goto (Imm $ Addr $ Code test) []) bot

addInstr it st@(St { currBlock = (name, instrs) }) =
    st { currBlock = (name, instrs ++ [it]) }
addBlock jump name' st@(St { currBlock = (name, instrs), emitted = e }) =
    st { emitted = Map.insert name (Block instrs jump) e
       , currBlock = (name', [])
       }

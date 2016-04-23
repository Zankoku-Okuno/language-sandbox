{-#LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RecordWildCards, ScopedTypeVariables #-}
module Packrat
    ( Message
    , Stream(..)
    , Parser
    ) where

import Control.Applicative
import Control.Monad


type Message = String -- TODO perhaps a richer error message system

class Location l t | l -> t where
    update :: l -> t -> l
    start :: l
class Location l t => Stream s t l | s -> t, s -> l where
    next :: s -> Maybe (t, s)

newtype Parser s t l u a = Parser { _run :: State s t l u -> Result s t l u a }

data State s t l u = State
    { loc_ :: l
    , input_ :: s
    , errors_ :: [(l, Error)]
    , userState_ :: u
    -- TODO cache the results of parses
    }

data Result s t l u a
    = Ok a (State s t l u)
    | Err l Error

data Error
    = MsgErr Message
    | Choices [Error]
    -- TODO more kinds of error


instance Stream s t l => Functor (Parser s t l u) where
    fmap = liftM
instance Stream s t l => Applicative (Parser s t l u) where
    pure = return
    (<*>) = ap
instance Stream s t l => Monad (Parser s t l u) where
    return x = Parser $ Ok x
    (Parser p1) >>= f = Parser $ \s0 -> case p1 s0 of
        Ok x s' -> let Parser p2 = f x in p2 s'
        Err l msgs -> Err l msgs
    fail msg = Parser $ \s0@(State {..}) -> Err loc_ (MsgErr msg)
instance Stream s t l => Alternative (Parser s t l u) where
    empty = Parser $ \s0@(State {..}) -> Err loc_ (MsgErr "unknown")
    p1 <|> p2 = choice [p1, p2]

parse :: forall s t l u a. Stream s t l => Parser s t l u a -> u -> s -> Either [(l, Error)] a
parse (Parser p) u0 input = case p s0 of
    Ok it (State { errors_ = [] }) -> Right it
    Ok _ (State { errors_ = errs }) -> Left errs
    Err l msg -> Left [(l, msg)]
    where
    s0 = State
        { loc_ = start
        , input_ = input
        , errors_ = []
        , userState_ = u0
        }


choice :: Stream s t l => [Parser s t l u a] -> Parser s t l u a
choice ps = Parser $ \s0 -> go s0 [] ps
    where
    go :: Stream s t l => State s t l u -> [Error] -> [Parser s t l u a] -> Result s t l u a
    go s0 []    [] = Err (loc_ s0) (MsgErr "given no choices")
    go s0 [err] [] = Err (loc_ s0) err
    go s0 errs  [] = Err (loc_ s0) (Choices errs)
    go s0 []    [Parser p] = p s0
    go s0 errs  (Parser p : ps) = case p s0 of
        ok@(Ok _ _) -> ok
        Err _ err' -> go s0 (mergeErr err') ps
            where
            mergeErr (Choices errs) = errs
            mergeErr err = [err]

lookahead :: Stream s t l => Parser s t l u a -> Parser s t l u (Maybe a)
lookahead (Parser p) = Parser $ \s0 -> case p s0 of
    (Ok x _) -> Ok (Just x) s0
    (Err _ _) -> Ok Nothing s0

expect :: Stream s t l => String -> Parser s t l u a -> Parser s t l u a
expect msg (Parser p) = Parser $ \s0@(State {..}) -> case p s0 of
    ok@(Ok _ _) -> ok
    Err _ _ -> Err loc_ (MsgErr $ "expected " ++ msg)

recover :: Stream s t l => Parser s t l u a -> Parser s t l u z -> Parser s t l u (Maybe a)
recover (Parser p) (Parser find) = Parser $ \s0 -> case p s0 of
    Ok x s' -> Ok (Just x) s'
    Err l msg -> skip s0
        where
        skip s = case (find s, (next . input_) s) of
            (Ok _ s', _) -> Ok Nothing (s' { errors_ = errors_ s' ++ [(l, msg)] })
            (Err _ _, Just (_, rest)) -> skip s { input_ = rest }
            (Err _ _, Nothing) -> Err l msg

satisfy :: Stream s t l => (t -> Bool) -> Parser s t l u t
satisfy p = Parser $ \s0@(State {..}) -> case next input_ of
    Just (t, rest) | p t -> Ok t (s0 { input_ = rest, loc_ = loc_ `update` t })
                   | otherwise -> Err loc_ (MsgErr "unexpected input")
    Nothing -> Err loc_ (MsgErr "end of input")

location :: Stream s t l => Parser s t l u l
location = Parser $ \s0@(State {..}) -> Ok loc_ s0

getState :: Stream s t l => Parser s t l u u
getState = Parser $ \s0@(State {..}) -> Ok userState_ s0

setState :: Stream s t l => u -> Parser s t l u ()
setState s' = Parser $ \s0@(State {..}) -> Ok () (s0 { userState_ = s'})
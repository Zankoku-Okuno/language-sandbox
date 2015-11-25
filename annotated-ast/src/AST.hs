{-#LANGUAGE PatternSynonyms #-}
module AST (
      AST
    , name, pattern Name
    , lambda, pattern Lambda
    , apply, pattern Apply
    , annot, chAnnot
    ) where

data AST a =
      Name'   a  String
    | Lambda' a  [String] (AST a)
    | Apply'  a  [AST a]

name = Name'
pattern Name x <- Name' _ x

lambda = Lambda'
pattern Lambda xs e <- Lambda' _ xs e

apply = Apply'
pattern Apply es <- Apply' _ es

annot (Name' a _) = a
annot (Lambda' a _ _) = a
annot (Apply' a _) = a

chAnnot (Name' _ x) a' = Name' a' x
chAnnot (Lambda' _ xs e) a' = Lambda' a' xs e
chAnnot (Apply' _ es) a' = Apply' a' es
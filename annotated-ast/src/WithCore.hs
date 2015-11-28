{-#LANGUAGE PatternSynonyms #-}
module WithCore where

data Wrap ast ann = Wrap ast ann

unwrap :: Wrap ast ann -> (ast, ann)
unwrap (Wrap ast ann) = (ast, ann)

mapwrap :: (ast -> ast') -> Wrap ast ann -> Wrap ast' ann
mapwrap f (Wrap x a) = Wrap (f x) a

instance (Eq ast) => Eq (Wrap ast ann) where
    (Wrap a _) == (Wrap b _) = a == b
instance (Show ast) => Show (Wrap ast ann) where
    show (Wrap it _) = show it



type AST = Wrap Core ()
data Core =
      Name String
    | Abs [String] AST
    | Apply [AST]

pattern WName x <- Wrap (Name x) _
pattern WAbs xs e <- Wrap (Abs xs e) _
pattern WApply es <- Wrap (Apply es) _
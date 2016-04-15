module PrimTypes where


data PrimTypeFormat
    = Int_Wrap
    | TwosCompl_Wrap
    | BinFloat
    | DecFloat
    deriving (Eq, Show)

data PrimType = PrimType {
      pt_format :: PrimTypeFormat
    , pt_size :: Integer
    , pt_align :: Integer
}

instance Show PrimType where
    show p =
        "#<" ++
        show (pt_format p) ++ " " ++
        show (pt_size p) ++
        (if pt_align p == pt_size p then "" else " align " ++ show (pt_align p)) ++
        ">"

class TypeLayout t where
    sizeof :: t -> Maybe Integer
    alignof :: t -> Maybe Integer

instance TypeLayout PrimType where
    sizeof = Just . pt_size
    alignof = Just . pt_align
module Main where


data PrimTypeFormat =
	  Address
    | Offset
    | UInt
    | SInt
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


main = print $ PrimType BinFloat 64 128
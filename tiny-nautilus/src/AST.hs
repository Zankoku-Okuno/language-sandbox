module AST where

------ all the ways to name things ------
type AbiName = String
type VarName = String
type TypeName = String
type TypeVar = String
type ArgName = String
type FieldName = String

------ primitive data types --------
data Sign = Signed | Unsigned
data Base = Binary | Decimal
data Format = Fixed Sign | Float Base

data PrimType = PrimType
    { size_bytes  :: Integer
    , radix_bits  :: Integer
    , align_bytes :: Integer
    , format      :: Format
    }

------ Syntax ------

data Type
    = U8 -- TODO replace with PrimType, make u8 available as a nominal type
    | FunType [ArgSig] Type
    deriving (Eq, Show)
data ArgSig = ArgSig
    { argName :: VarName
    , argType :: Type
    -- TODO arg modifiers, like implicit
    }
    deriving (Eq, Show)
data Args = Args
    { posArgs :: [Expr]
    , kwArgs :: [(ArgName, Expr)]
    }
    deriving (Eq, Show)

data Expr
    = NumLit Integer (Maybe Type) -- TODO use Rational instead of Integer
    | Var VarName
    | Call Expr Args
    | Block [Stmt]
    deriving (Eq, Show)
data Stmt
    = Expr Expr
    | VarStmt VarName (Maybe Expr) (Maybe Type)
    deriving (Eq, Show)

data Directive
    = FunDef [TypeVar] AbiName Type Expr
    deriving (Eq, Show)
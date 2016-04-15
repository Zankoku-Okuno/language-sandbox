{-#LANGUAGE RecordWildCards #-}
module TypeSyntax where
import Data.Maybe
import Data.List
import Control.Applicative

type Map k v = [(k, v)]

-- FIXME I'm using Integer in several places where Natural is better
-- FIXME a fixed number in many places should be replaced by an expression
-- FIXME I need a monad with `Reader (Machine, TypeEnv)` and `Either LayoutError`

data TypeSyntax
    = TypeName String
    | VecSyntax TypeSyntax Integer
    | PtrSyntax TypeSyntax
    | FunSyntax [ArgSyntax] TypeSyntax
    | StructSyntax [String] [MemberSyntax]
    -- TODO instantiated struct
    -- TODO union syntax
    -- TODO enum syntax
    -- TODO variant sugar
data ArgSyntax = ArgSyntax
    { arg_typeSyntax :: TypeSyntax
    , arg_nameSyntax :: String
    , arg_implicitSyntax :: Implicitness
    }
data MemberSyntax
    = DataMemberSyntax String TypeSyntax
    | OpenDataMemberSyntax (Maybe String) TypeSyntax
    | BitfieldMemberSyntax (Maybe String) TypeSyntax [BitfieldSyntax]
    | PaddingMemberSyntax Integer
    | AlignmentMemberSyntax Integer
    | DynArrMemberSyntax String TypeSyntax
data BitfieldSyntax = BitfieldSyntax
    { bf_nameSyntax :: String
    , bf_typeSyntax :: TypeSyntax
    , bf_sizeSyntax :: Integer
    , bf_shiftRightSyntax :: Integer
    }

data Type
    = OpaqueType String
    | NominalType String Type
    | PrimType
        { pt_format :: PrimTypeFormat
        , pt_size :: Integer
        , pt_align :: Integer
        }
    | VecType Type Integer
    | PtrType Type
    | FunType [Arg] Type
    | StructType
        { st_tvars :: [String]
        , st_members :: Map String Member
        , st_size :: Maybe Integer
        , st_align :: Integer
        }
data Arg = Arg
    { arg_type :: Type
    , arg_name :: String
    , arg_implicit :: Implicitness
    }
data Implicitness = Explicit | Implicit
data Member
    = Member Type Integer
    | Bitfield
        { bf_container :: (Integer, Integer) -- offset, size
        , bf_type :: Type
        , bf_mask :: (Integer, Integer) -- (high,low) limits of range of bits to keep, inclusive
        , bf_shiftRight :: Integer
        }

data PrimTypeFormat
    = PrimIntegral Signedness OvfMode RadixPoints
    | PrimFloat FloatFormat
    | PrimBCD Signedness BCDPackedness RadixPoints
    | PrimBlock
    -- MAYBE FIXME or just let format be a string?
data Signedness = Unsigned | TwosComplement | OnesComplement | SignBit
data OvfMode = Undefined | Wrap | Check | Carry | Saturate
newtype RadixPoints = RadixPoints Integer
data FloatFormat = IEEE
data BCDPackedness = BCDPacked | BCDUnpacked


type TypeEnv = Map String Type


typeFromSyntax :: TypeEnv -> TypeSyntax -> Maybe Type
typeFromSyntax e (TypeName name) = lookup name e
typeFromSyntax e (VecSyntax t len) = do
    t' <- typeFromSyntax e t
    return $ VecType t' len
typeFromSyntax e (PtrSyntax t) = PtrType <$> typeFromSyntax e t
typeFromSyntax e (FunSyntax args res) = do
    args' <- mapM (argFromSyntax e) args
    res' <- typeFromSyntax e res
    return $ FunType args' res'
typeFromSyntax e (StructSyntax tvars members) = do
    let e' = map (\tvar -> (tvar, OpaqueType tvar)) tvars ++ e
    (members', layout) <- membersFromSyntax e' [] (0, 1) members
    return $ StructType
        { st_tvars = tvars
        , st_members = members'
        , st_size = fst layout
        , st_align = snd layout
        }

argFromSyntax :: TypeEnv -> ArgSyntax -> Maybe Arg
argFromSyntax e arg = do
    t <- typeFromSyntax e (arg_typeSyntax arg)
    return $ Arg
        { arg_type = t
        , arg_name = arg_nameSyntax arg
        , arg_implicit = arg_implicitSyntax arg
        }

membersFromSyntax :: TypeEnv
                  -> Map String Member -- previously defined members
                  -> (Integer, Integer) -- current offset, alignment
                  -> [MemberSyntax] -- members to go
                  -> Maybe (Map String Member, (Maybe Integer, Integer))
membersFromSyntax _ output (offset, align) [] = do
    -- add padding to fill the structure out so that it is suitable for use in a vector
    let size = alignUpto align offset
    return $ (output, (Just size, align))
membersFromSyntax e output (offset, maxalign) (DataMemberSyntax name t : rest) = do
    t' <- typeFromSyntax e t
    align <- alignof t'
    size <- sizeof t'
    let aligned_offset = alignUpto align offset
        output' = output ++ [(name, Member t' aligned_offset)]
        offset' = aligned_offset + size
        maxalign' = max align maxalign
    if isNameIn name output
        then Nothing
        else membersFromSyntax e output' (offset', maxalign') rest
membersFromSyntax e output (offset, maxalign) (OpenDataMemberSyntax name_m t : rest) = do
    t' <- typeFromSyntax e t
    align <- alignof t'
    size <- sizeof t'
    let aligned_offset = alignUpto align offset
        outerMember = case name_m of
            Nothing -> Nothing
            Just name -> Just (name, Member t' aligned_offset)
        innerMembers = adjustOffset aligned_offset (membersof t')
        output' = output ++ maybeToList outerMember ++ innerMembers
        offset' = aligned_offset + size
        maxalign' = max align maxalign
        outerMemberName = fst <$> outerMember
        memberNames = map fst $ maybeToList outerMember ++ innerMembers
    if     maybe False (`isNameIn` output) outerMemberName
        || maybe False (`isNameIn` innerMembers) outerMemberName
        || any (`isNameIn` output) memberNames
        then Nothing
        else membersFromSyntax e output' (offset', maxalign') rest
membersFromSyntax e output (offset, maxalign) (BitfieldMemberSyntax name_m t bfs : rest) = do
    containerType <- typeFromSyntax e t
    align <- alignof containerType
    size <- sizeof containerType
    let aligned_offset = alignUpto align offset
        outerMember = case name_m of
            Nothing -> Nothing
            Just name -> Just (name, Member containerType aligned_offset)
    innerMembers <- bitfieldsFromSyntax e (aligned_offset, size) size bfs
    let output' = output ++ maybeToList outerMember ++ innerMembers
        offset' = aligned_offset + size
        maxalign' = max align maxalign
        outerMemberName = fst <$> outerMember
        memberNames = map fst $ maybeToList outerMember ++ innerMembers
    if     maybe False (`isNameIn` output) outerMemberName
        || maybe False (`isNameIn` innerMembers) outerMemberName
        || any (`isNameIn` output) memberNames
        then Nothing
        else membersFromSyntax e output' (offset', maxalign') rest
membersFromSyntax e output (offset, maxalign) (PaddingMemberSyntax padding : rest) = do
    let offset' = offset + padding
    membersFromSyntax e output (offset', maxalign) rest
membersFromSyntax e output (offset, maxalign) (AlignmentMemberSyntax align : rest) = do
    let offset' = alignUpto align offset
        maxalign' = max align maxalign
    membersFromSyntax e output (offset', maxalign') rest
membersFromSyntax e output (offset, maxalign) (DynArrMemberSyntax name t : []) = do
    t' <- typeFromSyntax e t
    -- FIXME if I don't know alignment of t, then I just don't know the alignment of the struct
    let aligned_offset = maybe offset (`alignUpto` offset) (alignof t')
        output' = output ++ [(name, Member t' aligned_offset)]
        maxalign' = maybe maxalign (max maxalign) (alignof t')
    case lookup name output of
        Just _ -> Nothing
        Nothing -> return (output', (Nothing, maxalign'))
    -- FIXME if we have a dynamic array of an unknown type, we'd rather state that alignment is at least such-and-such, not that it is exact
membersFromSyntax e output layout (DynArrMemberSyntax _ _ : _) = Nothing

bitfieldsFromSyntax :: TypeEnv
                    -> (Integer, Integer) -- containter (offset, size)
                    -> Integer -- high mask limit
                    -> [BitfieldSyntax]
                    -> Maybe (Map String Member)
bitfieldsFromSyntax e container hMask [] = return []
bitfieldsFromSyntax e container hMask (BitfieldSyntax {..} : rest) = do
    let fieldSize = bf_sizeSyntax
        lMask = hMask - fieldSize
    () <- if lMask < 0 then Nothing else Just ()
    t' <- typeFromSyntax e bf_typeSyntax
    () <- case t' of
        PrimType { pt_format = PrimIntegral _ _ _ } -> Just ()
        -- TODO BCD could be allowed, but the number of bits must evenly store decimal digits
        PtrType _ -> Just ()
        _ -> Nothing
    let bf = Bitfield
                { bf_type = t'
                , bf_container = container
                , bf_mask = (hMask-1, lMask)
                , bf_shiftRight = bf_shiftRightSyntax
                }
    bfs <- bitfieldsFromSyntax e container lMask rest
    return $ (bf_nameSyntax, bf):bfs





sizeof :: Type -> Maybe Integer
sizeof (OpaqueType _) = Nothing
sizeof (NominalType _ t) = sizeof t
sizeof (PrimType { pt_size = it }) = Just it
sizeof (VecType t len) = do
    elemsize <- sizeof t
    elemalign <- alignof t
    if elemsize `mod` elemalign /= 0
    -- to pack data tight, the element size must be a multiple of its alignment
        then Nothing
        else Just $ len * elemsize
sizeof (PtrType _) = Just 64 --FIXME lookup the size of a ptr based on the chosen machine
sizeof (FunType _ _) = Just 64 --FIXME lookup the size of a function ptr based on the chosen machine
sizeof (StructType { st_size = it }) = it

alignof :: Type -> Maybe Integer
alignof (OpaqueType _) = Nothing
alignof (NominalType _ t) = alignof t
alignof (PrimType {..}) = Just pt_align
alignof (VecType t _) = alignof t
alignof (PtrType _) = Just 64 --FIXME lookup the align of a ptr based on the chosen machine
alignof (FunType _ _) = Just 64 --FIXME lookup the align of a function ptr based on the chosen machine
alignof (StructType {..}) = Just st_align

membersof :: Type -> Map String Member
membersof (StructType {..}) = st_members
membersof _ = []

isNameIn :: String -> Map String Member -> Bool
isNameIn name members = isJust $ lookup name members

adjustOffset :: Integer -> Map String Member -> Map String Member
adjustOffset offset = map $ \(name, Member t loc) -> (name, Member t (loc + offset))

alignUpto :: Integer -> Integer -> Integer
alignUpto align i = if mismatch == 0 then i else i + align - mismatch
    where mismatch = i `mod` align





instance Show TypeSyntax where
    show (TypeName name) = name
    show (VecSyntax t len) = show t ++ "[" ++ show len ++ "]"
    show (PtrSyntax t) = show t ++ "*"
    show (FunSyntax args res)
        = "(" ++ intercalate ", " (map showArg args) ++ ") -> " ++ show res
        where
        showArg (ArgSyntax {..})
            = case arg_implicitSyntax of { Implicit -> "implicit "; _ -> "" }
            ++ arg_nameSyntax ++ ": " ++ show arg_typeSyntax
    show (StructSyntax tvars members)
        =  "struct"
        ++ (if null tvars then "" else "(" ++ intercalate ", " tvars ++ ")")
        ++ " {\n\t" ++ intercalate "\n\t" (map showMember members) ++ "\n}"
        where
        showMember (DataMemberSyntax name t)
            = name ++ ": " ++ show t ++ ";"
        showMember (OpenDataMemberSyntax name_m t)
            =  maybe "" (\name -> "(" ++ name ++ "): ") name_m
            ++ show t ++ ";"
        showMember (BitfieldMemberSyntax name_m t fields)
            = (maybe "" (\name -> "(" ++ name ++ "): ") name_m)
            ++ show t ++ " "
            ++ "{\n\t" ++ intercalate "\n\t" (map showBitfield fields) ++ "\n};"
            where
            showBitfield (BitfieldSyntax {..})
                =  bf_nameSyntax
                ++ " [" ++ show bf_sizeSyntax ++ "]"
                ++ (if bf_shiftRightSyntax == 0 then "" else "<<" ++ show bf_shiftRightSyntax)
                ++ ": " ++ show bf_typeSyntax
                ++ ";"
        showMember (PaddingMemberSyntax bytes)
            = "padding " ++ show bytes ++ ";"
        showMember (AlignmentMemberSyntax bytes)
            = "alignto " ++ show bytes ++ ";"
        showMember (DynArrMemberSyntax name t)
            = name ++ ": " ++ show t ++ "[];"




instance Show Type where
    show (OpaqueType name) = name
    show (NominalType name _) = name
    show (PrimType {..}) = showPrimType (pt_size, pt_align) pt_format
    show (VecType t len) = show t ++ "[" ++ show len ++ "]"
    show (PtrType t) = show t ++ "*"
    show (FunType args res) = "(" ++ intercalate ", " (map showArg args) ++ ") -> " ++ show res
        where
        showArg (Arg {..})
            = case arg_implicit of { Implicit -> "implicit "; _ -> "" }
            ++ arg_name ++ ": " ++ show arg_type
    show (StructType {..})
        =  "struct"
        ++ (if null st_tvars then "" else "(" ++ intercalate ", " st_tvars ++ ")")
        ++ "<" ++ maybe "?" show st_size ++ ", " ++ show st_align ++ ">"
        ++ "{" ++ intercalate "; " (map showMember $ st_members) ++ "}"
        where
        showMember (name, Member t offset) = name ++ "(+" ++ show offset ++ "): " ++ show t
        showMember (name, Bitfield {..})
            =  name
            ++ "(+" ++ show (fst bf_container) ++ ":" ++ show (snd bf_container)
            ++ "[" ++ show (fst bf_mask) ++ "," ++ show (snd bf_mask) ++ "]"
            ++ (if bf_shiftRight == 0 then "" else "<<" ++ show bf_shiftRight)
            ++ "): "
            ++ show bf_type

showPrimType :: (Integer, Integer) -> PrimTypeFormat -> String
showPrimType (size, align) (PrimIntegral sign mode (RadixPoints radixpts))
    =  case sign of {
            Unsigned -> "u"           ;
            TwosComplement -> "i"     ;
            OnesComplement -> "i_one" ;
            SignBit -> "i_sbit"       }
    ++ (if radixpts == 0 then show size else show (size-radixpts) ++ "_" ++ show radixpts)
    ++ (if align == size then "" else "a" ++ show align)
    ++ case mode of {
            Undefined -> "_unsafe"  ;
            Wrap -> ""              ;
            Check -> "_check"       ;
            Carry -> "_carry"       ;
            Saturate -> "_saturate" }
    ++ "_t"
showPrimType (size, align) (PrimFloat format)
    =  "f" ++ show size
    ++ (if align == size then "" else "a" ++ show align)
    ++ "_t"
showPrimType (size, align) (PrimBCD sign pack radix)
    =  case pack of {
            BCDPacked -> "bcdp"  ;
            BCDUnpacked -> "bcd" }
    ++ "TODO"
    ++ "_t"
showPrimType (size, align) PrimBlock
    = "block" ++ show size
    ++ (if align == size then "" else "_" ++ show align)
    ++ "_t"
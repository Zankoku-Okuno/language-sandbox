module Structs where

import PrimTypes

{-
members
    type
    name
padding
    size in bytes
    adds an inaccessible field of given size (may be automatically padded itself)
alignment
    size in bytes
    forces the next member to have at least the given alignment
    or, if no next member, add padding so the next peice of memory after this is aligned
root NO -- nested structs would require special-casing, we would need forward size and backward size to understand a type, not just size
    the following member is the one at offset zero
    previous pointers are at negative offset
    only one root allowed
bitfields
    the whole bitfield
        a containing type
        padded to fill that type, so we can control which fields are in what words
        possibly given a name so the whole thing can be copied at once
    each feild
        num bits
        signed/unsigned
        shift left (i.e. don't shift all the way right)

dependent arrays?
    an array whose length is given by a prior (closer to root) member
    -- done naively, there are probems when a struct needs two dependent arrays, but...
    all dependent arrays must appear at the end of a struct (though a dynamic array could appear after them)
    each is specified with a member name, which must be defined earlier and be an unsigned integer
    {- e.g.
        struct gc_data {
            raw_btyes : uint;
            count_objs : uint;
            unboxed : byte[raw_bytes];
            boxed : gc_ptr[count_objs];
        }
     -}
dynamic arrays
    only allowed as last member
    size unknown from structure alone, programmer is responsible for it




struct Buf(a) {
    size_t cap;
    size_t len;
    -> a[.cap] arr;
}


-}

type Map k v = [(k, v)]




data Type
    = OpaqueType String
    | PrimType PrimTypeFormat
    | StructType StructInfo



data StructSpec = StructSpec
    { structTypevars :: [String]
    , memberSpec :: [MemberSpec]
    }
data MemberSpec
    = DataSpec Type String
    | PaddingSpec (Either Integer Type)
    | AlignmentSpec (Either Integer Type)
    | RootSpec MemberSpec
    -- | BitFieldSpec BitFieldInfo
    -- | DepArr Type ? String
    | DynArrSpec Type String


data StructInfo = StructInfo
    { structSize :: Integer
    , structAlign :: Integer
    , memberInfo :: Map String MemberInfo
    }
data MemberInfo = MemberInfo
    { memberType :: Type
    , memberOffset :: Integer
    , memberBitmaskshift :: Maybe (Integer, Integer)
    }


type Env = Map String Type
--compileStruct :: Env -> StructSpec -> Maybe StructInfo



instance TypeLayout StructInfo where
    sizeof = Just . structSize
    alignof = Just . structAlign
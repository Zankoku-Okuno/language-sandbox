module Main where
import TypeSyntax


main = mapM main1 types
    where
    main1 t = do
        print t
        putStrLn "--->"
        print $ typeFromSyntax startEnv t
        putStrLn "========"

startEnv :: TypeEnv
startEnv =
    [ ("bool", PrimType
                { pt_format = PrimIntegral Unsigned Wrap (RadixPoints 0)
                , pt_size = 1
                , pt_align = 8
                })
    , ("u32", PrimType
                { pt_format = PrimIntegral Unsigned Wrap (RadixPoints 0)
                , pt_size = 32
                , pt_align = 32
                })
    , ("i32", PrimType
                { pt_format = PrimIntegral TwosComplement Wrap (RadixPoints 0)
                , pt_size = 32
                , pt_align = 32
                })
    , ("size_t", PrimType
                { pt_format = PrimIntegral Unsigned Check (RadixPoints 0)
                , pt_size = 64
                , pt_align = 64
                })
    , ("FILE", OpaqueType "FILE")
    ]


types =
    [ TypeName "i32"
    , VecSyntax (TypeName "size_t") 2
    , PtrSyntax (TypeName "FILE")
    , FunSyntax
        [ ArgSyntax (PtrSyntax $ TypeName "FILE") "fp" Explicit
        , ArgSyntax (TypeName "i32") "seek_bytes" Explicit
        ] (TypeName "i32")
    , FunSyntax
        [ ArgSyntax (TypeName "i32") "a" Explicit
        , ArgSyntax (TypeName "i32") "b" Explicit
        , ArgSyntax (PtrSyntax $ TypeName "i32") "carry" Implicit
        ] (TypeName "i32")
    , StructSyntax []
        [ DataMemberSyntax "a" (TypeName "i32")
        , DataMemberSyntax "b" (TypeName "i32")
        ]
    , StructSyntax []
        [ DataMemberSyntax "a" (TypeName "i32")
        , DataMemberSyntax "b" (PtrSyntax $ TypeName "FILE")
        ]
    , StructSyntax []
        [ PaddingMemberSyntax 32
        , DataMemberSyntax "a" (TypeName "i32")
        , AlignmentMemberSyntax 128
        , DynArrMemberSyntax "dynarr" (PtrSyntax $ TypeName "FILE")
        ]
    , StructSyntax []
        [ OpenDataMemberSyntax Nothing $ StructSyntax []
            [ DataMemberSyntax "x" (TypeName "i32")
            , DataMemberSyntax "y" (TypeName "i32")
            ]
        , DataMemberSyntax "z" (TypeName "i32")
        , AlignmentMemberSyntax 128
        ]
    , StructSyntax []
        [ OpenDataMemberSyntax (Just "xy") $ StructSyntax []
            [ DataMemberSyntax "x" (TypeName "i32")
            , DataMemberSyntax "y" (TypeName "i32")
            ]
        , DataMemberSyntax "z" (TypeName "i32")
        , AlignmentMemberSyntax 128
        ]
    , StructSyntax ["elem"]
        [ DataMemberSyntax "cap" (TypeName "size_t")
        , DataMemberSyntax "len" (TypeName "size_t")
        , DataMemberSyntax "data" (PtrSyntax $ TypeName "elem")
        ]
    , StructSyntax ["elem"]
        [ DataMemberSyntax "cap" (TypeName "size_t")
        , DataMemberSyntax "len" (TypeName "size_t")
        , DynArrMemberSyntax "data" (TypeName "elem")
        ]
    , StructSyntax []
        [ DataMemberSyntax "len" (TypeName "size_t")
        , DataMemberSyntax "pos" (TypeName "size_t")
        , BitfieldMemberSyntax Nothing (PtrSyntax $ TypeName "FILE")
            [ BitfieldSyntax "ptr" (PtrSyntax $ TypeName "FILE") 62 2
            , BitfieldSyntax "dirty" (TypeName "bool") 1 0
            , BitfieldSyntax "invalid" (TypeName "bool") 1 0
            ]
        ]
    ]
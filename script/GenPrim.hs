import Data.List as List

infixr 5 <+>
(<+>) :: String -> String -> String
s <+> t = s ++ ' ' : t

commaSep :: [String] -> String
commaSep = List.intercalate ", "

data T = INT | WORD | FLOAT deriving Eq

types128, types256, types512 :: [(String, String, T, Int)]
types128 =
  [("Int8X16", "Int8", INT, 16)
  ,("Int16X8", "Int16", INT, 8)
  ,("Int32X4", "Int32", INT, 4)
  ,("Int64X2", "Int64", INT, 2)
  ,("Word8X16", "Word8", WORD, 16)
  ,("Word16X8", "Word16", WORD, 8)
  ,("Word32X4", "Word32", WORD, 4)
  ,("Word64X2", "Word64", WORD, 2)
  ,("FloatX4", "Float", FLOAT, 4)
  ,("DoubleX2", "Double", FLOAT, 2)
  ]

types256 =
  [("Int8X32", "Int8", INT, 32)
  ,("Int16X16", "Int16", INT, 16)
  ,("Int32X8", "Int32", INT, 8)
  ,("Int64X4", "Int64", INT, 4)
  ,("Word8X32", "Word8", WORD, 32)
  ,("Word16X16", "Word16", WORD, 16)
  ,("Word32X8", "Word32", WORD, 8)
  ,("Word64X4", "Word64", WORD, 4)
  ,("FloatX8", "Float", FLOAT, 8)
  ,("DoubleX4", "Double", FLOAT, 4)
  ]

types512 =
  [("Int8X64", "Int8", INT, 64)
  ,("Int16X32", "Int16", INT, 32)
  ,("Int32X16", "Int32", INT, 16)
  ,("Int64X8", "Int64", INT, 8)
  ,("Word8X64", "Word8", WORD, 64)
  ,("Word16X32", "Word16", WORD, 32)
  ,("Word32X16", "Word32", WORD, 16)
  ,("Word64X8", "Word64", WORD, 8)
  ,("FloatX16", "Float", FLOAT, 16)
  ,("DoubleX8", "Double", FLOAT, 8)
  ]

mkUnary :: (String, Int) -> String -> (String -> String) -> [String]
mkUnary (typename, n) name scalarOp =
  let vecTy = typename ++ "X" ++ shows n "#"
  in [ name ++ vecTy ++ " :: " ++ vecTy ++ " -> " ++ vecTy
     , name ++ vecTy ++ " u = case unpack" ++ vecTy ++ " u of (# " ++ commaSep ["u" ++ show i | i <- [0..n-1]] ++ " #) -> pack" ++ vecTy ++ " (# " ++ commaSep [scalarOp ("u" ++ show i) | i <- [0..n-1]] ++ " #)"
     , "{-# INLINE [0] " ++ name ++ vecTy ++ " #-}"
     ]

mkBinary :: (String, Int) -> String -> (String -> String -> String) -> [String]
mkBinary (typename, n) name scalarOp =
  let vecTy = typename ++ "X" ++ shows n "#"
  in [ name ++ vecTy ++ " :: " ++ vecTy ++ " -> " ++ vecTy ++ " -> " ++ vecTy
     , name ++ vecTy ++ " u v = case unpack" ++ vecTy ++ " u of (# " ++ commaSep ["u" ++ show i | i <- [0..n-1]] ++ " #) -> case unpack" ++ vecTy ++ " v of (# " ++ commaSep ["v" ++ show i | i <- [0..n-1]] ++ " #) -> pack" ++ vecTy ++ " (# " ++ commaSep [scalarOp ("u" ++ show i) ("v" ++ show i) | i <- [0..n-1]] ++ " #)"
     , "{-# INLINE [0] " ++ name ++ vecTy ++ " #-}"
     ]

mkForeignUnary :: (String, Int) -> String -> [String]
mkForeignUnary (typename, n) name =
  let vecTySymbol = typename ++ "X" ++ show n
      vecTy = typename ++ "X" ++ shows n "#"
  in [ "foreign import ccall unsafe \"hs_simdy_" ++ name ++ vecTySymbol ++ "\""
     , "  " ++ name ++ vecTy ++ " :: " ++ vecTy ++ " -> " ++ vecTy
     ]

mkForeignBinary :: (String, Int) -> String -> [String]
mkForeignBinary (typename, n) name =
  let vecTySymbol = typename ++ "X" ++ show n
      vecTy = typename ++ "X" ++ shows n "#"
  in [ "foreign import ccall unsafe \"hs_simdy_" ++ name ++ vecTySymbol ++ "\""
     , "  " ++ name ++ vecTy ++ " :: " ++ vecTy ++ " -> " ++ vecTy ++ " -> " ++ vecTy
     ]

content :: String -> Int -> [(String, String, T, Int)] -> [String] -> [String]
content moduleName width types reexports =
  let int8 = ("Int8", width `quot` 8)
      int16 = ("Int16", width `quot` 16)
      int32 = ("Int32", width `quot` 32)
      int64 = ("Int64", width `quot` 64)
      word8 = ("Word8", width `quot` 8)
      word16 = ("Word16", width `quot` 16)
      word32 = ("Word32", width `quot` 32)
      word64 = ("Word64", width `quot` 64)
      float = ("Float", width `quot` 32)
      double = ("Double", width `quot` 64)
      exports = concat
        [[t ++ "#" | (t, _, _, _) <- types]
        ,["broadcast" ++ t ++ "#" | (t, _, _, _) <- types]
        ,["pack" ++ t ++ "#" | (t, _, _, _) <- types]
        ,["unpack" ++ t ++ "#" | (t, _, _, _) <- types]
        ,["insert" ++ t ++ "#" | (t, _, _, _) <- types]
        ,["plus" ++ t ++ "#" | (t, _, _, _) <- types]
        ,["minus" ++ t ++ "#" | (t, _, _, _) <- types]
        ,["times" ++ t ++ "#" | (t, _, _, _) <- types]
        ,["divide" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        ,["quot" ++ t ++ "#" | (t, _, k, _) <- types, k == INT || k == WORD]
        ,["rem" ++ t ++ "#" | (t, _, k, _) <- types, k == INT || k == WORD]
        ,["negate" ++ t ++ "#" | (t, _, k, _) <- types, k == INT || k == FLOAT]
        ,["index" ++ t ++ "Array#" | (t, _, _, _) <- types]
        ,["read" ++ t ++ "Array#" | (t, _, _, _) <- types]
        ,["write" ++ t ++ "Array#" | (t, _, _, _) <- types]
        ,["index" ++ t ++ "OffAddr#" | (t, _, _, _) <- types]
        ,["read" ++ t ++ "OffAddr#" | (t, _, _, _) <- types]
        ,["write" ++ t ++ "OffAddr#" | (t, _, _, _) <- types]
        ,["index" ++ s ++ "ArrayAs" ++ t ++ "#" | (t, s, _, _) <- types]
        ,["read" ++ s ++ "ArrayAs" ++ t ++ "#" | (t, s, _, _) <- types]
        ,["write" ++ s ++ "ArrayAs" ++ t ++ "#" | (t, s, _, _) <- types]
        ,["index" ++ s ++ "OffAddrAs" ++ t ++ "#" | (t, s, _, _) <- types]
        ,["read" ++ s ++ "OffAddrAs" ++ t ++ "#" | (t, s, _, _) <- types]
        ,["write" ++ s ++ "OffAddrAs" ++ t ++ "#" | (t, s, _, _) <- types]
        -- GHC 9.12 or later
        ,["#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)"]
        ,["fmadd" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        ,["fmsub" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        ,["fnmadd" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        ,["fnmsub" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        ,["shuffle" ++ t ++ "#" | (t, _, _, _) <- types]
        ,["min" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        ,["max" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        ,["#endif"]
        ,["min" ++ t ++ "#" | (t, _, k, _) <- types, k /= FLOAT]
        ,["max" ++ t ++ "#" | (t, _, k, _) <- types, k /= FLOAT]
        -- GHC 9.16 or later
        ,["and" ++ t ++ "#" | (t, _, k, _) <- types, k /= FLOAT]
        ,["or" ++ t ++ "#" | (t, _, k, _) <- types, k /= FLOAT]
        ,["xor" ++ t ++ "#" | (t, _, k, _) <- types, k /= FLOAT]
        ,["#if MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)"]
        ,["and" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        ,["or" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        ,["xor" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        ,["#endif"]
        ,["abs" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT || k == INT]
        ,["sqrt" ++ t ++ "#" | (t, _, k, _) <- types, k == FLOAT]
        -- Future
        ,["complement" ++ t ++ "#" | (t, _, k, _) <- types, k /= FLOAT]
        ] ++ ["module " ++ mod | mod <- reexports]
  in
    [ "-- This file was created by script/GenPrim.hs. Do not edit by hand!"
    , "{-# LANGUAGE CPP #-}"
    , "{-# LANGUAGE MagicHash #-}"
    , "{-# LANGUAGE UnboxedTuples #-}"
    , "{-# LANGUAGE UnliftedFFITypes #-}"
    , "module " ++ moduleName
    ]
    ++ zipWith (\i ident -> case ident of '#':_ -> ident; _ -> if i == 0 then "  ( " ++ ident else "  , " ++ ident) [0..] exports
    ++ ["  ) where"]
    ++ ["import           " ++ mod | mod <- reexports]
    ++ [ ""
       , "#if defined(BROADCAST_IS_BROKEN)"
       , "-- The LLVM backend of GHC 9.12.{1,2} has a bug with broadcast: https://gitlab.haskell.org/ghc/ghc/-/issues/25561"
       , "import           GHC.PrimOps hiding (" ++ commaSep ["broadcast" ++ ty ++ "#" | (ty, _, _, _) <- types] ++ ")"
       , ""
       ]
    ++ concat [ [ "broadcast" ++ vecTy ++ "# :: " ++ elemTy ++ "# -> " ++ vecTy ++ "#"
                , "broadcast" ++ vecTy ++ "# x = pack" ++ vecTy ++ "# (# " ++ commaSep (replicate n "x") ++ " #)"
                , "{-# INLINE [0] broadcast" ++ vecTy ++ "# #-}"
                , ""
                ]
              | (vecTy, elemTy, _, n) <- types
              ]
    ++ [ "#elif MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)"
       , ""
       , "-- from ghc-experimental"
       , "import           GHC.PrimOps"
       , ""
       , "#else"
       , ""
       , "import           GHC.Exts"
       , ""
       , "#endif"
       , ""
       ]
    ++ concat (intersperse [""] $
      [["#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)"] | width == 128] ++
      [ mkUnary int8 "complement" (\u -> "intToInt8# (notI# (int8ToInt# " ++ u ++ "))")
      , mkUnary int16 "complement" (\u -> "intToInt16# (notI# (int16ToInt# " ++ u ++ "))")
      , mkUnary int32 "complement" (\u -> "intToInt32# (notI# (int32ToInt# " ++ u ++ "))")
      , mkUnary int64 "complement" (\u -> "word64ToInt64# (not64# (int64ToWord64# " ++ u ++ "))")
      , mkUnary word8 "complement" (\u -> "wordToWord8# (not# (word8ToWord# " ++ u ++ "))")
      , mkUnary word16 "complement" (\u -> "wordToWord16# (not# (word16ToWord# " ++ u ++ "))")
      , mkUnary word32 "complement" (\u -> "wordToWord32# (not# (word32ToWord# " ++ u ++ "))")
      , mkUnary word64 "complement" (\u -> "not64# " ++ u)
      , ["#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)"]
      , mkBinary int8 "and" (\u v -> "intToInt8# (int8ToInt# " ++ u ++ " `andI#` int8ToInt# " ++ v ++ ")")
      , mkBinary int16 "and" (\u v -> "intToInt16# (int16ToInt# " ++ u ++ " `andI#` int16ToInt# " ++ v ++ ")")
      , mkBinary int32 "and" (\u v -> "intToInt32# (int32ToInt# " ++ u ++ " `andI#` int32ToInt# " ++ v ++ ")")
      , mkBinary int64 "and" (\u v -> "word64ToInt64# (int64ToWord64# " ++ u ++ " `and64#` int64ToWord64# " ++ v ++ ")")
      , mkBinary word8 "and" (\u v -> "wordToWord8# (word8ToWord# " ++ u ++ " `and#` word8ToWord# " ++ v ++ ")")
      , mkBinary word16 "and" (\u v -> "wordToWord16# (word16ToWord# " ++ u ++ " `and#` word16ToWord# " ++ v ++ ")")
      , mkBinary word32 "and" (\u v -> "wordToWord32# (word32ToWord# " ++ u ++ " `and#` word32ToWord# " ++ v ++ ")")
      , mkBinary word64 "and" (\u v -> "and64# " ++ u <+> v)
      , mkBinary int8 "or" (\u v -> "intToInt8# (int8ToInt# " ++ u ++ " `orI#` int8ToInt# " ++ v ++ ")")
      , mkBinary int16 "or" (\u v -> "intToInt16# (int16ToInt# " ++ u ++ " `orI#` int16ToInt# " ++ v ++ ")")
      , mkBinary int32 "or" (\u v -> "intToInt32# (int32ToInt# " ++ u ++ " `orI#` int32ToInt# " ++ v ++ ")")
      , mkBinary int64 "or" (\u v -> "word64ToInt64# (int64ToWord64# " ++ u ++ " `or64#` int64ToWord64# " ++ v ++ ")")
      , mkBinary word8 "or" (\u v -> "wordToWord8# (word8ToWord# " ++ u ++ " `or#` word8ToWord# " ++ v ++ ")")
      , mkBinary word16 "or" (\u v -> "wordToWord16# (word16ToWord# " ++ u ++ " `or#` word16ToWord# " ++ v ++ ")")
      , mkBinary word32 "or" (\u v -> "wordToWord32# (word32ToWord# " ++ u ++ " `or#` word32ToWord# " ++ v ++ ")")
      , mkBinary word64 "or" (\u v -> "or64# " ++ u <+> v)
      , mkBinary int8 "xor" (\u v -> "intToInt8# (int8ToInt# " ++ u ++ " `xorI#` int8ToInt# " ++ v ++ ")")
      , mkBinary int16 "xor" (\u v -> "intToInt16# (int16ToInt# " ++ u ++ " `xorI#` int16ToInt# " ++ v ++ ")")
      , mkBinary int32 "xor" (\u v -> "intToInt32# (int32ToInt# " ++ u ++ " `xorI#` int32ToInt# " ++ v ++ ")")
      , mkBinary int64 "xor" (\u v -> "word64ToInt64# (int64ToWord64# " ++ u ++ " `xor64#` int64ToWord64# " ++ v ++ ")")
      , mkBinary word8 "xor" (\u v -> "wordToWord8# (word8ToWord# " ++ u ++ " `xor#` word8ToWord# " ++ v ++ ")")
      , mkBinary word16 "xor" (\u v -> "wordToWord16# (word16ToWord# " ++ u ++ " `xor#` word16ToWord# " ++ v ++ ")")
      , mkBinary word32 "xor" (\u v -> "wordToWord32# (word32ToWord# " ++ u ++ " `xor#` word32ToWord# " ++ v ++ ")")
      , mkBinary word64 "xor" (\u v -> "xor64# " ++ u <+> v)
      , ["#endif"]
      , ["#if !MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)"]
      , mkBinary int8 "min" (\u v -> "case ltInt8# " ++ u <+> v ++ " of { 0# -> " ++ v ++ "; _ -> " ++ u ++ " }")
      , mkBinary int16 "min" (\u v -> "case ltInt16# " ++ u <+> v ++ " of { 0# -> " ++ v ++ "; _ -> " ++ u ++ " }")
      , mkBinary int32 "min" (\u v -> "case ltInt32# " ++ u <+> v ++ " of { 0# -> " ++ v ++ "; _ -> " ++ u ++ " }")
      , mkBinary int64 "min" (\u v -> "case ltInt64# " ++ u <+> v ++ " of { 0# -> " ++ v ++ "; _ -> " ++ u ++ " }")
      , mkBinary word8 "min" (\u v -> "case ltWord8# " ++ u <+> v ++ " of { 0# -> " ++ v ++ "; _ -> " ++ u ++ " }")
      , mkBinary word16 "min" (\u v -> "case ltWord16# " ++ u <+> v ++ " of { 0# -> " ++ v ++ "; _ -> " ++ u ++ " }")
      , mkBinary word32 "min" (\u v -> "case ltWord32# " ++ u <+> v ++ " of { 0# -> " ++ v ++ "; _ -> " ++ u ++ " }")
      , mkBinary word64 "min" (\u v -> "case ltWord64# " ++ u <+> v ++ " of { 0# -> " ++ v ++ "; _ -> " ++ u ++ " }")
      , mkBinary int8 "max" (\u v -> "case ltInt8# " ++ u <+> v ++ " of { 0# -> " ++ u ++ "; _ -> " ++ v ++ " }")
      , mkBinary int16 "max" (\u v -> "case ltInt16# " ++ u <+> v ++ " of { 0# -> " ++ u ++ "; _ -> " ++ v ++ " }")
      , mkBinary int32 "max" (\u v -> "case ltInt32# " ++ u <+> v ++ " of { 0# -> " ++ u ++ "; _ -> " ++ v ++ " }")
      , mkBinary int64 "max" (\u v -> "case ltInt64# " ++ u <+> v ++ " of { 0# -> " ++ u ++ "; _ -> " ++ v ++ " }")
      , mkBinary word8 "max" (\u v -> "case ltWord8# " ++ u <+> v ++ " of { 0# -> " ++ u ++ "; _ -> " ++ v ++ " }")
      , mkBinary word16 "max" (\u v -> "case ltWord16# " ++ u <+> v ++ " of { 0# -> " ++ u ++ "; _ -> " ++ v ++ " }")
      , mkBinary word32 "max" (\u v -> "case ltWord32# " ++ u <+> v ++ " of { 0# -> " ++ u ++ "; _ -> " ++ v ++ " }")
      , mkBinary word64 "max" (\u v -> "case ltWord64# " ++ u <+> v ++ " of { 0# -> " ++ u ++ "; _ -> " ++ v ++ " }")
      , ["#endif"]
      -- ExtendLiterals is not available on GHC 9.6
      , ["#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)"]
      , mkUnary int8 "abs" (\u -> "case ltInt8# " ++ u ++ " (intToInt8# 0#) of { 0# -> " ++ u ++ "; _ -> negateInt8# " ++ u ++ " }")
      , mkUnary int16 "abs" (\u -> "case ltInt16# " ++ u ++ " (intToInt16# 0#) of { 0# -> " ++ u ++ "; _ -> negateInt16# " ++ u ++ " }")
      , mkUnary int32 "abs" (\u -> "case ltInt32# " ++ u ++ " (intToInt32# 0#) of { 0# -> " ++ u ++ "; _ -> negateInt32# " ++ u ++ " }")
      , mkUnary int64 "abs" (\u -> "case ltInt64# " ++ u ++ " (intToInt64# 0#) of { 0# -> " ++ u ++ "; _ -> negateInt64# " ++ u ++ " }")
      , ["#endif"]
      ] ++
      (if width == 128
       then
        [ ["#else"] -- GHC >= 9.14 || LLVM
        , mkForeignUnary int8 "complement"
        , mkForeignUnary int16 "complement"
        , mkForeignUnary int32 "complement"
        , mkForeignUnary int64 "complement"
        , mkForeignUnary word8 "complement"
        , mkForeignUnary word16 "complement"
        , mkForeignUnary word32 "complement"
        , mkForeignUnary word64 "complement"
        , ["#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)"]
        , mkForeignBinary int8 "and"
        , mkForeignBinary int16 "and"
        , mkForeignBinary int32 "and"
        , mkForeignBinary int64 "and"
        , mkForeignBinary word8 "and"
        , mkForeignBinary word16 "and"
        , mkForeignBinary word32 "and"
        , mkForeignBinary word64 "and"
        , mkForeignBinary int8 "or"
        , mkForeignBinary int16 "or"
        , mkForeignBinary int32 "or"
        , mkForeignBinary int64 "or"
        , mkForeignBinary word8 "or"
        , mkForeignBinary word16 "or"
        , mkForeignBinary word32 "or"
        , mkForeignBinary word64 "or"
        , mkForeignBinary int8 "xor"
        , mkForeignBinary int16 "xor"
        , mkForeignBinary int32 "xor"
        , mkForeignBinary int64 "xor"
        , mkForeignBinary word8 "xor"
        , mkForeignBinary word16 "xor"
        , mkForeignBinary word32 "xor"
        , mkForeignBinary word64 "xor"
        -- We are using GHC 9.12, so we have min/max primops
        , mkForeignUnary int8 "abs"
        , mkForeignUnary int16 "abs"
        , mkForeignUnary int32 "abs"
        , mkForeignUnary int64 "abs"
        , ["#endif"
          ,"#endif"] -- GHC >= 9.14 || LLVM
        ]
       else []) ++
      [ ["#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)"]
      , mkUnary float "abs" (\u -> "fabsFloat# " ++ u)
      , mkUnary double "abs" (\u -> "fabsDouble# " ++ u)
      , mkUnary float "sqrt" (\u -> "sqrtFloat# " ++ u)
      , mkUnary double "sqrt" (\u -> "sqrtDouble# " ++ u)
      , ["#endif"]
      ])

main :: IO ()
main = do
  writeFile "src-vl128/Data/Simdy/Internal/SIMD128/Prim.hs" $ unlines $
    content "Data.Simdy.Internal.SIMD128.Prim" 128 types128 []
  writeFile "src-vl256/Data/Simdy/Internal/SIMD256/Prim.hs" $ unlines $
    content "Data.Simdy.Internal.SIMD256.Prim" 256 types256 ["Data.Simdy.Internal.SIMD128.Prim"]
  writeFile "src-vl512/Data/Simdy/Internal/SIMD512/Prim.hs" $ unlines $
    content "Data.Simdy.Internal.SIMD512.Prim" 512 types512 ["Data.Simdy.Internal.SIMD256.Prim"]

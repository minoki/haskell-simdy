-- Run as 'cabal run script/Gen.hs'
{- cabal:
build-depends: base, directory
default-language: GHC2021
-}
import System.IO
import qualified Data.List as List
import Control.Monad
import System.Directory (createDirectoryIfMissing)
import Numeric

maxTupleLen :: Int
maxTupleLen = 6

maxTupleLenForUnboxedVector :: Int
maxTupleLenForUnboxedVector = 6

infixr 5 <+>
(<+>) :: String -> String -> String
s <+> t = s ++ ' ' : t

parens :: String -> String
parens s = '(' : s ++ ")"

spaceSep :: [String] -> String
spaceSep = List.intercalate " "

commaSep :: [String] -> String
commaSep = List.intercalate ", "

semicolonSep :: [String] -> String
semicolonSep = List.intercalate "; "

gen :: Int -> Int -> [String]
gen !vecCount !maxBits
  = ["-- | @'" ++ tyCon ++ "' a@ is a fixed-length vector of length " ++ show vecCount ++ "."
    ,"--"
    ,"-- Conceptually, @data '" ++ tyCon ++ "' a = Mk" ++ tyCon ++ concat (replicate vecCount " !a") ++ "@."
    ,"--"
    ,"-- You can access the elements by 'mk" ++ tyCon ++ "', 'pack" ++ tyCon ++ "' and 'unpack" ++ tyCon ++ "'."
    ,"data family " ++ tyCon ++ " a"
    ,"instance KnownSIMDLength " ++ tyCon ++ " where"
    ,"  type SIMDLength " ++ tyCon ++ " = " ++ show vecCount
    ,"  simdLength = " ++ show vecCount
    ,"  {-# INLINE simdLength #-}"
    ,"newtype instance " ++ tyCon ++ " Bool = MkBool" ++ tyCon ++ " Word" ++ show (max vecCount 8)
    ,"type instance Mask (" ++ tyCon ++ " a) = " ++ tyCon ++ " Bool"
    ,"instance MaskIsLiftedBool " ++ tyCon ++ " a"
    ,"instance BooleanF " ++ tyCon ++ " Bool where"
    ,"  andF (MkBool" ++ tyCon ++ " x) (MkBool" ++ tyCon ++ " y) = MkBool" ++ tyCon ++ " (x .&. y)"
    ,"  orF (MkBool" ++ tyCon ++ " x) (MkBool" ++ tyCon ++ " y) = MkBool" ++ tyCon ++ " (x .|. y)"
    ,"  xorF (MkBool" ++ tyCon ++ " x) (MkBool" ++ tyCon ++ " y) = MkBool" ++ tyCon ++ " (xor x y)"
    ,"  complementF (MkBool" ++ tyCon ++ " x) = MkBool" ++ tyCon ++ " (0x" ++ showHex (2^vecCount - 1) " - x)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance EquatableF " ++ tyCon ++ " a => Equatable (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance OrderedF " ++ tyCon ++ " a => Ordered (" ++ tyCon ++ " a)"
    ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " a => IsList (" ++ tyCon ++ " a) where"
    ,"  type Item (" ++ tyCon ++ " a) = a"
    ,"  toList = toList" ++ tyCon
    ,"  fromList = fromList" ++ tyCon
    ,"  {-# INLINE toList #-}"
    ,"  {-# INLINE fromList #-}"
    ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " Bool where"
    ,"  mk" ++ tyCon ++ concat [" !x" ++ show i | i <- [0..vecCount-1]] ++ " = MkBool" ++ tyCon ++ " (" ++ List.intercalate " .|. " ["(if x" ++ show i ++ " then 0x" ++ showHex (2^i) " else 0)" | i <- [0..vecCount-1]] ++ ")"
    ,"  unpack" ++ tyCon ++ " (MkBool" ++ tyCon ++ " !x) = (" ++ List.intercalate ", " ["testBit x " ++ show i | i <- [0..vecCount-1]] ++ ")"
    ,"instance Broadcast " ++ tyCon ++ " Bool where"
    ,"  broadcast False = MkBool" ++ tyCon ++ " 0"
    ,"  broadcast True = MkBool" ++ tyCon ++ " 0x" ++ showHex (2^vecCount-1) ""
    ,"  {-# INLINE broadcast #-}"
    ,"instance SelectableF " ++ tyCon ++ " Bool where"
    ,"  selectF (MkBool" ++ tyCon ++ " !cond) (MkBool" ++ tyCon ++ " !x) (MkBool" ++ tyCon ++ " !y) = MkBool" ++ tyCon ++ " ((cond .&. x) .|. (complement cond .&. y))"
    ,"  {-# INLINE selectF #-}"
    ]
    ++ genType "Float" "F#" 32 "0.0#" maxBits [genEquatable, genOrderedFloat, genNum True True, genFractional, genFloating, genFMA, genEnumFromZero ".0#", genPrim, genStorable]
    ++ genType "Double" "D#" 64 "0.0##" maxBits [genEquatable, genOrderedFloat, genNum True True, genFractional, genFloating, genFMA, genEnumFromZero ".0##", genPrim, genStorable]
    ++ ["#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)" | maxBits == 128]
    ++ ["instance ImplementationDescription " ++ tyCon ++ " where"
       ,"  implementationDescription _ = \"" ++ tyCon ++ ";maxBits=" ++ show maxBits ++ "\""
       ]
    ++ genType "Int8" "I8#" 8 "intToInt8# 0#" maxBits [genEquatable, genOrderedInt, genNum True False, genBits, genEnumFromZero "#Int8", genPrim, genStorable]
    ++ genType "Int16" "I16#" 16 "intToInt16# 0#" maxBits [genEquatable, genOrderedInt, genNum True False, genBits, genEnumFromZero "#Int16", genPrim, genStorable]
    ++ genType "Int32" "I32#" 32 "intToInt32# 0#" maxBits [genEquatable, genOrderedInt, genNum True False, genBits, genEnumFromZero "#Int32", genPrim, genStorable]
    ++ genType "Int64" "I64#" 64 "intToInt64# 0#" maxBits [genEquatable, genOrderedInt, genNum True False, genBits, genEnumFromZero "#Int64", genPrim, genStorable]
    ++ genType "Word8" "W8#" 8 "wordToWord8# 0##" maxBits [genEquatable, genOrderedInt, genNum False False, genBits, genEnumFromZero "#Word8", genPrim, genStorable]
    ++ genType "Word16" "W16#" 16 "wordToWord16# 0##" maxBits [genEquatable, genOrderedInt, genNum False False, genBits, genEnumFromZero "#Word16", genPrim, genStorable]
    ++ genType "Word32" "W32#" 32 "wordToWord32# 0##" maxBits [genEquatable, genOrderedInt, genNum False False, genBits, genEnumFromZero "#Word32", genPrim, genStorable]
    ++ genType "Word64" "W64#" 64 "wordToWord64# 0##" maxBits [genEquatable, genOrderedInt, genNum False False, genBits, genEnumFromZero "#Word64", genPrim, genStorable]
    ++ (if maxBits == 128
        then ["#else"
             ,"-- The NCG of GHC 9.12 does not support integer vectors"
             ,"instance ImplementationDescription " ++ tyCon ++ " where"
             ,"  implementationDescription _ = \"" ++ tyCon ++ ";maxBits(Float,Double)=" ++ show maxBits ++ ",maxBits(other)=0\""
             ]
             ++ genType "Int8" "I8#" 8 "intToInt8# 0#" 0 [genEquatable, genOrderedInt, genNum True False, genBits, genEnumFromZero "#Int8", genPrim, genStorable]
             ++ genType "Int16" "I16#" 16 "intToInt16# 0#" 0 [genEquatable, genOrderedInt, genNum True False, genBits, genEnumFromZero "#Int16", genPrim, genStorable]
             ++ genType "Int32" "I32#" 32 "intToInt32# 0#" 0 [genEquatable, genOrderedInt, genNum True False, genBits, genEnumFromZero "#Int32", genPrim, genStorable]
             ++ genType "Int64" "I64#" 64 "intToInt64# 0#" 0 [genEquatable, genOrderedInt, genNum True False, genBits, genEnumFromZero "#Int64", genPrim, genStorable]
             ++ genType "Word8" "W8#" 8 "wordToWord8# 0##" 0 [genEquatable, genOrderedInt, genNum False False, genBits, genEnumFromZero "#Word8", genPrim, genStorable]
             ++ genType "Word16" "W16#" 16 "wordToWord16# 0##" 0 [genEquatable, genOrderedInt, genNum False False, genBits, genEnumFromZero "#Word16", genPrim, genStorable]
             ++ genType "Word32" "W32#" 32 "wordToWord32# 0##" 0 [genEquatable, genOrderedInt, genNum False False, genBits, genEnumFromZero "#Word32", genPrim, genStorable]
             ++ genType "Word64" "W64#" 64 "wordToWord64# 0##" 0 [genEquatable, genOrderedInt, genNum False False, genBits, genEnumFromZero "#Word64", genPrim, genStorable]
             ++ ["#endif"]
        else []
       )
    ++ genNewtype "Sum"
    ++ genNewtype "Product"
    ++ genNewtype "Min"
    ++ genNewtype "Max"
    ++ ["data instance " ++ tyCon ++ " (Complex a) = MkComplex" ++ tyCon ++ " !(" ++ tyCon ++ " a) !(" ++ tyCon ++ " a)"
       ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " a => Pack" ++ tyCon ++ " " ++ tyCon ++ " (Complex a) where"
       ,"  mk" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " :+ y" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " = MkComplex" ++ tyCon ++ " (mk" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (mk" ++ tyCon ++ " " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ")"
       ,"  unpack" ++ tyCon ++ " (MkComplex" ++ tyCon ++ " s t) = case unpack" ++ tyCon ++ " s of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> case unpack" ++ tyCon ++ " t of (" ++ commaSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") -> (" ++ commaSep ["x" ++ show i ++ " :+ y" ++ show i | i <- [0..vecCount-1]] ++ ")"
       ,"  {-# INLINE mk" ++ tyCon ++ " #-}"
       ,"  {-# INLINE unpack" ++ tyCon ++ " #-}"
       ,"instance Broadcast " ++ tyCon ++ " a => Broadcast " ++ tyCon ++ " (Complex a) where"
       ,"  broadcast (x :+ y) = MkComplex" ++ tyCon ++ " (broadcast x) (broadcast y)"
       ,"  {-# INLINE broadcast #-}"
       ,"instance SelectableF " ++ tyCon ++ " a => SelectableF " ++ tyCon ++ " (Complex a) where"
       ,"  selectF !cond (MkComplex" ++ tyCon ++ " x y) (MkComplex" ++ tyCon ++ " x' y') = MkComplex" ++ tyCon ++ " (selectF cond x x') (selectF cond y y')"
       ,"  {-# INLINE selectF #-}"
       ]
    {-
    ++ ["instance UnboxSIMD " ++ tyCon ++ " a => UnboxSIMD " ++ tyCon ++ " (Complex a) where"
       ,"  unsafeIndexUnboxedSIMD (VUB.V_Complex (VUB.V_2 _ u v)) !i = MkComplex" ++ tyCon ++ " (unsafeIndexUnboxedSIMD u i) (unsafeIndexUnboxedSIMD v i)"
       ,"  unsafeReadUnboxedSIMD (VUB.MV_Complex (VUB.MV_2 _ u v)) !i = do { x <- unsafeReadUnboxedSIMD u i; y <- unsafeReadUnboxedSIMD v i; pure (MkComplex" ++ tyCon ++ " x y) }"
       ,"  unsafeWriteUnboxedSIMD (VUB.MV_Complex (VUB.MV_2 _ u v)) !i (MkComplex" ++ tyCon ++ " x y) = do { unsafeWriteUnboxedSIMD u i x; unsafeWriteUnboxedSIMD v i y }"
       ,"  {-# INLINE unsafeIndexUnboxedSIMD #-}"
       ,"  {-# INLINE unsafeReadUnboxedSIMD #-}"
       ,"  {-# INLINE unsafeWriteUnboxedSIMD #-}"
       ]
    -}
    ++ ["data instance " ++ tyCon ++ " () = MkUnit" ++ tyCon
       ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " () where"
       ,"  mk" ++ tyCon ++ " " ++ spaceSep (replicate vecCount "_") ++ " = MkUnit" ++ tyCon
       ,"  unpack" ++ tyCon ++ " MkUnit" ++ tyCon ++ " = (" ++ commaSep (replicate vecCount "()") ++ ")"
       ,"  {-# INLINE mk" ++ tyCon ++ " #-}"
       ,"  {-# INLINE unpack" ++ tyCon ++ " #-}"
       ,"instance Broadcast " ++ tyCon ++ " () where"
       ,"  broadcast _ = MkUnit" ++ tyCon
       ,"  {-# INLINE broadcast #-}"
       ,"instance SelectableF " ++ tyCon ++ " () where"
       ,"  selectF _ _ _ = MkUnit" ++ tyCon
       ,"  {-# INLINE selectF #-}"
       ]
    ++ concatMap genTuple [2..maxTupleLen]
    ++ ["instance (Pack" ++ tyCon ++ " " ++ tyCon ++ " a, Pack" ++ tyCon ++ " " ++ tyCon ++ " b) => LiftSIMD " ++ tyCon ++ " a b where"
       ,"  liftSIMD f !v = case unpack" ++ tyCon ++ " v of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> mk" ++ tyCon ++ " " ++ spaceSep ["(f x" ++ show i ++ ")" | i <- [0..vecCount-1]]
       ,"  {-# INLINE liftSIMD #-}"
       ,"instance (Pack" ++ tyCon ++ " " ++ tyCon ++ " a, Pack" ++ tyCon ++ " " ++ tyCon ++ " b, Pack" ++ tyCon ++ " " ++ tyCon ++ " c) => LiftSIMD2 " ++ tyCon ++ " a b c where"
       ,"  liftSIMD2 f !u !v = case unpack" ++ tyCon ++ " u of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> case unpack" ++ tyCon ++ " v of (" ++ commaSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") -> mk" ++ tyCon ++ " " ++ spaceSep ["(f x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
       ,"  {-# INLINE liftSIMD2 #-}"
       ]
    ++ ["instance LiftConstructor " ++ tyCon ++ " where"]
    ++ ["  mkTuple" ++ show i ++ " = MkTuple" ++ show i ++ tyCon | i <- [2..maxTupleLen]]
    ++ ["  deconstructTuple" ++ show i ++ " (MkTuple" ++ show i ++ tyCon ++ " " ++ spaceSep ["v" ++ show j | j <- [0..i-1]] ++ ") = (" ++ commaSep ["v" ++ show j | j <- [0..i-1]] ++ ")" | i <- [2..maxTupleLen]]
    ++ ["  " ++ name ++ " = coerce" | name <- ["mkSum", "getSum'", "mkProduct", "getProduct'", "mkMin", "getMin'", "mkMax", "getMax'" {- , "mkAll", "getAll'", "mkAny", "getAny'" -}]]
    ++ ["  mkComplex = MkComplex" ++ tyCon]
    ++ ["  deconstructComplex (MkComplex" ++ tyCon ++ " x y) = (x, y)"]
    ++ ["  {-# INLINE mkTuple" ++ show i ++ " #-}" | i <- [2..maxTupleLen]]
    ++ ["  {-# INLINE deconstructTuple" ++ show i ++ " #-}" | i <- [2..maxTupleLen]]
    ++ ["  {-# INLINE " ++ name ++ " #-}" | name <- ["mkSum", "getSum'", "mkProduct", "getProduct'", "mkMin", "getMin'", "mkMax", "getMax'" {- , "mkAll", "getAll'", "mkAny", "getAny'" -}]]
    ++ ["  {-# INLINE mkComplex #-}"]
    ++ ["  {-# INLINE deconstructComplex #-}"]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance SelectableF " ++ tyCon ++ " a => Selectable (" ++ tyCon ++ " a)"]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance NumF " ++ tyCon ++ " a => Num (" ++ tyCon ++ " a)"]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance FractionalF " ++ tyCon ++ " a => Fractional (" ++ tyCon ++ " a)"]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance FloatingF " ++ tyCon ++ " a => Floating (" ++ tyCon ++ " a)"]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance BooleanF " ++ tyCon ++ " a => Boolean (" ++ tyCon ++ " a)"]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance BitShiftF " ++ tyCon ++ " a => BitShift (" ++ tyCon ++ " a)"]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance MinMaxF " ++ tyCon ++ " a => MinMax (" ++ tyCon ++ " a)"]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance FusedMultiplyAddF " ++ tyCon ++ " a => FusedMultiplyAdd (" ++ tyCon ++ " a)"]
  where
    tyCon = 'X' : show vecCount
    genType name primCon !bitsPerElem zero maxBits others
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
            halfTyCon = if vecCount == 2 then "Identity" else "X" ++ show (vecCount `quot` 2)
            mainDef = if maxBits == 0
                      then ["data instance " ++ tyCon ++ " " ++ name ++ " = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep (replicate vecCount ('!':name))
                           ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  mk" ++ tyCon ++ " = Mk" ++ name ++ tyCon ++ "WithElems"
                           ,"  unpack" ++ tyCon ++ " (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ")"
                           ,"  {-# INLINE mk" ++ tyCon ++ " #-}"
                           ,"  {-# INLINE unpack" ++ tyCon ++ " #-}"
                           ,"instance Broadcast " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  broadcast !x = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep (replicate vecCount "x")
                           ,"  {-# INLINE broadcast #-}"
                           ,"instance SelectableF " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  selectF (MkBool" ++ tyCon ++ " !cond) (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(if testBit cond " ++ show i ++ " then x" ++ show i ++ " else y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                           ,"instance (" ++ commaSep ["i" ++ show i ++ " < " ++ show vecCount | i <- [0..vecCount-1]] ++  ", " ++ commaSep ["Pick " ++ name ++ " i" ++ show i | i <- [0..vecCount-1]] ++ ") => UnaryShuffle [" ++ commaSep ["i" ++ show i | i <- [0..vecCount-1]] ++ "] " ++ tyCon <+> name ++ " where"
                           ,"  unaryShuffle (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = let { sources = \\case { " ++ semicolonSep [(if i == vecCount - 1 then "_" else show i) ++ " -> x" ++ show i | i <- [0..vecCount-1]] ++ " } } in Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(pick @_ @i" ++ show i ++ " sources)" | i <- [0..vecCount-1]]
                           ,"  {-# INLINE unaryShuffle #-}"
                           ,"instance (" ++ commaSep ["i" ++ show i ++ " < " ++ show (2 * vecCount) | i <- [0..vecCount-1]] ++  ", " ++ commaSep ["Pick " ++ name ++ " i" ++ show i | i <- [0..vecCount-1]] ++ ") => BinaryShuffle [" ++ commaSep ["i" ++ show i | i <- [0..vecCount-1]] ++ "] " ++ tyCon <+> name ++ " where"
                           ,"  binaryShuffle (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [vecCount..2*vecCount-1]] ++ ") = let { sources = \\case { " ++ semicolonSep [(if i == 2 * vecCount - 1 then "_" else show i) ++ " -> x" ++ show i | i <- [0..2*vecCount-1]] ++ " } } in Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(pick @_ @i" ++ show i ++ " sources)" | i <- [0..vecCount-1]]
                           ,"  {-# INLINE binaryShuffle #-}"
                           ]
                      else
                        let shortVecSize = vecBitCount `div` bitsPerElem
                            shortVecCount = max bitCount 128 `div` vecBitCount
                            shortVecName = name ++ "X" ++ show shortVecSize
                            suffix | shortVecCount == 1 = ""
                                   | otherwise = "WithVec" ++ show vecBitCount
                        in ["data instance " ++ tyCon ++ " " ++ name ++ " = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep (replicate shortVecCount (shortVecName ++ "#"))
                           ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  mk" ++ tyCon <+> spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " = Mk" ++ name ++ tyCon ++ suffix <+> spaceSep ["(pack" ++ shortVecName ++ "# (# " ++ commaSep [if k < vecCount then "x" ++ show k else zero | j <- [0..shortVecSize - 1], let k = i * shortVecSize + j] ++ " #))" | i <- [0..shortVecCount - 1]]
                           ,"  unpack" ++ tyCon ++ " (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = " ++ concat ["case unpack" ++ shortVecName ++ "# v" ++ show i ++ " of (# " ++ commaSep [if k < vecCount then "x" ++ show k else "_" | j <- [0..shortVecSize - 1], let k = i * shortVecSize + j] ++ " #) -> " | i <- [0..shortVecCount - 1]] ++ "(" ++ commaSep [primCon ++ " x" ++ show i | i <- [0..vecCount-1]] ++ ")"
                           ,"  {-# INLINE mk" ++ tyCon ++ " #-}"
                           ,"  {-# INLINE unpack" ++ tyCon ++ " #-}"
                           ,"instance Broadcast " ++ tyCon ++ " " ++ name ++ " where"
                           ,if shortVecCount == 1
                            then "  broadcast (" ++ primCon ++ " x) = Mk" ++ name ++ tyCon ++ suffix ++ " (broadcast" ++ name ++ "X" ++ show shortVecSize ++ "# x)"
                            else "  broadcast (" ++ primCon ++ " x) = let !v = broadcast" ++ name ++ "X" ++ show shortVecSize ++ "# x in Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep (replicate shortVecCount "v")
                           ,"  {-# INLINE broadcast #-}"
                           ,"instance SelectableF " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  selectF (MkBool" ++ tyCon ++ " !cond) (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["x" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["y" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(select" ++ shortVecName ++ "# " ++ cond_i ++ " x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..shortVecCount-1], let cond_i = if shortVecCount == 1 then if max 8 shortVecSize == max 8 vecCount then "cond" else "(fromIntegral cond)" else "(" ++ (if max 8 shortVecSize == max 8 vecCount then "" else "fromIntegral $ ") ++ "cond `unsafeShiftR` " ++ show (i * shortVecSize) ++ ")" ]
                           ,"  {-# INLINE selectF #-}"
                           ] ++
                           (if shortVecCount == 1
                           then
                             ["instance (AllLessThan indices " ++ show vecCount ++ ", ShuffleMany " ++ shortVecName ++ "# indices) => UnaryShuffle indices " ++ tyCon <+> name ++ " where"
                             ,"  unaryShuffle (Mk" ++ name ++ tyCon ++ suffix <+> "x) = Mk" ++ name ++ tyCon ++ suffix ++ " (shuffleMany# @_ @_ @indices (\\_ -> x))"
                             ,"  {-# INLINE unaryShuffle #-}"
                             ,"instance (AllLessThan indices " ++ show (2 * vecCount) ++ ", ShuffleMany " ++ shortVecName ++ "# indices) => BinaryShuffle indices " ++ tyCon <+> name ++ " where"
                             ,"  binaryShuffle (Mk" ++ name ++ tyCon ++ suffix <+> "x0) (Mk" ++ name ++ tyCon ++ suffix <+> "x1) = Mk" ++ name ++ tyCon ++ suffix ++ " (shuffleMany# @_ @_ @indices (\\case { 0 -> x0; _ -> x1 }))"
                             ,"  {-# INLINE binaryShuffle #-}"
                           ]
                           else
                             ["instance (" ++ commaSep ["i" ++ show i ++ " < " ++ show vecCount | i <- [0..vecCount-1]] ++  ", " ++ commaSep ["ShuffleMany " ++ shortVecName ++ "# [" ++ commaSep ["i" ++ show i | i <- [g*shortVecSize..(g+1)*shortVecSize-1]] ++ "]" | g <- [0..shortVecCount-1]] ++ ") => UnaryShuffle [" ++ commaSep ["i" ++ show i | i <- [0..vecCount-1]] ++ "] " ++ tyCon <+> name ++ " where"
                             ,"  unaryShuffle (Mk" ++ name ++ tyCon ++ suffix <+> spaceSep ["x" ++ show i | i <- [0..shortVecCount-1]] ++ ") = let { sources = \\case { " ++ semicolonSep [(if i == shortVecCount - 1 then "_" else show i) ++ " -> x" ++ show i | i <- [0..shortVecCount-1]] ++ " } } in Mk" ++ name ++ tyCon ++ suffix <+> spaceSep ["(shuffleMany# @_ @_ @[" ++ commaSep ["i" ++ show i | i <- [g*shortVecSize..(g+1)*shortVecSize-1]] ++ "]" ++ " sources)" | g <- [0..shortVecCount-1]]
                             ,"  {-# INLINE unaryShuffle #-}"
                             ,"instance (" ++ commaSep ["i" ++ show i ++ " < " ++ show (2 * vecCount) | i <- [0..vecCount-1]] ++  ", " ++ commaSep ["ShuffleMany " ++ shortVecName ++ "# [" ++ commaSep ["i" ++ show i | i <- [g*shortVecSize..(g+1)*shortVecSize-1]] ++ "]" | g <- [0..shortVecCount-1]] ++ ") => BinaryShuffle [" ++ commaSep ["i" ++ show i | i <- [0..vecCount-1]] ++ "] " ++ tyCon <+> name ++ " where"
                             ,"  binaryShuffle (Mk" ++ name ++ tyCon ++ suffix <+> spaceSep ["x" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix <+> spaceSep ["x" ++ show i | i <- [shortVecCount..2*shortVecCount-1]] ++ ") = let { sources = \\case { " ++ semicolonSep [(if i == 2 * shortVecCount - 1 then "_" else show i) ++ " -> x" ++ show i | i <- [0..2*shortVecCount-1]] ++ " } } in Mk" ++ name ++ tyCon ++ suffix <+> spaceSep ["(shuffleMany# @_ @_ @[" ++ commaSep ["i" ++ show i | i <- [g*shortVecSize..(g+1)*shortVecSize-1]] ++ "] sources)" | g <- [0..shortVecCount-1]]
                             ,"  {-# INLINE binaryShuffle #-}"
                             ]
                           )
        in mainDef ++ concatMap (\f -> f name primCon bitsPerElem zero maxBits) others
    genEquatable name primCon !bitsPerElem _zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
        in if maxBits == 0
           then ["instance EquatableF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  eqF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = mk" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " == y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE eqF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = max bitCount 128 `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance EquatableF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  eqF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (eq" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ mask ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize), let mask = if bitCount < 128 then " .&. 0x" ++ showHex (2^vecCount - 1) "" else ""]
                ,"  {-# INLINE eqF #-}"
                ]
    genOrderedInt name primCon !bitsPerElem _zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
        in if maxBits == 0
           then ["instance OrderedF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  ltF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = mk" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " < y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  leF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = mk" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " <= y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  gtF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = mk" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " > y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  geF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = mk" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " >= y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE ltF #-}"
                ,"  {-# INLINE leF #-}"
                ,"  {-# INLINE gtF #-}"
                ,"  {-# INLINE geF #-}"
                ,"instance MinMaxF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  minF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(min x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  maxF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(max x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  minimumNumberF = minF"
                ,"  maximumNumberF = maxF"
                ,"  {-# INLINE minF #-}"
                ,"  {-# INLINE maxF #-}"
                ,"  {-# INLINE minimumNumberF #-}"
                ,"  {-# INLINE maximumNumberF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = max bitCount 128 `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance OrderedF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  ltF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (lt" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ mask ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize), let mask = if bitCount < 128 then " .&. 0x" ++ showHex (2^vecCount - 1) "" else ""]
                ,"  leF !x !y = complementF (ltF y x)"
                ,"  gtF !x !y = ltF y x"
                ,"  geF !x !y = complementF (ltF x y)"
                ,"  {-# INLINE ltF #-}"
                ,"  {-# INLINE leF #-}"
                ,"  {-# INLINE gtF #-}"
                ,"  {-# INLINE geF #-}"
                ,"instance MinMaxF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  minF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(min" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  maxF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(max" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  minimumNumberF = minF"
                ,"  maximumNumberF = maxF"
                ,"  {-# INLINE minF #-}"
                ,"  {-# INLINE maxF #-}"
                ,"  {-# INLINE minimumNumberF #-}"
                ,"  {-# INLINE maximumNumberF #-}"
                ]
    genOrderedFloat name primCon !bitsPerElem _zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
        in if maxBits == 0
           then ["instance OrderedF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  ltF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = mk" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " < y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  leF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = mk" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " <= y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  gtF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = mk" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " > y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  geF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = mk" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " >= y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE ltF #-}"
                ,"  {-# INLINE leF #-}"
                ,"  {-# INLINE gtF #-}"
                ,"  {-# INLINE geF #-}"
                ,"instance MinMaxF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  minF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(min x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  maxF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(max x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  minimumNumberF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(minimumNumber x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  maximumNumberF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(maximumNumber x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE minF #-}"
                ,"  {-# INLINE maxF #-}"
                ,"  {-# INLINE minimumNumberF #-}"
                ,"  {-# INLINE maximumNumberF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = max bitCount 128 `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance OrderedF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  ltF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (lt" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ mask ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize), let mask = if bitCount < 128 then " .&. 0x" ++ showHex (2^vecCount - 1) "" else ""]
                ,"  leF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (le" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ mask ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize), let mask = if bitCount < 128 then " .&. 0x" ++ showHex (2^vecCount - 1) "" else ""]
                ,"  gtF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (gt" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ mask ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize), let mask = if bitCount < 128 then " .&. 0x" ++ showHex (2^vecCount - 1) "" else ""]
                ,"  geF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (ge" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ mask ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize), let mask = if bitCount < 128 then " .&. 0x" ++ showHex (2^vecCount - 1) "" else ""]
                ,"  {-# INLINE ltF #-}"
                ,"  {-# INLINE leF #-}"
                ,"  {-# INLINE gtF #-}"
                ,"  {-# INLINE geF #-}"
                ,"instance MinMaxF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  minF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(minimum" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  maxF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(maximum" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  minimumNumberF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(minimumNumber" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  maximumNumberF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(maximumNumber" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  {-# INLINE minF #-}"
                ,"  {-# INLINE maxF #-}"
                ,"  {-# INLINE minimumNumberF #-}"
                ,"  {-# INLINE maximumNumberF #-}"
                ]
    genNum isSigned isFloating name primCon !bitsPerElem _zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
        in if maxBits == 0
           then (if isFloating
                 then ["negate" ++ tyCon ++ name ++ " :: " ++ tyCon <+> name ++ " -> " ++ tyCon <+> name
                      ,"negate" ++ tyCon ++ name ++ " (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(- x" ++ show i ++ ")" | i <- [0..vecCount-1]]
                      ,"#if defined(USE_FMA)"
                      ,"{-# INLINE [0] negate" ++ tyCon ++ name ++ " #-}"
                      ,"#else"
                      ,"{-# INLINE negate" ++ tyCon ++ name ++ " #-}"
                      ,"#endif"
                      ]
                 else []
                ) ++
                ["instance NumF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  plusF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " + y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  minusF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " - y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  timesF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " * y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ] ++
                (if isFloating
                 then ["  negateF = negate" ++ tyCon ++ name]
                 else ["  negateF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(- x" ++ show i ++ ")" | i <- [0..vecCount-1]]]
                ) ++
                ["  {-# INLINE plusF #-}"
                ,"  {-# INLINE minusF #-}"
                ,"  {-# INLINE timesF #-}"
                ,"  {-# INLINE negateF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = max bitCount 128 `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in (if isFloating
                 then ["negate" ++ tyCon ++ name ++ " :: " ++ tyCon <+> name ++ " -> " ++ tyCon <+> name
                      ,"negate" ++ tyCon ++ name ++ " (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(negate" ++ shortVecName ++ "# u" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                      ,"#if defined(USE_FMA)"
                      ,"{-# INLINE [0] negate" ++ tyCon ++ name ++ " #-}"
                      ,"#else"
                      ,"{-# INLINE negate" ++ tyCon ++ name ++ " #-}"
                      ,"#endif"
                      ]
                 else []
                ) ++
                ["instance NumF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  plusF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(plus" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  minusF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(minus" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  timesF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(times" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ]
                ++ (if isSigned
                    then (if isFloating
                          then ["  negateF = negate" ++ tyCon ++ name]
                          else ["  negateF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(negate" ++ shortVecName ++ "# u" ++ show i ++ ")" | i <- [0..shortVecCount-1]]]
                         ) ++
                         ["  absF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(abs" ++ shortVecName ++ "# u" ++ show i ++ ")" | i <- [0..shortVecCount-1]]]
                    else ["  -- Currently, there is no negate" ++ shortVecName ++ "#"
                         ,"  absF x = x"
                         ]
                   )
                ++ ["  {-# INLINE plusF #-}"
                   ,"  {-# INLINE minusF #-}"
                   ,"  {-# INLINE timesF #-}"
                   ]
                ++ ["  {-# INLINE negateF #-}" | isSigned]
                ++ ["  {-# INLINE absF #-}"]
    genFractional name primCon !bitsPerElem _zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
        in if maxBits == 0
           then ["instance FractionalF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  divideF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " / y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  recipF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(recip x" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE divideF #-}"
                ,"  {-# INLINE recipF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = max bitCount 128 `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance FractionalF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  divideF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(divide" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  {-# INLINE divideF #-}"
                ]
    genFloating name primCon !bitsPerElem _zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
        in if maxBits == 0
           then ["instance FloatingF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  sqrtF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(sqrt x" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE sqrtF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = max bitCount 128 `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance FloatingF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  sqrtF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(sqrt" ++ shortVecName ++ "# u" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  {-# INLINE sqrtF #-}"
                ]
    genFMA name primCon !bitsPerElem _zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
        in (if maxBits == 0
            then ["instance HasFMA => FusedMultiplyAddF " ++ tyCon ++ " " ++ name ++ " where"
                 ,"#if defined(USE_FMA)"
                 ,"  fusedMultiplyAddF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["z" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(fusedMultiplyAdd x" ++ show i ++ " y" ++ show i ++ " z" ++ show i ++ ")" | i <- [0..vecCount-1]]
                 ,"  {-# INLINE fusedMultiplyAddF #-}"
                 ,"#else"
                 ,"  fusedMultiplyAddF _ _ _ = fmaIsDisabled"
                 ,"#endif"
                 ]
            else
              let shortVecSize = vecBitCount `div` bitsPerElem
                  shortVecCount = max bitCount 128 `div` vecBitCount
                  shortVecName = name ++ "X" ++ show shortVecSize
                  suffix | shortVecCount == 1 = ""
                         | otherwise = "WithVec" ++ show vecBitCount
              in ["instance HasFMA => FusedMultiplyAddF " ++ tyCon ++ " " ++ name ++ " where"
                 ,"#if defined(USE_FMA)"
                 ,"  fusedMultiplyAddF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["w" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(fmadd" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ " w" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                 ,"  {-# INLINE fusedMultiplyAddF #-}"
                 ,"#else"
                 ,"  fusedMultiplyAddF _ _ _ = fmaIsDisabled"
                 ,"#endif"
                 ]
           ) ++
           -- Be careful to use specialized 'negate' function on the right hand side.
           -- Otherwise, GHC will consider that the dictionaries 'Num Float' and 'Num Double'
           -- are recursive and will refuse to inline them.
           ["#if defined(USE_FMA)"
           ,"{-# RULES"
           ,"\"Fusible/*+/" ++ tyCon <+> name ++ "\" forall a b c."
           ,"  a F.* b F.+ c = fusedMultiplyAdd a b c :: " ++ tyCon <+> name
           ,"\"Fusible/*-/" ++ tyCon <+> name ++ "\" forall a b c."
           ,"  a F.* b F.- c = fusedMultiplyAdd a b (negate" ++ tyCon ++ name ++ " c) :: " ++ tyCon <+> name
           ,"\"Fusible/-*+/" ++ tyCon <+> name ++ "\" forall a b c."
           ,"  negate" ++ tyCon ++ name ++ " (a F.* b) F.+ c = fusedMultiplyAdd (negate" ++ tyCon ++ name ++ " a) b c :: " ++ tyCon <+> name
           ,"\"Fusible/-*-/" ++ tyCon <+> name ++ "\" forall a b c."
           ,"  negate" ++ tyCon ++ name ++ " (a F.* b) F.- c = fusedMultiplyAdd (negate" ++ tyCon ++ name ++ " a) b (negate" ++ tyCon ++ name ++ " c) :: " ++ tyCon <+> name
           ,"\"Fusible/+*/" ++ tyCon <+> name ++ "\" forall a b c."
           ,"  a F.+ b F.* c = fusedMultiplyAdd b c a :: " ++ tyCon <+> name
           ,"\"Fusible/-*/" ++ tyCon <+> name ++ "\" forall a b c."
           ,"  a F.- b F.* c = fusedMultiplyAdd (negate" ++ tyCon ++ name ++ " b) c a :: " ++ tyCon <+> name
           ,"  #-}"
           ,"#endif"
           ]
    genBits name primCon !bitsPerElem _zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
        in if maxBits == 0
           then ["instance BooleanF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  andF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " .&. y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  orF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " .|. y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  xorF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(xor x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  complementF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(complement x" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE andF #-}"
                ,"  {-# INLINE orF #-}"
                ,"  {-# INLINE xorF #-}"
                ,"  {-# INLINE complementF #-}"
                ,"instance BitShiftF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  shiftLF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") !i = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(shiftL x" ++ show i ++ " i)" | i <- [0..vecCount-1]]
                ,"  unsafeShiftLF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") !i = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(unsafeShiftL x" ++ show i ++ " i)" | i <- [0..vecCount-1]]
                ,"  shiftRF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") !i = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(shiftR x" ++ show i ++ " i)" | i <- [0..vecCount-1]]
                ,"  unsafeShiftRF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") !i = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(unsafeShiftR x" ++ show i ++ " i)" | i <- [0..vecCount-1]]
                ,"  {-# INLINE shiftLF #-}"
                ,"  {-# INLINE unsafeShiftLF #-}"
                ,"  {-# INLINE shiftRF #-}"
                ,"  {-# INLINE unsafeShiftRF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = max bitCount 128 `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance BooleanF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  andF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(and" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  orF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(or" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  xorF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(xor" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  complementF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(complement" ++ shortVecName ++ "# u" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  {-# INLINE andF #-}"
                ,"  {-# INLINE orF #-}"
                ,"  {-# INLINE xorF #-}"
                ,"  {-# INLINE complementF #-}"
                ,"instance BitShiftF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  shiftLF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (I# i) = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(shiftL" ++ shortVecName ++ "# u" ++ show i ++ " i)" | i <- [0..shortVecCount-1]]
                ,"  shiftRF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (I# i) = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(shiftR" ++ shortVecName ++ "# u" ++ show i ++ " i)" | i <- [0..shortVecCount-1]]
                ,"  {-# INLINE shiftLF #-}"
                ,"  {-# INLINE shiftRF #-}"
                ]
    genEnumFromZero litSuffix name primCon !bitsPerElem _zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
        in if maxBits == 0
           then ["instance EnumFromZero_ " ++ tyCon ++ " " ++ name ++ " where"
                ,"  enumFromZero = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep [show i | i <- [0..vecCount-1]]
                ,"  -- {-# INLINE enumFromZero #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = max bitCount 128 `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance EnumFromZero_ " ++ tyCon ++ " " ++ name ++ " where"
                ,"#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)"
                ,"  enumFromZero = Mk" ++ name ++ tyCon ++ suffix <+> spaceSep ["(pack" ++ shortVecName ++ "# (# " ++ commaSep [(if k < vecCount then show k else "0") ++ litSuffix | j <- [0..shortVecSize - 1], let k = i * shortVecSize + j] ++ " #))" | i <- [0..shortVecCount - 1]]
                ,"#else"
                ,"  enumFromZero = mk" ++ tyCon ++ " " ++ spaceSep [show i | i <- [0..vecCount-1]]
                ,"#endif"
                ,"  -- {-# INLINE enumFromZero #-}"
                ]
    genPrim name primCon !bitsPerElem zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
            i_plus 0 = "i"
            i_plus k = "(i +# " ++ show k ++ "#)"
        in if maxBits == 0
           then ["instance MultiPrim " ++ tyCon ++ " " ++ name ++ " where"
                ,"  indexByteArraySIMD# ba i = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " (GHC.Exts.index" ++ name ++ "Array# ba " ++ i_plus i ++ "))" | i <- [0..vecCount-1]]
                ,"  readByteArraySIMD# mba i s0 = " ++ concat ["case GHC.Exts.read" ++ name ++ "Array# mba " ++ i_plus i ++ " s" ++ show i ++ " of (# s" ++ show (i + 1) ++ ", x" ++ show i ++ " #) -> " | i <- [0..vecCount-1]] ++ "(# s" ++ show vecCount ++ ", Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " #)"
                ,"  writeByteArraySIMD# mba i (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ ") s0 = " ++ concat ["case GHC.Exts.write" ++ name ++ "Array# mba " ++ i_plus i ++ " x" ++ show i ++ " s" ++ show i ++ " of s" ++ show (i + 1) ++ " -> " | i <- [0..vecCount-2]] ++ "GHC.Exts.write" ++ name ++ "Array# mba (i +# " ++ show (vecCount - 1) ++ "#) x" ++ show (vecCount - 1) ++ " s" ++ show (vecCount - 1)
                ,"  {-# INLINE indexByteArraySIMD# #-}"
                ,"  {-# INLINE readByteArraySIMD# #-}"
                ,"  {-# INLINE writeByteArraySIMD# #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = max bitCount 128 `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
                 makeRead s i readAcc vecAcc
                   | i >= vecCount = (reverse readAcc, reverse vecAcc, s)
                   | i + shortVecSize <= vecCount
                   = let s' = s + 1
                         vi = i `quot` shortVecSize
                         read = "case read" ++ name ++ "ArrayAs" ++ shortVecName ++ "# mba " ++ i_plus i ++ " s" ++ show s ++ " of (# s" ++ show s' ++ ", v" ++ show vi ++ " #) -> "
                         pack = "v" ++ show vi
                     in makeRead s' (i + shortVecSize) (read : readAcc) (pack : vecAcc)
                   | otherwise
                   = let lastLoop s i readAcc elems
                           | i < vecCount = let s' = s + 1
                                                read = "case GHC.Exts.read" ++ name ++ "Array# mba " ++ i_plus i ++ " s" ++ show s ++ " of (# s" ++ show s' ++ ", x" ++ show i ++ " #) -> "
                                            in lastLoop s' (i + 1) (read : readAcc) (("x" ++ show i) : elems)
                           | otherwise = let pack = "(pack" ++ shortVecName ++ "# (# " ++ commaSep (reverse elems ++ replicate (shortVecSize - length elems) zero) ++ " #))"
                                         in (reverse readAcc, reverse (pack : vecAcc), s)
                     in lastLoop s i readAcc []
                 makeWrite s i writesAcc
                   | i >= vecCount = reverse writesAcc
                   | i + shortVecSize == vecCount
                   = let s' = s + 1
                         vi = i `quot` shortVecSize
                         write = "write" ++ name ++ "ArrayAs" ++ shortVecName ++ "# mba " ++ i_plus i ++ " v" ++ show vi ++ " s" ++ show s
                     in reverse (write : writesAcc)
                   | i + shortVecSize < vecCount
                   = let s' = s + 1
                         vi = i `quot` shortVecSize
                         write = "case write" ++ name ++ "ArrayAs" ++ shortVecName ++ "# mba " ++ i_plus i ++ " v" ++ show vi ++ " s" ++ show s ++ " of s" ++ show s' ++ " -> "
                     in makeWrite s' (i + shortVecSize) (write : writesAcc)
                   | otherwise
                   = let lastLoop s i scalarWriteAcc patternAcc
                           | i < vecCount - 1 = let s' = s + 1
                                                    write = "case GHC.Exts.write" ++ name ++ "Array# mba " ++ i_plus i ++ " x" ++ show i ++ " s" ++ show s ++ " of s" ++ show s' ++ " -> "
                                                in lastLoop s' (i + 1) (write : scalarWriteAcc) (("x" ++ show i) : patternAcc)
                           | otherwise = let unpack = "case unpack" ++ shortVecName ++ "# v" ++ show (i `quot` shortVecSize) ++ " of (# " ++ commaSep (reverse (("x" ++ show i) : patternAcc) ++ replicate (shortVecSize - length patternAcc - 1) "_") ++ " #) -> "
                                             lastWrite = "GHC.Exts.write" ++ name ++ "Array# mba " ++ i_plus i ++ " x" ++ show i ++ " s" ++ show s
                                         in reverse writesAcc ++ unpack : reverse (lastWrite : scalarWriteAcc)
                     in lastLoop s i [] []
             in ["instance MultiPrim " ++ tyCon ++ " " ++ name ++ " where"
                ,"  indexByteArraySIMD# ba i = Mk" ++ name ++ tyCon ++ suffix <+>
                   spaceSep [if i * shortVecSize + shortVecSize <= vecCount
                             then "(index" ++ name ++ "ArrayAs" ++ shortVecName ++ "# ba " ++ i_plus (i * shortVecSize) ++ ")"
                             else "(pack" ++ shortVecName ++ "# (# " ++ commaSep [if k < vecCount then "GHC.Exts.index" ++ name ++ "Array# ba " ++ i_plus k else zero | j <- [0..shortVecSize - 1], let k = i * shortVecSize + j] ++ " #))"
                            | i <- [0..shortVecCount-1]]
                ,case makeRead 0 0 [] [] of
                   (reads, vecs, s) -> "  readByteArraySIMD# mba i s0 = " ++ concat reads ++ "(# s" ++ show s ++ ", Mk" ++ name ++ tyCon ++ suffix <+> spaceSep vecs ++ " #)"
                ,case makeWrite 0 0 [] of
                   writes -> "  writeByteArraySIMD# mba i (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") s0 = " ++ concat writes
                ,"  {-# INLINE indexByteArraySIMD# #-}"
                ,"  {-# INLINE readByteArraySIMD# #-}"
                ,"  {-# INLINE writeByteArraySIMD# #-}"
                ]
    genStorable name primCon !bitsPerElem zero maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
            i_plus 0 = "i"
            i_plus k = "(i +# " ++ show k ++ "#)"
        in if maxBits == 0
           then ["instance MultiStorable " ++ tyCon ++ " " ++ name ++ " where"
                ,"  peekElemOffSIMD (Ptr addr) (I# i) = IO (\\s0 -> " ++ concat ["case GHC.Exts.read" ++ name ++ "OffAddr# addr " ++ i_plus i ++ " s" ++ show i ++ " of (# s" ++ show (i + 1) ++ ", x" ++ show i ++ " #) -> " | i <- [0..vecCount-1]] ++ "(# s" ++ show vecCount ++ ", Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " #))"
                ,"  pokeElemOffSIMD (Ptr addr) (I# i) (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ ") = IO (\\s0 -> " ++ concat ["case GHC.Exts.write" ++ name ++ "OffAddr# addr " ++ i_plus i ++ " x" ++ show i ++ " s" ++ show i ++ " of s" ++ show (i + 1) ++ " -> " | i <- [0..vecCount-2]] ++ "(# GHC.Exts.write" ++ name ++ "OffAddr# addr (i +# " ++ show (vecCount - 1) ++ "#) x" ++ show (vecCount - 1) ++ " s" ++ show (vecCount - 1) ++ ", () #))"
                ,"  {-# INLINE peekElemOffSIMD #-}"
                ,"  {-# INLINE pokeElemOffSIMD #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = max bitCount 128 `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
                 makeRead s i readAcc vecAcc
                   | i >= vecCount = (reverse readAcc, reverse vecAcc, s)
                   | i + shortVecSize <= vecCount
                   = let s' = s + 1
                         vi = i `quot` shortVecSize
                         read = "case read" ++ name ++ "OffAddrAs" ++ shortVecName ++ "# addr " ++ i_plus i ++ " s" ++ show s ++ " of (# s" ++ show s' ++ ", v" ++ show vi ++ " #) -> "
                         pack = "v" ++ show vi
                     in makeRead s' (i + shortVecSize) (read : readAcc) (pack : vecAcc)
                   | otherwise
                   = let lastLoop s i readAcc elems
                           | i < vecCount = let s' = s + 1
                                                read = "case GHC.Exts.read" ++ name ++ "OffAddr# addr " ++ i_plus i ++ " s" ++ show s ++ " of (# s" ++ show s' ++ ", x" ++ show i ++ " #) -> "
                                            in lastLoop s' (i + 1) (read : readAcc) (("x" ++ show i) : elems)
                           | otherwise = let pack = "(pack" ++ shortVecName ++ "# (# " ++ commaSep (reverse elems ++ replicate (shortVecSize - length elems) zero) ++ " #))"
                                         in (reverse readAcc, reverse (pack : vecAcc), s)
                     in lastLoop s i readAcc []
                 makeWrite s i writesAcc
                   | i >= vecCount = (reverse writesAcc, s)
                   | i + shortVecSize <= vecCount
                   = let s' = s + 1
                         vi = i `quot` shortVecSize
                         write = "case write" ++ name ++ "OffAddrAs" ++ shortVecName ++ "# addr " ++ i_plus i ++ " v" ++ show vi ++ " s" ++ show s ++ " of s" ++ show s' ++ " -> "
                     in makeWrite s' (i + shortVecSize) (write : writesAcc)
                   | otherwise
                   = let lastLoop s i scalarWriteAcc patternAcc
                           | i < vecCount = let s' = s + 1
                                                write = "case GHC.Exts.write" ++ name ++ "OffAddr# addr " ++ i_plus i ++ " x" ++ show i ++ " s" ++ show s ++ " of s" ++ show s' ++ " -> "
                                            in lastLoop s' (i + 1) (write : scalarWriteAcc) (("x" ++ show i) : patternAcc)
                           | otherwise = let unpack = "case unpack" ++ shortVecName ++ "# v" ++ show (i `quot` shortVecSize) ++ " of (# " ++ commaSep (reverse patternAcc ++ replicate (shortVecSize - length patternAcc) "_") ++ " #) -> "
                                         in (reverse writesAcc ++ unpack : reverse scalarWriteAcc, s)
                     in lastLoop s i [] []
             in ["instance MultiStorable " ++ tyCon ++ " " ++ name ++ " where"
                ,case makeRead 0 0 [] [] of
                   (reads, vecs, s) -> "  peekElemOffSIMD (Ptr addr) (I# i) = IO (\\s0 -> " ++ concat reads ++ "(# s" ++ show s ++ ", Mk" ++ name ++ tyCon ++ suffix <+> spaceSep vecs ++ " #))"
                ,case makeWrite 0 0 [] of
                    (writes, s) -> "  pokeElemOffSIMD (Ptr addr) (I# i) (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = IO (\\s0 -> " ++ concat writes ++"(# s" ++ show s ++ ", () #))"
                ,"  {-# INLINE peekElemOffSIMD #-}"
                ,"  {-# INLINE pokeElemOffSIMD #-}"
                ]
    genTuple !n
      = ["data instance " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["!(" ++ tyCon ++ " a" ++ show i ++ ")" | i <- [0..n-1]]
        ,"instance (" ++ commaSep ["Pack" ++ tyCon ++ " " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => Pack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
        ,"  mk" ++ tyCon ++ " " ++ spaceSep ["(" ++ commaSep ["x" ++ show i ++ "_" ++ show j | j <- [0..n-1]] ++ ")" | i <- [0..vecCount-1]] ++ " = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(mk" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i ++ "_" ++ show j | i <- [0..vecCount-1]] ++ ")" | j <- [0..n-1]]
        ,"  unpack" ++ tyCon ++ " (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") = " ++ concat ["case unpack" ++ tyCon ++ " v" ++ show i ++ " of (" ++ commaSep ["x" ++ show j ++ "_" ++ show i | j <- [0..vecCount-1]] ++ ") -> " | i <- [0..n-1]] ++ "(" ++ commaSep ["(" ++ commaSep ["x" ++ show i ++ "_" ++ show j | j <- [0..n-1]] ++ ")" | i <- [0..vecCount-1]] ++ ")"
        ,"  {-# INLINE mk" ++ tyCon ++ " #-}"
        ,"  {-# INLINE unpack" ++ tyCon ++ " #-}"
        ,"instance (" ++ commaSep ["Broadcast " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => Broadcast " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
        ,"  broadcast (" ++ commaSep ["x" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(broadcast x" ++ show i ++ ")" | i <- [0..n-1]]
        ,"  {-# INLINE broadcast #-}"
        ,"instance (" ++ commaSep ["SelectableF " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => SelectableF " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
        ,"  selectF !cond (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["x" ++ show i | i <- [0..n-1]] ++ ") (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["y" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(selectF cond x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..n-1]]
        ,"  {-# INLINE selectF #-}"
        ] {- ++ if n <= maxTupleLenForUnboxedVector
             then ["instance (" ++ commaSep ["UnboxSIMD " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => UnboxSIMD " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
                  ,"  unsafeIndexUnboxedSIMD (VUB.V_" ++ show n ++ " _ " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") !i = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(unsafeIndexUnboxedSIMD v" ++ show i ++ " i)" | i <- [0..n-1]]
                  ,"  unsafeReadUnboxedSIMD (VUB.MV_" ++ show n ++ " _ " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") !i = do { " ++ semicolonSep ["!s" ++ show i ++ " <- unsafeReadUnboxedSIMD v" ++ show i ++ " i" | i <- [0..n-1]] ++ "; pure (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["s" ++ show i | i <- [0..n-1]] ++ ") }"
                  ,"  unsafeWriteUnboxedSIMD (VUB.MV_" ++ show n ++ " _ " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") !i (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["s" ++ show i | i <- [0..n-1]] ++ ") = do { " ++ semicolonSep ["unsafeWriteUnboxedSIMD v" ++ show i ++ " i s" ++ show i | i <- [0..n-1]] ++ " }"
                  ,"  {-# INLINE unsafeIndexUnboxedSIMD #-}"
                  ,"  {-# INLINE unsafeReadUnboxedSIMD #-}"
                  ,"  {-# INLINE unsafeWriteUnboxedSIMD #-}"
                  ]
             else [] -}
    genNewtype !name
      = ["newtype instance " ++ tyCon ++ " (" ++ name ++ " a) = Mk" ++ name ++ tyCon ++ " (" ++ tyCon ++ " a)"
        ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " a => Pack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  mk" ++ tyCon ++ " = coerce (mk" ++ tyCon ++ " @" ++ tyCon ++ " @a)"
        ,"  unpack" ++ tyCon ++ " = coerce (unpack" ++ tyCon ++ " @" ++ tyCon ++ " @a)"
        ,"  {-# INLINE mk" ++ tyCon ++ " #-}"
        ,"  {-# INLINE unpack" ++ tyCon ++ " #-}"
        ,"instance Broadcast " ++ tyCon ++ " a => Broadcast " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  broadcast = coerce (broadcast @" ++ tyCon ++ " @a)"
        ,"  {-# INLINE broadcast #-}"
        ,"instance SelectableF " ++ tyCon ++ " a => SelectableF " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  selectF = coerce (selectF @" ++ tyCon ++ " @a)"
        ,"  {-# INLINE selectF #-}"
        {-
        ,"instance UnboxSIMD " ++ tyCon ++ " a => UnboxSIMD " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  unsafeIndexUnboxedSIMD = coerce (unsafeIndexUnboxedSIMD @" ++ tyCon ++ " @a)"
        ,"  unsafeReadUnboxedSIMD = coerce (unsafeReadUnboxedSIMD @" ++ tyCon ++ " @a)"
        ,"  unsafeWriteUnboxedSIMD = coerce (unsafeWriteUnboxedSIMD @" ++ tyCon ++ " @a)"
        ,"  {-# INLINE unsafeIndexUnboxedSIMD #-}"
        ,"  {-# INLINE unsafeReadUnboxedSIMD #-}"
        ,"  {-# INLINE unsafeWriteUnboxedSIMD #-}"
        -}
        ]

genHalf :: Int -> Int -> [String]
genHalf !vecCount !maxBits
  = [if vecCount == 2
     then "type instance HalfVector " ++ tyCon ++ " = Identity"
     else "type instance HalfVector " ++ tyCon ++ " = X" ++ show (vecCount `quot` 2)
    ,"instance SplitShortVector " ++ tyCon ++ " Bool where"
    ,if vecCount == 2
     then "  splitShortVector (MkBool" ++ tyCon ++ " !x) = (Identity (testBit x 0), Identity (testBit x 1))"
     else "  splitShortVector (MkBool" ++ tyCon ++ " !x) = (MkBoolX" ++ show (vecCount `quot` 2) ++ " $ fromIntegral $ x .&. 0x" ++ showHex (2^(vecCount `quot` 2) - 1) ", MkBoolX" ++ show (vecCount `quot` 2) ++ " $ fromIntegral $ x `unsafeShiftR` " ++ show (vecCount `quot` 2) ++ ")"
    ,if vecCount == 2
     then "  joinShortVector (Identity !x) (Identity !y) = MkBool" ++ tyCon ++ " ((if x then 1 else 0) .|. (if y then 2 else 0))"
     else "  joinShortVector (MkBoolX" ++ show (vecCount `quot` 2) ++ " !x) (MkBoolX" ++ show (vecCount `quot` 2) ++ " !y) = MkBool" ++ tyCon ++ " (fromIntegral x .|. (fromIntegral y `unsafeShiftL` " ++ show (vecCount `quot` 2) ++ "))"
    ]
    ++ genType "Float" "F#" 32 maxBits
    ++ genType "Double" "D#" 64 maxBits
    ++ ["#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)" | maxBits == 128]
    ++ genType "Int8" "I8#" 8 maxBits
    ++ genType "Int16" "I16#" 16 maxBits
    ++ genType "Int32" "I32#" 32 maxBits
    ++ genType "Int64" "I64#" 64 maxBits
    ++ genType "Word8" "W8#" 8 maxBits
    ++ genType "Word16" "W16#" 16 maxBits
    ++ genType "Word32" "W32#" 32 maxBits
    ++ genType "Word64" "W64#" 64 maxBits
    ++ (if maxBits == 128
        then ["#else"
             ,"-- The NCG of GHC 9.12 does not support integer vectors"]
             ++ genType "Int8" "I8#" 8 0
             ++ genType "Int16" "I16#" 16 0
             ++ genType "Int32" "I32#" 32 0
             ++ genType "Int64" "I64#" 64 0
             ++ genType "Word8" "W8#" 8 0
             ++ genType "Word16" "W16#" 16 0
             ++ genType "Word32" "W32#" 32 0
             ++ genType "Word64" "W64#" 64 0
             ++ ["#endif"]
        else []
       )
    ++ genNewtype "Sum"
    ++ genNewtype "Product"
    ++ genNewtype "Min"
    ++ genNewtype "Max"
    ++ (if vecCount == 2
        then ["instance SplitShortVector " ++ tyCon ++ " a => SplitShortVector " ++ tyCon ++ " (Complex a) where"
             ,"  splitShortVector (MkComplex" ++ tyCon ++ " s t) = case splitShortVector s of (Identity x0, Identity x1) -> case splitShortVector t of (Identity y0, Identity y1) -> (Identity (x0 :+ y0), Identity (x1 :+ y1))"
             ,"  joinShortVector (Identity (x0 :+ y0)) (Identity (x1 :+ y1)) = MkComplex" ++ tyCon ++ " (joinShortVector (Identity x0) (Identity x1)) (joinShortVector (Identity y0) (Identity y1))"
             ,"  {-# INLINE splitShortVector #-}"
             ,"  {-# INLINE joinShortVector #-}"
             ]
        else
          let halfTyCon = "X" ++ show (vecCount `quot` 2)
          in ["instance SplitShortVector " ++ tyCon ++ " a => SplitShortVector " ++ tyCon ++ " (Complex a) where"
             ,"  splitShortVector (MkComplex" ++ tyCon ++ " s t) = case splitShortVector s of (x0, x1) -> case splitShortVector t of (y0, y1) -> (MkComplex" ++ halfTyCon ++ " x0 y0, MkComplex" ++ halfTyCon ++ " x1 y1)"
             ,"  joinShortVector (MkComplex" ++ halfTyCon ++ " x0 y0) (MkComplex" ++ halfTyCon ++ " x1 y1) = MkComplex" ++ tyCon ++ " (joinShortVector x0 x1) (joinShortVector y0 y1)"
             ,"  {-# INLINE splitShortVector #-}"
             ,"  {-# INLINE joinShortVector #-}"
             ]
       )
    ++ (if vecCount == 2
        then ["instance SplitShortVector " ++ tyCon ++ " () where"
             ,"  splitShortVector MkUnit" ++ tyCon ++ " = (Identity (), Identity ())"
             ,"  joinShortVector _ _ = MkUnit" ++ tyCon
             ,"  {-# INLINE splitShortVector #-}"
             ,"  {-# INLINE joinShortVector #-}"
             ]
        else
          let halfTyCon = "X" ++ show (vecCount `quot` 2)
          in ["instance SplitShortVector " ++ tyCon ++ " () where"
             ,"  splitShortVector MkUnit" ++ tyCon ++ " = (MkUnit" ++ halfTyCon ++ ", MkUnit" ++ halfTyCon ++ ")"
             ,"  joinShortVector MkUnit" ++ halfTyCon ++ " MkUnit" ++ halfTyCon ++ " = MkUnit" ++ tyCon
             ,"  {-# INLINE splitShortVector #-}"
             ,"  {-# INLINE joinShortVector #-}"
             ]
       )
    ++ concatMap genTuple [2..maxTupleLen]
  where
    tyCon = 'X' : show vecCount
    genType name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min (max bitCount 128) maxBits
            halfTyCon = if vecCount == 2 then "Identity" else "X" ++ show (vecCount `quot` 2)
        in if maxBits == 0
           then if vecCount == 2
                then ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                     ,"  splitShortVector (Mk" ++ name ++ tyCon ++ "WithElems x0 x1) = (Identity x0, Identity x1)"
                     ,"  joinShortVector (Identity x0) (Identity x1) = Mk" ++ name ++ tyCon ++ "WithElems x0 x1"
                     ,"  {-# INLINE splitShortVector #-}"
                     ,"  {-# INLINE joinShortVector #-}"
                     ]
                else ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                     ,"  splitShortVector (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ", Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ")"
                     ,"  joinShortVector (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ") (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]]
                     ,"  {-# INLINE splitShortVector #-}"
                     ,"  {-# INLINE joinShortVector #-}"
                     ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in if vecCount == 2
                then ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                     ,"  splitShortVector v = coerce (unpack" ++ tyCon ++ " v)"
                     ,"  joinShortVector (Identity x0) (Identity x1) = mk" ++ tyCon ++ " x0 x1"
                     ,"  {-# INLINE splitShortVector #-}"
                     ,"  {-# INLINE joinShortVector #-}"
                     ]
                else
                  let halfVecBitCount = min (bitsPerElem * vecCount `div` 2) maxBits
                  in if halfVecBitCount < 128 || shortVecCount == 1
                     then ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                          ,"  splitShortVector v = case unpack" ++ tyCon ++ " v of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> (mk" ++ halfTyCon <+> spaceSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ", mk" ++ halfTyCon <+> spaceSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ")"
                          ,"  joinShortVector u v = case unpack" ++ halfTyCon ++ " u of (" ++ commaSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ") -> case unpack" ++ halfTyCon ++ " v of (" ++ commaSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ") -> mk" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]]
                          ,"  {-# INLINE splitShortVector #-}"
                          ,"  {-# INLINE joinShortVector #-}"
                          ]
                     else
                       let halfSuffix | shortVecCount == 2 = ""
                                      | otherwise = "WithVec" ++ show halfVecBitCount
                       in ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                          ,"  splitShortVector (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = (Mk" ++ name ++ halfTyCon ++ halfSuffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..(shortVecCount `quot` 2)-1]] ++ ", Mk" ++ name ++ halfTyCon ++ halfSuffix ++ " " ++ spaceSep ["v" ++ show i | i <- [shortVecCount `quot` 2..shortVecCount-1]] ++ ")"
                          ,"  joinShortVector (Mk" ++ name ++ halfTyCon ++ halfSuffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..(shortVecCount `quot` 2)-1]] ++ ") (Mk" ++ name ++ halfTyCon ++ halfSuffix ++ " " ++ spaceSep ["v" ++ show i | i <- [shortVecCount `quot` 2..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]]
                          ,"  {-# INLINE splitShortVector #-}"
                          ,"  {-# INLINE joinShortVector #-}"
                          ]
    genTuple !n
      = if vecCount == 2
        then ["instance (" ++ commaSep ["SplitShortVector " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => SplitShortVector " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
             ,"  splitShortVector (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") = " ++ concat ["case splitShortVector v" ++ show i ++ " of (Identity a" ++ show i ++ ", Identity b" ++ show i ++ ") -> " | i <- [0..n-1]] ++ "(Identity (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ "), Identity (" ++ commaSep ["b" ++ show i | i <- [0..n-1]] ++ "))"
             ,"  joinShortVector (Identity (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ")) (Identity (" ++ commaSep ["b" ++ show i | i <- [0..n-1]] ++ ")) = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(joinShortVector (Identity a" ++ show i ++ ") (Identity b" ++ show i ++ "))" | i <- [0..n-1]]
             ,"  {-# INLINE splitShortVector #-}"
             ,"  {-# INLINE joinShortVector #-}"
             ]
        else
          let halfTyCon = "X" ++ show (vecCount `quot` 2)
          in ["instance (" ++ commaSep ["SplitShortVector " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => SplitShortVector " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
             ,"  splitShortVector (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") = " ++ concat ["case splitShortVector v" ++ show i ++ " of (a" ++ show i ++ ", b" ++ show i ++ ") -> " | i <- [0..n-1]] ++ "(MkTuple" ++ show n ++ halfTyCon ++ " " ++ spaceSep ["a" ++ show i | i <- [0..n-1]] ++ ", MkTuple" ++ show n ++ halfTyCon ++ " " ++ spaceSep ["b" ++ show i | i <- [0..n-1]] ++ ")"
             ,"  joinShortVector (MkTuple" ++ show n ++ halfTyCon ++ " " ++ spaceSep ["a" ++ show i | i <- [0..n-1]] ++ ") (MkTuple" ++ show n ++ halfTyCon ++ " " ++ spaceSep ["b" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(joinShortVector a" ++ show i ++ " b" ++ show i ++ ")" | i <- [0..n-1]]
             ,"  {-# INLINE splitShortVector #-}"
             ,"  {-# INLINE joinShortVector #-}"
             ]
    genNewtype !name
      = ["instance SplitShortVector " ++ tyCon ++ " a => SplitShortVector " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  splitShortVector = coerce (splitShortVector @" ++ tyCon ++ " @a)"
        ,"  joinShortVector = coerce (joinShortVector @" ++ tyCon ++ " @a)"
        ,"  {-# INLINE splitShortVector #-}"
        ,"  {-# INLINE joinShortVector #-}"
        ]

genHalfReplicated :: Int -> Int -> [String]
genHalfReplicated !vecCount !baseCount
  = ["type instance HalfVector " ++ tyCon ++ " = X" ++ show (vecCount `quot` 2)
    ,"instance SplitShortVector " ++ tyCon ++ " a where"
    ,"  splitShortVector (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") = "
     ++ if n == 2
        then "(u0, u1)"
        else "(" ++ commaSep [halfDataCon ++ " " ++ spaceSep ["u" ++ show (i + j) | j <- [0..(n `quot` 2) - 1]] | i <- [0,n `quot` 2]] ++ ")"
    ,if n == 2
     then "  joinShortVector = " ++ dataCon
     else "  joinShortVector " ++ spaceSep ["(" ++ halfDataCon ++ " " ++ spaceSep ["u" ++ show (i * (n `quot` 2) + j) | j <- [0..(n `quot` 2) - 1]] ++ ")" | i <- [0,1]] ++ " = " ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]]
    ]
  where
    tyCon = 'X' : show vecCount
    baseTyCon = 'X' : show baseCount
    n = vecCount `quot` baseCount
    dataCon = "Mk" ++ tyCon ++ "With" ++ baseTyCon
    halfDataCon = "MkX" ++ show (vecCount `quot` 2) ++ "With" ++ baseTyCon

{-
genFile :: String -> Int -> [String]
genFile moduleName !maxBits
  = ["-- This file was created by script/Gen.hs. Do not edit by hand!"
    ,"{-# LANGUAGE DerivingVia #-}"
    ,"{-# LANGUAGE MagicHash #-}"
    ,"{-# LANGUAGE TypeFamilies #-}"
    ,"{-# LANGUAGE UnboxedTuples #-}"
    ,"{-# LANGUAGE UndecidableInstances #-}"
    ,"module " ++ moduleName ++ " where"
    ,"import GHC.Int"
    ,"import GHC.Word"
    ,"import Data.Monoid"
    ,"import Data.Semigroup"
    ,"import Data.Complex"
    ,"import GHC.IO"
    ,"import GHC.Exts"
    ] ++ gen 2 maxBits ++ gen 4 maxBits ++ gen 8 maxBits ++ gen 16 maxBits ++ gen 32 maxBits
-}

genFile :: String -> [String] -> Int -> Int -> [String]
genFile moduleName primModules !n !maxBits
  = ["-- This file was created by script/Gen.hs. Do not edit by hand!"
    ,"{-# LANGUAGE CPP #-}"
    ,"{-# LANGUAGE DataKinds #-}"
    ,"{-# LANGUAGE DerivingVia #-}"
    ,"{-# LANGUAGE LambdaCase #-}"
    ,"{-# LANGUAGE MagicHash #-}"
    ,"{-# LANGUAGE TypeFamilies #-}"
    ,"{-# LANGUAGE UnboxedTuples #-}"
    ,"{-# LANGUAGE UndecidableInstances #-}"
    ,"#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)"
    ,"{-# LANGUAGE ExtendedLiterals #-}"
    ,"#endif"
    ,"{-# OPTIONS_GHC -Wno-unused-imports #-}"
    ,"{-# OPTIONS_HADDOCK hide #-}"
    ,"module " ++ moduleName ++ " where"
    ,"import           Data.Bits"
    ,"import           Data.Coerce (coerce)"
    ,"import           Data.Complex"
    ,"import           Data.Monoid"
    ,"import           Data.Semigroup"
    ,"import qualified Data.Simdy.Fusible as F"
    ,"import           Data.Simdy.Internal.Bits (Boolean, BitShift)"
    ,"import           Data.Simdy.Internal.Class"
    ,"import           Data.Simdy.Internal.Shuffle"
    ,"import           Data.Type.Ord (type (<))"
    ] ++ ["import           " ++ primModule | primModule <- primModules] ++
    ["import qualified GHC.Exts"
    ,"import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..), intToInt8#, intToInt16#, intToInt32#, intToInt64#, wordToWord8#, wordToWord16#, wordToWord32#, wordToWord64#)"
    ,"import           GHC.Int"
    ,"import           GHC.IO"
    ,"import           GHC.Word"
    -- ,"import qualified Data.Vector.Unboxed.Base as VUB"
    ,"import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=), min, max)"
    ] ++ gen n maxBits

genReplicatedDef :: String -> [String] -> Int -> Int -> [String]
genReplicatedDef moduleName imports !vecCount !baseCount
  = ["-- This file was created by script/Gen.hs. Do not edit by hand!"
    ,"{-# LANGUAGE CPP #-}"
    ,"{-# LANGUAGE DataKinds #-}"
    ,"{-# LANGUAGE DerivingVia #-}"
    ,"{-# LANGUAGE MagicHash #-}"
    ,"{-# LANGUAGE TypeFamilies #-}"
    ,"{-# LANGUAGE UnboxedTuples #-}"
    ,"{-# LANGUAGE UndecidableInstances #-}"
    ,"#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)"
    ,"{-# LANGUAGE ExtendedLiterals #-}"
    ,"#endif"
    ,"{-# OPTIONS_GHC -Wno-unused-imports #-}"
    ,"{-# OPTIONS_HADDOCK hide #-}"
    ,"module " ++ moduleName ++ " where"
    ,"import           Data.Bits"
    ,"import           Data.Coerce (coerce)"
    ,"import           Data.Complex"
    ,"import           Data.Monoid"
    ,"import           Data.Primitive (Prim)"
    ,"import           Data.Semigroup"
    ,"import           Data.Simdy.Internal.Bits (Boolean, BitShift)"
    ,"import           Data.Simdy.Internal.Class"
    ] ++ ["import           " ++ m | m <- imports] ++
    ["import           Foreign.Storable (Storable)"
    ,"import qualified GHC.Exts"
    ,"import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..))"
    ,"import           GHC.Int"
    ,"import           GHC.IO"
    ,"import           GHC.Word"
    ,"import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=), min, max)"
    ,"-- | @'" ++ tyCon ++ "' a@ is a fixed-length vector of length " ++ show vecCount ++ "."
    ,"--"
    ,"-- Conceptually, @data '" ++ tyCon ++ "' a = Mk" ++ tyCon ++ concat (replicate vecCount " !a") ++ "@."
    ,"--"
    ,"-- You can access the elements by 'mk" ++ tyCon ++ "', 'pack" ++ tyCon ++ "' and 'unpack" ++ tyCon ++ "'."
    ,"data " ++ tyCon ++ " a = " ++ dataCon <+> spaceSep (replicate n ("!(" ++ baseTyCon ++ " a)"))
    ,"instance ImplementationDescription " ++ tyCon ++ " where"
    ,"  implementationDescription _ = \"" ++ tyCon ++ ";maxBits=0\""
    ,"instance KnownSIMDLength " ++ tyCon ++ " where"
    ,"  type SIMDLength " ++ tyCon ++ " = " ++ show vecCount
    ,"  simdLength = " ++ show vecCount
    ,"  {-# INLINE simdLength #-}"
    ,"type instance Mask (" ++ tyCon ++ " a) = " ++ tyCon ++ " Bool"
    ,"instance MaskIsLiftedBool " ++ tyCon ++ " a"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance EquatableF " ++ baseTyCon ++ " a => Equatable (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance OrderedF " ++ baseTyCon ++ " a => Ordered (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance SelectableF " ++ baseTyCon ++ " a => Selectable (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance (Num a, NumF " ++ baseTyCon ++ " a, Broadcast " ++ baseTyCon ++ " a, Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a) => Num (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance (Fractional a, FractionalF " ++ baseTyCon ++ " a, Broadcast " ++ baseTyCon ++ " a, Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a) => Fractional (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance (Floating a, FloatingF " ++ baseTyCon ++ " a, Broadcast " ++ baseTyCon ++ " a, Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a) => Floating (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance BooleanF " ++ tyCon ++ " a => Boolean (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance BitShiftF " ++ tyCon ++ " a => BitShift (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance MinMaxF " ++ tyCon ++ " a => MinMax (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance (Num a, FusedMultiplyAddF " ++ baseTyCon ++ " a, Broadcast " ++ baseTyCon ++ " a, Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a) => FusedMultiplyAdd (" ++ tyCon ++ " a)"
    ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " a => IsList (" ++ tyCon ++ " a) where"
    ,"  type Item (" ++ tyCon ++ " a) = a"
    ,"  toList = toList" ++ tyCon
    ,"  fromList = fromList" ++ tyCon
    ,"  {-# INLINE toList #-}"
    ,"  {-# INLINE fromList #-}"
    ,"instance Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a => Pack" ++ tyCon ++ " " ++ tyCon ++ " a where"
    ,"  mk" ++ tyCon ++ concat [" !x" ++ show i | i <- [0..vecCount-1]] ++ " = " ++ dataCon ++ " " ++ spaceSep ["(mk" ++ baseTyCon ++ " " ++ spaceSep ["x" ++ show (i * baseCount + j)| j <- [0..baseCount - 1]] ++ ")" | i <- [0..n-1]]
    ,"  unpack" ++ tyCon ++ " (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") = " ++ concat ["case unpack" ++ baseTyCon ++ " u" ++ show i ++ " of (" ++ commaSep ["x" ++ show (i * baseCount + j) | j <- [0..baseCount-1]] ++ ") -> " | i <- [0..n-1]] ++ "(" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ")"
    ,"  {-# INLINE mk" ++ tyCon ++ " #-}"
    ,"  {-# INLINE unpack" ++ tyCon ++ " #-}"
    ,"instance (Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a, Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " b) => LiftSIMD " ++ tyCon ++ " a b where"
    ,"  liftSIMD f !v = case unpack" ++ tyCon ++ " v of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> mk" ++ tyCon ++ " " ++ spaceSep ["(f x" ++ show i ++ ")" | i <- [0..vecCount-1]]
    ,"  {-# INLINE liftSIMD #-}"
    ,"instance (Pack" ++ tyCon ++ " " ++ tyCon ++ " a, Pack" ++ tyCon ++ " " ++ tyCon ++ " b, Pack" ++ tyCon ++ " " ++ tyCon ++ " c) => LiftSIMD2 " ++ tyCon ++ " a b c where"
    ,"  liftSIMD2 f !u !v = case unpack" ++ tyCon ++ " u of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> case unpack" ++ tyCon ++ " v of (" ++ commaSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") -> mk" ++ tyCon ++ " " ++ spaceSep ["(f x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
    ,"  {-# INLINE liftSIMD2 #-}"
    ,"instance LiftConstructor " ++ baseTyCon ++ " => LiftConstructor " ++ tyCon ++ " where"]
    ++ ["  mkTuple" ++ show i <+> spaceSep [parens (dataCon <+> spaceSep ["u" ++ show j ++ "_" ++ show k | k <- [0..n-1]]) | j <- [0..i-1]] ++ " = " ++ dataCon <+> spaceSep [parens ("mkTuple" ++ show i <+> spaceSep ["u" ++ show j ++ "_" ++ show k | j <- [0..i-1]]) | k <- [0..n-1]] | i <- [2..maxTupleLen]]
    ++ ["  deconstructTuple" ++ show i <+> parens (dataCon <+> spaceSep ["u" ++ show j | j <- [0..n-1]]) ++ " = " ++ concat ["case deconstructTuple" ++ show i <+> "u" ++ show j ++ " of " ++ parens (commaSep ["u" ++ show j ++ "_" ++ show k | k <- [0..i-1]]) ++ " -> "| j <- [0..n-1]] ++ parens (commaSep [dataCon <+> spaceSep ["u" ++ show j ++ "_" ++ show k | j <- [0..n-1]] | k <- [0..i-1]]) | i <- [2..maxTupleLen]]
    ++ [liftUnary name | name <- ["mkSum", "getSum'", "mkProduct", "getProduct'", "mkMin", "getMin'", "mkMax", "getMax'" {- , "mkAll", "getAll'", "mkAny", "getAny'" -}]]
    ++ [liftBinary "mkComplex"]
    ++ ["  deconstructComplex " ++ parens (dataCon <+> spaceSep ["u" ++ show j | j <- [0..n-1]]) ++ " = " ++ concat ["case deconstructComplex u" ++ show j ++ " of " ++ parens ("v" ++ show j ++ ", w" ++ show j) ++ " -> "| j <- [0..n-1]] ++ parens (dataCon <+> spaceSep ["v" ++ show j | j <- [0..n-1]] ++ ", " ++ dataCon <+> spaceSep ["w" ++ show j | j <- [0..n-1]])]
    ++ ["  {-# INLINE mkTuple" ++ show i ++ " #-}" | i <- [2..maxTupleLen]]
    ++ ["  {-# INLINE deconstructTuple" ++ show i ++ " #-}" | i <- [2..maxTupleLen]]
    ++ ["  {-# INLINE " ++ name ++ " #-}" | name <- ["mkSum", "getSum'", "mkProduct", "getProduct'", "mkMin", "getMin'", "mkMax", "getMax'" {- , "mkAll", "getAll'", "mkAny", "getAny'" -}]]
    ++ ["  {-# INLINE mkComplex #-}"]
    ++ ["  {-# INLINE deconstructComplex #-}"
    ,"instance Broadcast " ++ baseTyCon ++ " a => Broadcast " ++ tyCon ++ " a where"
    ,"  broadcast !x = let !v = broadcast x in " ++ dataCon ++ " " ++ spaceSep (replicate n "v")
    ,"  {-# INLINE broadcast #-}"
    ,"instance SelectableF " ++ baseTyCon ++ " a => SelectableF " ++ tyCon ++ " a where"
    ,"  selectF (" ++ dataCon ++ " " ++ spaceSep ["cond" ++ show i | i <- [0..n-1]] ++ ") (" ++ dataCon ++ " " ++ spaceSep ["x" ++ show i | i <- [0..n-1]] ++ ") (" ++ dataCon ++ " " ++ spaceSep ["y" ++ show i | i <- [0..n-1]] ++ ") = " ++ dataCon ++ " " ++ spaceSep ["(selectF cond" ++ show i ++ " x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..n-1]]
    ,"  {-# INLINE selectF #-}"
    ,"instance EquatableF " ++ baseTyCon ++ " a => EquatableF " ++ tyCon ++ " a where"
    ,liftBinary "eqF"
    ,"  {-# INLINE eqF #-}"
    ,"instance OrderedF " ++ baseTyCon ++ " a => OrderedF " ++ tyCon ++ " a where"
    ,liftBinary "ltF"
    ,liftBinary "leF"
    ,liftBinary "gtF"
    ,liftBinary "geF"
    ,"  {-# INLINE ltF #-}"
    ,"  {-# INLINE leF #-}"
    ,"  {-# INLINE gtF #-}"
    ,"  {-# INLINE geF #-}"
    ,"instance MinMaxF " ++ baseTyCon ++ " a => MinMaxF " ++ tyCon ++ " a where"
    ,liftBinary "minF"
    ,liftBinary "maxF"
    ,liftBinary "minimumNumberF"
    ,liftBinary "maximumNumberF"
    ,"  {-# INLINE minF #-}"
    ,"  {-# INLINE maxF #-}"
    ,"  {-# INLINE minimumNumberF #-}"
    ,"  {-# INLINE maximumNumberF #-}"
    ,"instance (Num a, NumF " ++ baseTyCon ++ " a, Broadcast " ++ baseTyCon ++ " a, Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a) => NumF " ++ tyCon ++ " a where"
    ,liftBinary "plusF"
    ,liftBinary "minusF"
    ,liftBinary "timesF"
    ,liftUnary "negateF"
    ,"  {-# INLINE plusF #-}"
    ,"  {-# INLINE minusF #-}"
    ,"  {-# INLINE timesF #-}"
    ,"  {-# INLINE negateF #-}"
    ,"instance (Fractional a, FractionalF " ++ baseTyCon ++ " a, Broadcast " ++ baseTyCon ++ " a, Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a) => FractionalF " ++ tyCon ++ " a where"
    ,liftBinary "divideF"
    ,liftUnary "recipF"
    ,"  {-# INLINE divideF #-}"
    ,"  {-# INLINE recipF #-}"
    ,"instance (Floating a, FloatingF " ++ baseTyCon ++ " a, Broadcast " ++ baseTyCon ++ " a, Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a) => FloatingF " ++ tyCon ++ " a where"
    ,liftUnary "sqrtF"
    ,"  {-# INLINE sqrtF #-}"
    ,"instance BooleanF " ++ baseTyCon ++ " a => BooleanF " ++ tyCon ++ " a where"
    ,liftBinary "andF"
    ,liftBinary "orF"
    ,liftBinary "xorF"
    ,liftUnary "complementF"
    ,"  {-# INLINE andF #-}"
    ,"  {-# INLINE orF #-}"
    ,"  {-# INLINE xorF #-}"
    ,"  {-# INLINE complementF #-}"
    ,"instance BitShiftF " ++ baseTyCon ++ " a => BitShiftF " ++ tyCon ++ " a where"
    ,"  shiftLF (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") !i = " ++ dataCon ++ " " ++ spaceSep ["(shiftLF u" ++ show i ++ " i)" | i <- [0..n-1]]
    ,"  unsafeShiftLF (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") !i = " ++ dataCon ++ " " ++ spaceSep ["(unsafeShiftLF u" ++ show i ++ " i)" | i <- [0..n-1]]
    ,"  shiftRF (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") !i = " ++ dataCon ++ " " ++ spaceSep ["(shiftRF u" ++ show i ++ " i)" | i <- [0..n-1]]
    ,"  unsafeShiftRF (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") !i = " ++ dataCon ++ " " ++ spaceSep ["(unsafeShiftRF u" ++ show i ++ " i)" | i <- [0..n-1]]
    ,"  {-# INLINE shiftLF #-}"
    ,"  {-# INLINE unsafeShiftLF #-}"
    ,"  {-# INLINE shiftRF #-}"
    ,"  {-# INLINE unsafeShiftRF #-}"
    ,"instance (Num a, FusedMultiplyAddF " ++ baseTyCon ++ " a, Broadcast " ++ baseTyCon ++ " a, Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a) => FusedMultiplyAddF " ++ tyCon ++ " a where"
    ,"  fusedMultiplyAddF (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") (" ++ dataCon ++ " " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") (" ++ dataCon ++ " " ++ spaceSep ["w" ++ show i | i <- [0..n-1]] ++ ") = " ++ dataCon ++ " " ++ spaceSep ["(fusedMultiplyAddF u" ++ show i ++ " v" ++ show i ++ " w" ++ show i ++ ")" | i <- [0..n-1]]
    ,"  {-# INLINE fusedMultiplyAddF #-}"
    ,"instance (Num a, Pack" ++ baseTyCon ++ " " ++ baseTyCon ++ " a) => EnumFromZero_ " ++ tyCon ++ " a where"
    ,"  enumFromZero = mk" ++ tyCon ++ " " ++ spaceSep [show i | i <- [0..vecCount-1]]
    ,"  {-# INLINE enumFromZero #-}"
    ,"instance (Prim a, MultiPrim " ++ baseTyCon ++ " a) => MultiPrim " ++ tyCon ++ " a where"
    ,"  indexByteArraySIMD# ba i = " ++ dataCon ++ " " ++ spaceSep ["(indexByteArraySIMD# ba " ++ i_plus (i * baseCount) ++ ")" | i <- [0..n-1]]
    ,"  readByteArraySIMD# mba i s0 = " ++ concat ["case readByteArraySIMD# mba " ++ i_plus (i * baseCount) ++ " s" ++ show i ++ " of (# s" ++ show (i + 1) ++ ", u" ++ show i ++ " #) -> " | i <- [0..n-1]] ++ "(# s" ++ show n ++ ", " ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ " #)"
    ,"  writeByteArraySIMD# mba i (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") s0 = " ++ concat ["case writeByteArraySIMD# mba " ++ i_plus i ++ " u" ++ show i ++ " s" ++ show i ++ " of s" ++ show (i + 1) ++ " -> " | i <- [0..n-2]] ++ "writeByteArraySIMD# mba (i +# " ++ show (n - 1) ++ "#) u" ++ show (n - 1) ++ " s" ++ show (n - 1)
    ,"  {-# INLINE indexByteArraySIMD# #-}"
    ,"  {-# INLINE readByteArraySIMD# #-}"
    ,"  {-# INLINE writeByteArraySIMD# #-}"
    ,"instance (Storable a, MultiStorable " ++ baseTyCon ++ " a) => MultiStorable " ++ tyCon ++ " a where"
    ,"  peekElemOffSIMD !ptr !i = " ++ dataCon ++ " <$> peekElemOffSIMD ptr i" ++ concat [" <*> peekElemOffSIMD ptr (i + " ++ show k ++ ")" | k <- [1..n-1]]
    ,"  pokeElemOffSIMD !ptr !i (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") = pokeElemOffSIMD ptr i u0" ++ concat [" >> pokeElemOffSIMD ptr (i + " ++ show k ++ ") u" ++ show k | k <- [1..n-1]]
    ,"  {-# INLINE peekElemOffSIMD #-}"
    ,"  {-# INLINE pokeElemOffSIMD #-}"
    ]
  where
    tyCon = 'X' : show vecCount
    gtyCon = 'X' : show vecCount ++ "G"
    baseTyCon = 'X' : show baseCount
    n = vecCount `quot` baseCount
    dataCon = "Mk" ++ tyCon ++ "With" ++ baseTyCon
    gdataCon = "Mk" ++ gtyCon
    liftUnary f = "  " ++ f ++ " (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") = " ++ dataCon ++ " " ++ spaceSep ["(" ++ f ++ " u" ++ show i ++ ")" | i <- [0..n-1]]
    liftBinary f = "  " ++ f ++ " (" ++ dataCon ++ " " ++ spaceSep ["u" ++ show i | i <- [0..n-1]] ++ ") (" ++ dataCon ++ " " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") = " ++ dataCon ++ " " ++ spaceSep ["(" ++ f ++ " u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..n-1]]
    i_plus 0 = "i"
    i_plus k = "(i +# " ++ show k ++ "#)"

genHalfFile :: String -> [String] -> [Int] -> [Int] -> Int -> [String]
genHalfFile moduleName imports counts repCounts !maxBits
  = ["-- This file was created by script/Gen.hs. Do not edit by hand!"
    ] ++ ["{-# LANGUAGE CPP #-}" | maxBits == 128] ++
    ["{-# LANGUAGE TypeFamilies #-}"
    ,"{-# OPTIONS_GHC -Wno-orphans #-}"
    ,"{-# OPTIONS_HADDOCK hide #-}"
    ,"module " ++ moduleName ++ " where"
    ,"import           Data.Bits"
    ,"import           Data.Complex"
    ,"import           Data.Monoid"
    ,"import           Data.Semigroup"
    ,"import           Data.Simdy.Internal.Class"
    ,"import           GHC.Exts"
    ,"import           GHC.Int"
    ,"import           GHC.Word"
    ] ++ ["import           " ++ mod | mod <- imports]
      ++ concat [genHalf n maxBits | n <- counts]
      ++ concat [genHalfReplicated n (maximum counts) | n <- repCounts]

main :: IO ()
main = do
  createDirectoryIfMissing True "src-common/Data/Simdy/Internal/Class"
  writeFile "src-common/Data/Simdy/Internal/Class/Generated.hs" $ unlines $
    ["-- This file was created by script/Gen.hs. Do not edit by hand!"
    ,"{-# LANGUAGE PatternSynonyms #-}"
    ,"{-# LANGUAGE ViewPatterns #-}"
    ,"{-# OPTIONS_HADDOCK hide #-}"
    ,"module Data.Simdy.Internal.Class.Generated where"]
    ++ concatMap (\n -> ["-- | Pack\\/unpack " ++ show n ++ "-lane SIMD vectors from\\/to individual scalar elements."
                        ,"class PackX" ++ show n ++ " f a where"
                        ,"  mkX" ++ show n ++ " :: " ++ concat (replicate n "a -> ") ++ "f a"
                        ,"  unpackX" ++ show n ++ " :: f a -> (" ++ commaSep (replicate n "a") ++ ")"
                        ,"pattern MkX" ++ show n ++ " :: PackX" ++ show n ++ " x a => " ++ concat (replicate n "a -> ") ++ "x a"
                        ,"pattern MkX" ++ show n ++ concat [" x" ++ show i | i <- [0..n-1]] ++ " <- (unpackX" ++ show n ++ " -> " ++ parens (commaSep ["x" ++ show i | i <- [0..n-1]]) ++ ") where"
                        ,"  MkX" ++ show n ++ " = mkX" ++ show n
                        ,"packX" ++ show n ++ " :: PackX" ++ show n ++ " x a => (" ++ commaSep (replicate n "a") ++ ") -> x a"
                        ,"packX" ++ show n ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") = mkX" ++ show n ++ concat [" a" ++ show i | i <- [0..n-1]]
                        ,"toListX" ++ show n ++ " :: PackX" ++ show n ++ " x a => x a -> [a]"
                        ,"toListX" ++ show n ++ " v = case unpackX" ++ show n ++ " v of"
                        ,"  (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") -> [" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ "]"
                        ,"fromListX" ++ show n ++ " :: PackX" ++ show n ++ " x a => [a] -> x a"
                        ,"fromListX" ++ show n ++ " [" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ "] = mkX" ++ show n ++ concat [" a" ++ show i | i <- [0..n-1]]
                        ,"fromListX" ++ show n ++ " xs | length xs < " ++ show n ++ " = error \"fromListX" ++ show n ++ ": List too short\""
                        ,"         " ++ map (const ' ') (show n) ++ "    | otherwise = error \"fromListX" ++ show n ++ ": List too long\""
                        ]) [2,4,8,16,32,64]
    {-
    ++ ["class MkTuple f where"]
    ++ ["  mkTuple" ++ show n ++ " :: " ++ concat ["f a" ++ show i ++ " -> " | i <- [0..n-1]] ++ "f (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ")" | n <- [2..maxTupleLen]]
    ++ ["  deconstructTuple" ++ show n ++ " :: " ++ "f (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") -> (" ++ commaSep ["f a" ++ show i | i <- [0..n-1]] ++ ")" | n <- [2..maxTupleLen]]
    ++ concat [["pattern MkTuple" ++ show n ++ " :: MkTuple f => " ++ concat ["f a" ++ show i ++ " -> " | i <- [0..n-1]] ++ "f (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ")"
               ,"pattern MkTuple" ++ show n ++ " " ++ spaceSep ["x" ++ show i | i <- [0..n-1]] ++ " <- (deconstructTuple" ++ show n ++ " -> (" ++ commaSep ["x" ++ show i | i <- [0..n-1]] ++ ")) where"
               ,"  MkTuple" ++ show n ++ " = mkTuple" ++ show n
               ] | n <- [2..maxTupleLen]]
    -}
  {-
  writeFile "src/Data/Simdy/Internal/NoSIMD.hs" $ unlines $ genFile "Data.Simdy.Internal.NoSIMD" 0
  writeFile "src/Data/Simdy/Internal/SIMD128.hs" $ unlines $ genFile "Data.Simdy.Internal.SIMD128" 128
  writeFile "src/Data/Simdy/Internal/SIMD256.hs" $ unlines $ genFile "Data.Simdy.Internal.SIMD256" 256
  writeFile "src/Data/Simdy/Internal/SIMD512.hs" $ unlines $ genFile "Data.Simdy.Internal.SIMD512" 512
  -}
  createDirectoryIfMissing True "src-no-simd/Data/Simdy/Internal/NoSIMD"
  createDirectoryIfMissing True "src-vl128/Data/Simdy/Internal/SIMD128"
  createDirectoryIfMissing True "src-vl256/Data/Simdy/Internal/SIMD256"
  createDirectoryIfMissing True "src-vl512/Data/Simdy/Internal/SIMD512"
  forM_ [2,4,8,16,32,64] $ \i -> do
    writeFile ("src-no-simd/Data/Simdy/Internal/NoSIMD/X" ++ show i ++ ".hs") $ unlines $
      if i <= 8 then
        genFile ("Data.Simdy.Internal.NoSIMD.X" ++ show i) [] i 0
      else
        genReplicatedDef ("Data.Simdy.Internal.NoSIMD.X" ++ show i) ["Data.Simdy.Internal.NoSIMD.X8"] i 8
    writeFile ("src-vl128/Data/Simdy/Internal/SIMD128/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.SIMD128.X" ++ show i) ["Data.Simdy.Internal.SIMD128.Prim", "Data.Simdy.Internal.SIMD128.PrimExtra"] i 128
    when (i * 64 > 128) $ writeFile ("src-vl256/Data/Simdy/Internal/SIMD256/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.SIMD256.X" ++ show i) ["Data.Simdy.Internal.SIMD256.Prim", "Data.Simdy.Internal.SIMD128.PrimExtra", "Data.Simdy.Internal.SIMD256.PrimExtra"] i 256
    when (i * 64 > 256) $ writeFile ("src-vl512/Data/Simdy/Internal/SIMD512/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.SIMD512.X" ++ show i) ["Data.Simdy.Internal.SIMD512.Prim", "Data.Simdy.Internal.SIMD128.PrimExtra", "Data.Simdy.Internal.SIMD256.PrimExtra", "Data.Simdy.Internal.SIMD512.PrimExtra"] i 512
  writeFile "src-no-simd/Data/Simdy/Internal/NoSIMD/HalfVector.hs" $ unlines $ genHalfFile "Data.Simdy.Internal.NoSIMD.HalfVector"
    ["Data.Functor.Identity"
    ,"Data.Simdy.Internal.NoSIMD.X2"
    ,"Data.Simdy.Internal.NoSIMD.X4"
    ,"Data.Simdy.Internal.NoSIMD.X8"
    ,"Data.Simdy.Internal.NoSIMD.X16"
    ,"Data.Simdy.Internal.NoSIMD.X32"
    ,"Data.Simdy.Internal.NoSIMD.X64"
    ] [2,4,8] [16,32,64] 0
  writeFile "src-vl128/Data/Simdy/Internal/SIMD128/HalfVector.hs" $ unlines $ genHalfFile "Data.Simdy.Internal.SIMD128.HalfVector"
    ["Data.Functor.Identity"
    ,"Data.Simdy.Internal.SIMD128.X2"
    ,"Data.Simdy.Internal.SIMD128.X4"
    ,"Data.Simdy.Internal.SIMD128.X8"
    ,"Data.Simdy.Internal.SIMD128.X16"
    ,"Data.Simdy.Internal.SIMD128.X32"
    ,"Data.Simdy.Internal.SIMD128.X64"
    ] [2,4,8,16,32,64] [] 128
  writeFile "src-vl256/Data/Simdy/Internal/SIMD256/HalfVector.hs" $ unlines $ genHalfFile "Data.Simdy.Internal.SIMD256.HalfVector"
    ["Data.Simdy.Internal.SIMD128 (X2 (..))"
    ,"Data.Simdy.Internal.SIMD256.X4"
    ,"Data.Simdy.Internal.SIMD256.X8"
    ,"Data.Simdy.Internal.SIMD256.X16"
    ,"Data.Simdy.Internal.SIMD256.X32"
    ,"Data.Simdy.Internal.SIMD256.X64"
    ] [4,8,16,32,64] [] 256
  writeFile "src-vl512/Data/Simdy/Internal/SIMD512/HalfVector.hs" $ unlines $ genHalfFile "Data.Simdy.Internal.SIMD512.HalfVector"
    ["Data.Simdy.Internal.SIMD256 (X4 (..))"
    ,"Data.Simdy.Internal.SIMD512.X8"
    ,"Data.Simdy.Internal.SIMD512.X16"
    ,"Data.Simdy.Internal.SIMD512.X32"
    ,"Data.Simdy.Internal.SIMD512.X64"
    ] [8,16,32,64] [] 512

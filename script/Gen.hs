-- Run as 'cabal run script/Gen.hs'
{- cabal:
build-depends: base, directory
default-language: GHC2021
-}
import System.IO
import qualified Data.List as List
import Control.Monad
import System.Directory (createDirectoryIfMissing)

maxTupleLen :: Int
maxTupleLen = 6

maxTupleLenForUnboxedVector :: Int
maxTupleLenForUnboxedVector = 6

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
    ,"-- Conceptually, @data '" ++ tyCon ++ "' a = Pack" ++ tyCon ++ concat (replicate vecCount " !a") ++ "@."
    ,"--"
    ,"-- You can access the elements by 'pack" ++ tyCon ++ "' and 'unpack" ++ tyCon ++ "'."
    ,"data family " ++ tyCon ++ " a"
    ,"instance KnownSIMDLength " ++ tyCon ++ " where"
    ,"  type SIMDLength " ++ tyCon ++ " = " ++ show vecCount
    ,"  simdLength = " ++ show vecCount
    ,"  {-# INLINE simdLength #-}"
    ,"newtype instance " ++ tyCon ++ " Bool = MkBool" ++ tyCon ++ " Word" ++ show (max vecCount 8)
    ,"type instance Mask (" ++ tyCon ++ " a) = " ++ tyCon ++ " Bool"
    ,"instance MaskIsLiftedBool " ++ tyCon ++ " a"
    ,"instance BooleanF " ++ tyCon ++ " where"
    ,"  trueF = MkBool" ++ tyCon ++ " " ++ show (2^vecCount - 1)
    ,"  falseF = MkBool" ++ tyCon ++ " 0"
    ,"  notF (MkBool" ++ tyCon ++ " x) = MkBool" ++ tyCon ++ " (" ++ show (2^vecCount - 1) ++ " - x)"
    ,"  landF (MkBool" ++ tyCon ++ " x) (MkBool" ++ tyCon ++ " y) = MkBool" ++ tyCon ++ " (x .&. y)"
    ,"  lorF (MkBool" ++ tyCon ++ " x) (MkBool" ++ tyCon ++ " y) = MkBool" ++ tyCon ++ " (x .|. y)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " Bool instance Boolean (" ++ tyCon ++ " Bool)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance EquatableF " ++ tyCon ++ " a => Equatable (" ++ tyCon ++ " a)"
    ,"deriving via WrappedMulti " ++ tyCon ++ " a instance OrderedF " ++ tyCon ++ " a => Ordered (" ++ tyCon ++ " a)"
    ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " a => IsList (" ++ tyCon ++ " a) where"
    ,"  type Item (" ++ tyCon ++ " a) = a"
    ,"  toList = toList" ++ tyCon
    ,"  fromList = fromList" ++ tyCon
    ,"  {-# INLINE toList #-}"
    ,"  {-# INLINE fromList #-}"
    ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " Bool where"
    ,"  pack" ++ tyCon ++ concat [" !x" ++ show i | i <- [0..vecCount-1]] ++ " = MkBool" ++ tyCon ++ " (" ++ List.intercalate " .|. " ["(if x" ++ show i ++ " then " ++ show (2^i) ++ " else 0)" | i <- [0..vecCount-1]] ++ ")"
    ,"  unpack" ++ tyCon ++ " (MkBool" ++ tyCon ++ " !x) = (" ++ List.intercalate ", " ["testBit x " ++ show i | i <- [0..vecCount-1]] ++ ")"
    ,"instance Broadcast " ++ tyCon ++ " Bool where"
    ,"  broadcast False = falseF"
    ,"  broadcast True = trueF"
    ,"  {-# INLINE broadcast #-}"
    ,"instance SelectableF " ++ tyCon ++ " Bool where"
    ,"  selectF (MkBool" ++ tyCon ++ " !cond) (MkBool" ++ tyCon ++ " !x) (MkBool" ++ tyCon ++ " !y) = MkBool" ++ tyCon ++ " ((cond .&. x) .|. (complement cond .&. y))"
    ,"  {-# INLINE selectF #-}"
    ]
    ++ genType "Float" "F#" 32 maxBits [genEquatable, genOrderedFloat, genNum True, genFractional, genFloating, genEnumFromZero ".0#", genPrim, genStorable]
    ++ genType "Double" "D#" 64 maxBits [genEquatable, genOrderedFloat, genNum True, genFractional, genFloating, genEnumFromZero ".0##", genPrim, genStorable]
    ++ ["#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)" | maxBits == 128]
    ++ genType "Int8" "I8#" 8 maxBits [genEquatable, genOrderedInt, genNum True, genBits, genEnumFromZero "#Int8", genPrim, genStorable]
    ++ genType "Int16" "I16#" 16 maxBits [genEquatable, genOrderedInt, genNum True, genBits, genEnumFromZero "#Int16", genPrim, genStorable]
    ++ genType "Int32" "I32#" 32 maxBits [genEquatable, genOrderedInt, genNum True, genBits, genEnumFromZero "#Int32", genPrim, genStorable]
    ++ genType "Int64" "I64#" 64 maxBits [genEquatable, genOrderedInt, genNum True, genBits, genEnumFromZero "#Int64", genPrim, genStorable]
    ++ genType "Word8" "W8#" 8 maxBits [genEquatable, genOrderedInt, genNum False, genBits, genEnumFromZero "#Word8", genPrim, genStorable]
    ++ genType "Word16" "W16#" 16 maxBits [genEquatable, genOrderedInt, genNum False, genBits, genEnumFromZero "#Word16", genPrim, genStorable]
    ++ genType "Word32" "W32#" 32 maxBits [genEquatable, genOrderedInt, genNum False, genBits, genEnumFromZero "#Word32", genPrim, genStorable]
    ++ genType "Word64" "W64#" 64 maxBits [genEquatable, genOrderedInt, genNum False, genBits, genEnumFromZero "#Word64", genPrim, genStorable]
    ++ (if maxBits == 128
        then ["#else"
             ,"-- The NCG of GHC 9.12 does not support integer vectors"]
             ++ genType "Int8" "I8#" 8 0 [genEquatable, genOrderedInt, genNum True, genBits, genEnumFromZero "#Int8", genPrim, genStorable]
             ++ genType "Int16" "I16#" 16 0 [genEquatable, genOrderedInt, genNum True, genBits, genEnumFromZero "#Int16", genPrim, genStorable]
             ++ genType "Int32" "I32#" 32 0 [genEquatable, genOrderedInt, genNum True, genBits, genEnumFromZero "#Int32", genPrim, genStorable]
             ++ genType "Int64" "I64#" 64 0 [genEquatable, genOrderedInt, genNum True, genBits, genEnumFromZero "#Int64", genPrim, genStorable]
             ++ genType "Word8" "W8#" 8 0 [genEquatable, genOrderedInt, genNum False, genBits, genEnumFromZero "#Word8", genPrim, genStorable]
             ++ genType "Word16" "W16#" 16 0 [genEquatable, genOrderedInt, genNum False, genBits, genEnumFromZero "#Word16", genPrim, genStorable]
             ++ genType "Word32" "W32#" 32 0 [genEquatable, genOrderedInt, genNum False, genBits, genEnumFromZero "#Word32", genPrim, genStorable]
             ++ genType "Word64" "W64#" 64 0 [genEquatable, genOrderedInt, genNum False, genBits, genEnumFromZero "#Word64", genPrim, genStorable]
             ++ ["#endif"]
        else []
       )
    ++ genNewtype "Sum"
    ++ genNewtype "Product"
    ++ genNewtype "Min"
    ++ genNewtype "Max"
    ++ ["data instance " ++ tyCon ++ " (Complex a) = MkComplex" ++ tyCon ++ " !(" ++ tyCon ++ " a) !(" ++ tyCon ++ " a)"
       ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " a => Pack" ++ tyCon ++ " " ++ tyCon ++ " (Complex a) where"
       ,"  pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " :+ y" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " = MkComplex" ++ tyCon ++ " (pack" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (pack" ++ tyCon ++ " " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ")"
       ,"  unpack" ++ tyCon ++ " (MkComplex" ++ tyCon ++ " s t) = case unpack" ++ tyCon ++ " s of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> case unpack" ++ tyCon ++ " t of (" ++ commaSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") -> (" ++ commaSep ["x" ++ show i ++ " :+ y" ++ show i | i <- [0..vecCount-1]] ++ ")"
       ,"  {-# INLINE pack" ++ tyCon ++ " #-}"
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
       ,"  pack" ++ tyCon ++ " " ++ spaceSep (replicate vecCount "_") ++ " = MkUnit" ++ tyCon
       ,"  unpack" ++ tyCon ++ " MkUnit" ++ tyCon ++ " = (" ++ commaSep (replicate vecCount "()") ++ ")"
       ,"  {-# INLINE pack" ++ tyCon ++ " #-}"
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
       ,"  liftSIMD f !v = case unpack" ++ tyCon ++ " v of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> pack" ++ tyCon ++ " " ++ spaceSep ["(f x" ++ show i ++ ")" | i <- [0..vecCount-1]]
       ,"  {-# INLINE liftSIMD #-}"
       ,"instance (Pack" ++ tyCon ++ " " ++ tyCon ++ " a, Pack" ++ tyCon ++ " " ++ tyCon ++ " b, Pack" ++ tyCon ++ " " ++ tyCon ++ " c) => LiftSIMD2 " ++ tyCon ++ " a b c where"
       ,"  liftSIMD2 f !u !v = case unpack" ++ tyCon ++ " u of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> case unpack" ++ tyCon ++ " v of (" ++ commaSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") -> pack" ++ tyCon ++ " " ++ spaceSep ["(f x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
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
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance BitsF " ++ tyCon ++ " a => MiniBits (" ++ tyCon ++ " a)"]
  where
    tyCon = 'X' : show vecCount
    genType name primCon !bitsPerElem maxBits others
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
            halfTyCon = if vecCount == 2 then "Identity" else "X" ++ show (vecCount `quot` 2)
            mainDef = if bitCount < 128 || maxBits == 0
                      then ["data instance " ++ tyCon ++ " " ++ name ++ " = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep (replicate vecCount ('!':name))
                           ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  pack" ++ tyCon ++ " = Mk" ++ name ++ tyCon ++ "WithElems"
                           ,"  unpack" ++ tyCon ++ " (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ")"
                           ,"  {-# INLINE pack" ++ tyCon ++ " #-}"
                           ,"  {-# INLINE unpack" ++ tyCon ++ " #-}"
                           ,"instance Broadcast " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  broadcast !x = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep (replicate vecCount "x")
                           ,"  {-# INLINE broadcast #-}"
                           ,"instance SelectableF " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  selectF (MkBool" ++ tyCon ++ " !cond) (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(if testBit cond " ++ show i ++ " then x" ++ show i ++ " else y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                           ]
                      else
                        let shortVecSize = vecBitCount `div` bitsPerElem
                            shortVecCount = bitCount `div` vecBitCount
                            shortVecName = name ++ "X" ++ show shortVecSize
                            suffix | shortVecCount == 1 = ""
                                   | otherwise = "WithVec" ++ show vecBitCount
                        in ["data instance " ++ tyCon ++ " " ++ name ++ " = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep (replicate shortVecCount (shortVecName ++ "#"))
                           ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  pack" ++ tyCon ++ " " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(pack" ++ shortVecName ++ "# (# " ++ commaSep ["x" ++ show (i * shortVecSize + j) | j <- [0..shortVecSize - 1]] ++ " #))" | i <- [0..shortVecCount - 1]]
                           ,"  unpack" ++ tyCon ++ " (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = " ++ concat ["case unpack" ++ shortVecName ++ "# v" ++ show i ++ " of (# " ++ commaSep ["x" ++ show (i * shortVecSize + j) | j <- [0..shortVecSize - 1]] ++ " #) -> " | i <- [0..shortVecCount - 1]] ++ "(" ++ commaSep [primCon ++ " x" ++ show i | i <- [0..vecCount-1]] ++ ")"
                           ,"  {-# INLINE pack" ++ tyCon ++ " #-}"
                           ,"  {-# INLINE unpack" ++ tyCon ++ " #-}"
                           ,"instance Broadcast " ++ tyCon ++ " " ++ name ++ " where"
                           ,if shortVecCount == 1
                            then "  broadcast (" ++ primCon ++ " x) = Mk" ++ name ++ tyCon ++ suffix ++ " (broadcast" ++ name ++ "X" ++ show shortVecSize ++ "# x)"
                            else "  broadcast (" ++ primCon ++ " x) = let !v = broadcast" ++ name ++ "X" ++ show shortVecSize ++ "# x in Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep (replicate shortVecCount "v")
                           ,"  {-# INLINE broadcast #-}"
                           ,"instance SelectableF " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  selectF (MkBool" ++ tyCon ++ " !cond) (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["x" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["y" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(select" ++ shortVecName ++ "# " ++ cond_i ++ " x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..shortVecCount-1], let cond_i = if shortVecCount == 1 then "cond" else "(" ++ (if max 8 shortVecSize == max 8 vecCount then "" else "fromIntegral $ ") ++ "cond `unsafeShiftR` " ++ show (i * shortVecSize) ++ ")" ]
                           ,"  {-# INLINE selectF #-}"
                           ]
        in mainDef ++ concatMap (\f -> f name primCon bitsPerElem maxBits) others
    genEquatable name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["instance EquatableF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  eqF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " == y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE eqF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance EquatableF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  eqF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (eq" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize)]
                ,"  {-# INLINE eqF #-}"
                ]
    genOrderedInt name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["instance OrderedF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  ltF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " < y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  leF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " <= y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  gtF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " > y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  geF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " >= y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE ltF #-}"
                ,"  {-# INLINE leF #-}"
                ,"  {-# INLINE gtF #-}"
                ,"  {-# INLINE geF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance OrderedF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  ltF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (lt" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize)]
                ,"  leF !x !y = notF (ltF y x)"
                ,"  gtF !x !y = ltF x y"
                ,"  geF !x !y = notF (ltF x y)"
                ,"  {-# INLINE ltF #-}"
                ,"  {-# INLINE leF #-}"
                ,"  {-# INLINE gtF #-}"
                ,"  {-# INLINE geF #-}"
                ]
    genOrderedFloat name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["instance OrderedF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  ltF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " < y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  leF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " <= y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  gtF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " > y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  geF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " >= y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE ltF #-}"
                ,"  {-# INLINE leF #-}"
                ,"  {-# INLINE gtF #-}"
                ,"  {-# INLINE geF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance OrderedF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  ltF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (lt" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize)]
                ,"  leF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (le" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize)]
                ,"  gtF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (gt" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize)]
                ,"  geF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = MkBool" ++ tyCon ++ " $ " ++ List.intercalate " .|. " ["(fromIntegral (ge" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" ++ shift ++ ")" | i <- [0..shortVecCount-1], let shift = if i == 0 then "" else " `unsafeShiftL` " ++ show (i * shortVecSize)]
                ,"  {-# INLINE ltF #-}"
                ,"  {-# INLINE leF #-}"
                ,"  {-# INLINE gtF #-}"
                ,"  {-# INLINE geF #-}"
                ]
    genNum isSigned name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["instance NumF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  plusF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " + y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  minusF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " - y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  timesF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " * y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  negateF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(- x" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE plusF #-}"
                ,"  {-# INLINE minusF #-}"
                ,"  {-# INLINE timesF #-}"
                ,"  {-# INLINE negateF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance NumF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  plusF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(plus" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  minusF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(minus" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  timesF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(times" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ]
                ++ if isSigned
                   then ["  negateF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(negate" ++ shortVecName ++ "# u" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                        ,"  -- Currently, there is no abs" ++ shortVecName ++ "#"
                        ]
                   else ["  -- Currently, there is no negate" ++ shortVecName ++ "#, abs" ++ shortVecName ++ "#"
                        ]
                ++ ["  {-# INLINE plusF #-}"
                   ,"  {-# INLINE minusF #-}"
                   ,"  {-# INLINE timesF #-}"
                   ]
                ++ ["  {-# INLINE negateF #-}" | isSigned]
    genFractional name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["instance FractionalF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  divF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " / y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  recipF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(recip x" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE divF #-}"
                ,"  {-# INLINE recipF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance FractionalF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  divF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(divide" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  {-# INLINE divF #-}"
                ]
    genFloating name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["instance FloatingF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  sqrtF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(sqrt x" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  {-# INLINE sqrtF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance FloatingF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  -- sqrtF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(sqrt" ++ shortVecName ++ "# u" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  -- Currently. there is no sqrt" ++ shortVecName ++ "#"
                ]
    genBits name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["instance BitsF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  andF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " .&. y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  orF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " .|. y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  xorF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(xor x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  complementF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(complement x" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  shiftLF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") !i = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(shiftL x" ++ show i ++ " i)" | i <- [0..vecCount-1]]
                ,"  unsafeShiftLF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") !i = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(unsafeShiftL x" ++ show i ++ " i)" | i <- [0..vecCount-1]]
                ,"  shiftRF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") !i = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(shiftR x" ++ show i ++ " i)" | i <- [0..vecCount-1]]
                ,"  unsafeShiftRF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") !i = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(unsafeShiftR x" ++ show i ++ " i)" | i <- [0..vecCount-1]]
                ,"  {-# INLINE andF #-}"
                ,"  {-# INLINE orF #-}"
                ,"  {-# INLINE xorF #-}"
                ,"  {-# INLINE complementF #-}"
                ,"  {-# INLINE shiftLF #-}"
                ,"  {-# INLINE unsafeShiftLF #-}"
                ,"  {-# INLINE shiftRF #-}"
                ,"  {-# INLINE unsafeShiftRF #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance BitsF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  andF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(and" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  orF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(or" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  xorF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(xor" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  complementF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(complement" ++ shortVecName ++ "# u" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  shiftLF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (I# i) = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(shiftL" ++ shortVecName ++ "# u" ++ show i ++ " i)" | i <- [0..shortVecCount-1]]
                ,"  shiftRF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (I# i) = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(shiftR" ++ shortVecName ++ "# u" ++ show i ++ " i)" | i <- [0..shortVecCount-1]]
                ,"  {-# INLINE andF #-}"
                ,"  {-# INLINE orF #-}"
                ,"  {-# INLINE xorF #-}"
                ,"  {-# INLINE complementF #-}"
                ,"  {-# INLINE shiftLF #-}"
                ,"  {-# INLINE shiftRF #-}"
                ]
    genEnumFromZero litSuffix name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
                      then ["instance EnumFromZero_ " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  enumFromZero = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep [show i | i <- [0..vecCount-1]]
                           ,"  -- {-# INLINE enumFromZero #-}"
                           ]
                      else
                        let shortVecSize = vecBitCount `div` bitsPerElem
                            shortVecCount = bitCount `div` vecBitCount
                            shortVecName = name ++ "X" ++ show shortVecSize
                            suffix | shortVecCount == 1 = ""
                                   | otherwise = "WithVec" ++ show vecBitCount
                        in ["instance EnumFromZero_ " ++ tyCon ++ " " ++ name ++ " where"
                           ,"#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)"
                           ,"  enumFromZero = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(pack" ++ shortVecName ++ "# (# " ++ commaSep [show (i * shortVecSize + j) ++ litSuffix | j <- [0..shortVecSize - 1]] ++ " #))" | i <- [0..shortVecCount - 1]]
                           ,"#else"
                           ,"  enumFromZero = pack" ++ tyCon ++ " " ++ spaceSep [show i | i <- [0..vecCount-1]]
                           ,"#endif"
                           ,"  -- {-# INLINE enumFromZero #-}"
                           ]
    genPrim name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
            i_plus 0 = "i"
            i_plus k = "(i +# " ++ show k ++ "#)"
        in if bitCount < 128 || maxBits == 0
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
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance MultiPrim " ++ tyCon ++ " " ++ name ++ " where"
                ,"  indexByteArraySIMD# ba i = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(index" ++ name ++ "ArrayAs" ++ shortVecName ++ "# ba " ++ i_plus (i * shortVecSize) ++ ")" | i <- [0..shortVecCount-1]]
                ,"  readByteArraySIMD# mba i s0 = " ++ concat ["case read" ++ name ++ "ArrayAs" ++ shortVecName ++ "# mba " ++ i_plus (i * shortVecSize) ++ " s" ++ show i ++ " of (# s" ++ show (i + 1) ++ ", v" ++ show i ++ " #) -> " | i <- [0..shortVecCount-1]] ++ "(# s" ++ show shortVecCount ++ ", Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ " #)"
                ,"  writeByteArraySIMD# mba i (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") s0 = " ++ concat ["case write" ++ name ++ "ArrayAs" ++ shortVecName ++ "# mba " ++ i_plus (i * shortVecSize) ++ " v" ++ show i ++ " s" ++ show i ++ " of s" ++ show (i + 1) ++ " -> " | i <- [0..shortVecCount-2]] ++ "write" ++ name ++ "ArrayAs" ++ shortVecName ++ "# mba " ++ i_plus ((shortVecCount - 1) * shortVecSize) ++ " v" ++ show (shortVecCount - 1) ++ " s" ++ show (shortVecCount - 1)
                ,"  {-# INLINE indexByteArraySIMD# #-}"
                ,"  {-# INLINE readByteArraySIMD# #-}"
                ,"  {-# INLINE writeByteArraySIMD# #-}"
                ]
    genStorable name primCon !bitsPerElem maxBits
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
            i_plus 0 = "i"
            i_plus k = "(i +# " ++ show k ++ "#)"
        in if bitCount < 128 || maxBits == 0
           then ["instance MultiStorable " ++ tyCon ++ " " ++ name ++ " where"
                ,"  peekElemOffSIMD (Ptr addr) (I# i) = IO (\\s0 -> " ++ concat ["case GHC.Exts.read" ++ name ++ "OffAddr# addr " ++ i_plus i ++ " s" ++ show i ++ " of (# s" ++ show (i + 1) ++ ", x" ++ show i ++ " #) -> " | i <- [0..vecCount-1]] ++ "(# s" ++ show vecCount ++ ", Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " #))"
                ,"  pokeElemOffSIMD (Ptr addr) (I# i) (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ ") = IO (\\s0 -> " ++ concat ["case GHC.Exts.write" ++ name ++ "OffAddr# addr " ++ i_plus i ++ " x" ++ show i ++ " s" ++ show i ++ " of s" ++ show (i + 1) ++ " -> " | i <- [0..vecCount-2]] ++ "(# GHC.Exts.write" ++ name ++ "OffAddr# addr (i +# " ++ show (vecCount - 1) ++ "#) x" ++ show (vecCount - 1) ++ " s" ++ show (vecCount - 1) ++ ", () #))"
                ,"  {-# INLINE peekElemOffSIMD #-}"
                ,"  {-# INLINE pokeElemOffSIMD #-}"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance MultiStorable " ++ tyCon ++ " " ++ name ++ " where"
                ,"  peekElemOffSIMD (Ptr addr) (I# i) = IO (\\s0 -> " ++ concat ["case read" ++ name ++ "OffAddrAs" ++ shortVecName ++ "# addr " ++ i_plus (i * shortVecSize) ++ " s" ++ show i ++ " of (# s" ++ show (i + 1) ++ ", v" ++ show i ++ " #) -> " | i <- [0..shortVecCount-1]] ++ "(# s" ++ show shortVecCount ++ ", Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ " #))"
                ,"  pokeElemOffSIMD (Ptr addr) (I# i) (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = IO (\\s0 -> " ++ concat ["case write" ++ name ++ "OffAddrAs" ++ shortVecName ++ "# addr " ++ i_plus (i * shortVecSize) ++ " v" ++ show i ++ " s" ++ show i ++ " of s" ++ show (i + 1) ++ " -> " | i <- [0..shortVecCount-2]] ++ "(# write" ++ name ++ "OffAddrAs" ++ shortVecName ++ "# addr " ++ i_plus ((shortVecCount - 1) * shortVecSize) ++ " v" ++ show (shortVecCount - 1) ++ " s" ++ show (shortVecCount - 1) ++ ", () #))"
                ,"  {-# INLINE peekElemOffSIMD #-}"
                ,"  {-# INLINE pokeElemOffSIMD #-}"
                ]
    genTuple !n
      = ["data instance " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["!(" ++ tyCon ++ " a" ++ show i ++ ")" | i <- [0..n-1]]
        ,"instance (" ++ commaSep ["Pack" ++ tyCon ++ " " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => Pack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
        ,"  pack" ++ tyCon ++ " " ++ spaceSep ["(" ++ commaSep ["x" ++ show i ++ "_" ++ show j | j <- [0..n-1]] ++ ")" | i <- [0..vecCount-1]] ++ " = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(pack" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i ++ "_" ++ show j | i <- [0..vecCount-1]] ++ ")" | j <- [0..n-1]]
        ,"  unpack" ++ tyCon ++ " (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") = " ++ concat ["case unpack" ++ tyCon ++ " v" ++ show i ++ " of (" ++ commaSep ["x" ++ show j ++ "_" ++ show i | j <- [0..vecCount-1]] ++ ") -> " | i <- [0..n-1]] ++ "(" ++ commaSep ["(" ++ commaSep ["x" ++ show i ++ "_" ++ show j | j <- [0..n-1]] ++ ")" | i <- [0..vecCount-1]] ++ ")"
        ,"  {-# INLINE pack" ++ tyCon ++ " #-}"
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
        ,"  pack" ++ tyCon ++ " = coerce (pack" ++ tyCon ++ " @" ++ tyCon ++ " @a)"
        ,"  unpack" ++ tyCon ++ " = coerce (unpack" ++ tyCon ++ " @" ++ tyCon ++ " @a)"
        ,"  {-# INLINE pack" ++ tyCon ++ " #-}"
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
     else "  splitShortVector (MkBool" ++ tyCon ++ " !x) = (MkBoolX" ++ show (vecCount `quot` 2) ++ " $ fromIntegral $ x .&. " ++ show (2^(vecCount `quot` 2)) ++ ", MkBoolX" ++ show (vecCount `quot` 2) ++ " $ fromIntegral $ x `unsafeShiftR` " ++ show (vecCount `quot` 2) ++ ")"
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
            vecBitCount = min bitCount maxBits
            halfTyCon = if vecCount == 2 then "Identity" else "X" ++ show (vecCount `quot` 2)
        in if bitCount < 128 || maxBits == 0
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
                     ,"  joinShortVector (Identity x0) (Identity x1) = pack" ++ tyCon ++ " x0 x1"
                     ,"  {-# INLINE splitShortVector #-}"
                     ,"  {-# INLINE joinShortVector #-}"
                     ]
                else
                  let halfVecBitCount = min (bitsPerElem * vecCount `div` 2) maxBits
                  in if halfVecBitCount < 128
                     then ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                          ,"  splitShortVector v = case unpack" ++ tyCon ++ " v of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ", Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ")"
                          ,"  joinShortVector (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ") (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]]
                          ,"  {-# INLINE splitShortVector #-}"
                          ,"  {-# INLINE joinShortVector #-}"
                          ]
                     else
                       if shortVecCount == 1
                       then
                         ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                         ,"  splitShortVector v = case unpack" ++ tyCon ++ " v of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> (pack" ++ halfTyCon ++ " " ++ spaceSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ", pack" ++ halfTyCon ++ " " ++ spaceSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ")"
                         ,"  joinShortVector v0 v1 = case unpack" ++ halfTyCon ++ " v0 of (" ++ commaSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ") -> case unpack" ++ halfTyCon ++ " v1 of (" ++ commaSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ") -> pack" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]]
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
    ,"{-# LANGUAGE MagicHash #-}"
    ,"{-# LANGUAGE TypeFamilies #-}"
    ,"{-# LANGUAGE UnboxedTuples #-}"
    ,"{-# LANGUAGE UndecidableInstances #-}"
    ,"#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)"
    ,"{-# LANGUAGE ExtendedLiterals #-}"
    ,"#endif"
    ,"{-# OPTIONS_GHC -Wno-unused-imports #-}"
    ,"module " ++ moduleName ++ " where"
    ,"import           Data.Bits"
    ,"import           Data.Coerce (coerce)"
    ,"import           Data.Complex"
    ,"import           Data.Monoid"
    ,"import           Data.Semigroup"
    ,"import           Data.Simdy.Class.Bits (MiniBits)"
    ,"import           Data.Simdy.Internal.Class"
    ] ++ ["import           " ++ primModule | primModule <- primModules] ++
    ["import qualified GHC.Exts"
    ,"import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..))"
    ,"import           GHC.Int"
    ,"import           GHC.IO"
    ,"import           GHC.Word"
    ,"import qualified Data.Vector.Unboxed.Base as VUB"
    ,"import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=))"
    ] ++ gen n maxBits

genHalfFile :: String -> [String] -> [Int] -> Int -> [String]
genHalfFile moduleName imports counts !maxBits
  = ["-- This file was created by script/Gen.hs. Do not edit by hand!"
    ] ++ ["{-# LANGUAGE CPP #-}" | maxBits == 128] ++
    ["{-# LANGUAGE TypeFamilies #-}"
    ,"{-# OPTIONS_GHC -Wno-orphans #-}"
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

main :: IO ()
main = do
  createDirectoryIfMissing True "src/Data/Simdy/Internal/Class"
  writeFile "src/Data/Simdy/Internal/Class/Generated.hs" $ unlines $
    ["-- This file was created by script/Gen.hs. Do not edit by hand!"
    -- ,"{-# LANGUAGE PatternSynonyms #-}"
    -- ,"{-# LANGUAGE ViewPatterns #-}"
    ,"module Data.Simdy.Internal.Class.Generated where"]
    ++ concatMap (\n -> ["class PackX" ++ show n ++ " f a where"
                        ,"  packX" ++ show n ++ " :: " ++ concat (replicate n "a -> ") ++ "f a"
                        ,"  unpackX" ++ show n ++ " :: f a -> (" ++ commaSep (replicate n "a") ++ ")"
                        ,"toListX" ++ show n ++ " :: PackX" ++ show n ++ " x a => x a -> [a]"
                        ,"toListX" ++ show n ++ " v = case unpackX" ++ show n ++ " v of"
                        ,"  (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") -> [" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ "]"
                        ,"fromListX" ++ show n ++ " :: PackX" ++ show n ++ " x a => [a] -> x a"
                        ,"fromListX" ++ show n ++ " [" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ "] = packX" ++ show n ++ concat [" a" ++ show i | i <- [0..n-1]]
                        ,"fromListX" ++ show n ++ " xs | length xs < " ++ show n ++ " = error \"fromListX" ++ show n ++ ": List too short\""
                        ,"         " ++ map (const ' ') (show n) ++ "    | otherwise = error \"fromListX" ++ show n ++ ": List too long\""
                        ]) [2,4,8,16,32]
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
  createDirectoryIfMissing True "src/Data/Simdy/Internal/NoSIMD"
  createDirectoryIfMissing True "src/Data/Simdy/Internal/SIMD128"
  createDirectoryIfMissing True "src/Data/Simdy/Internal/SIMD256"
  createDirectoryIfMissing True "src/Data/Simdy/Internal/SIMD512"
  forM_ [2,4,8,16,32] $ \i -> do
    writeFile ("src/Data/Simdy/Internal/NoSIMD/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.NoSIMD.X" ++ show i) [] i 0
    writeFile ("src/Data/Simdy/Internal/SIMD128/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.SIMD128.X" ++ show i) ["Data.Simdy.Internal.SIMD128.Prim", "Data.Simdy.Internal.SIMD128.PrimExtra"] i 128
    when (i * 64 > 128) $ writeFile ("src/Data/Simdy/Internal/SIMD256/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.SIMD256.X" ++ show i) ["Data.Simdy.Internal.SIMD256.Prim", "Data.Simdy.Internal.SIMD128.PrimExtra", "Data.Simdy.Internal.SIMD256.PrimExtra"] i 256
    when (i * 64 > 256) $ writeFile ("src/Data/Simdy/Internal/SIMD512/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.SIMD512.X" ++ show i) ["Data.Simdy.Internal.SIMD512.Prim", "Data.Simdy.Internal.SIMD128.PrimExtra", "Data.Simdy.Internal.SIMD256.PrimExtra", "Data.Simdy.Internal.SIMD512.PrimExtra"] i 512
  writeFile "src/Data/Simdy/Internal/NoSIMD/HalfVector.hs" $ unlines $ genHalfFile "Data.Simdy.Internal.NoSIMD.HalfVector"
    ["Data.Functor.Identity"
    ,"Data.Simdy.Internal.NoSIMD.X2"
    ,"Data.Simdy.Internal.NoSIMD.X4"
    ,"Data.Simdy.Internal.NoSIMD.X8"
    ,"Data.Simdy.Internal.NoSIMD.X16"
    ,"Data.Simdy.Internal.NoSIMD.X32"
    ] [2,4,8,16,32] 0
  writeFile "src/Data/Simdy/Internal/SIMD128/HalfVector.hs" $ unlines $ genHalfFile "Data.Simdy.Internal.SIMD128.HalfVector"
    ["Data.Functor.Identity"
    ,"Data.Simdy.Internal.SIMD128.X2"
    ,"Data.Simdy.Internal.SIMD128.X4"
    ,"Data.Simdy.Internal.SIMD128.X8"
    ,"Data.Simdy.Internal.SIMD128.X16"
    ,"Data.Simdy.Internal.SIMD128.X32"
    ] [2,4,8,16,32] 128
  writeFile "src/Data/Simdy/Internal/SIMD256/HalfVector.hs" $ unlines $ genHalfFile "Data.Simdy.Internal.SIMD256.HalfVector"
    ["Data.Simdy.Internal.SIMD128.HalfVector ()"
    ,"Data.Simdy.Internal.SIMD128.X2"
    ,"Data.Simdy.Internal.SIMD256.X4"
    ,"Data.Simdy.Internal.SIMD256.X8"
    ,"Data.Simdy.Internal.SIMD256.X16"
    ,"Data.Simdy.Internal.SIMD256.X32"
    ] [4,8,16,32] 256
  writeFile "src/Data/Simdy/Internal/SIMD512/HalfVector.hs" $ unlines $ genHalfFile "Data.Simdy.Internal.SIMD512.HalfVector"
    ["Data.Simdy.Internal.SIMD256.X4"
    ,"Data.Simdy.Internal.SIMD256.HalfVector ()"
    ,"Data.Simdy.Internal.SIMD512.X8"
    ,"Data.Simdy.Internal.SIMD512.X16"
    ,"Data.Simdy.Internal.SIMD512.X32"
    ] [8,16,32] 512

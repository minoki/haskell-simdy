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
  = ["data family " ++ tyCon ++ " a"
    ,"instance ShortVectorLength " ++ tyCon ++ " where"
    ,"  shortVectorLength = " ++ show vecCount
    ,if vecCount == 2
     then "type instance HalfVector " ++ tyCon ++ " = Identity"
     else "type instance HalfVector " ++ tyCon ++ " = X" ++ show (vecCount `quot` 2)
    ]
    ++ genType "Float" "F#" 32 [genNum True, genFractional, genFloating, genPrim, genStorable]
    ++ genType "Double" "D#" 64 [genNum True, genFractional, genFloating, genPrim, genStorable]
    ++ genType "Int8" "I8#" 8 [genNum True, genPrim, genStorable]
    ++ genType "Int16" "I16#" 16 [genNum True, genPrim, genStorable]
    ++ genType "Int32" "I32#" 32 [genNum True, genPrim, genStorable]
    ++ genType "Int64" "I64#" 64 [genNum True, genPrim, genStorable]
    ++ genType "Word8" "W8#" 8 [genNum False, genPrim, genStorable]
    ++ genType "Word16" "W16#" 16 [genNum False, genPrim, genStorable]
    ++ genType "Word32" "W32#" 32 [genNum False, genPrim, genStorable]
    ++ genType "Word64" "W64#" 64 [genNum False, genPrim, genStorable]
    ++ genNewtype "Sum"
    ++ genNewtype "Product"
    ++ genNewtype "Min"
    ++ genNewtype "Max"
    ++ ["data instance " ++ tyCon ++ " (Complex a) = MkComplex" ++ tyCon ++ " !(" ++ tyCon ++ " a) !(" ++ tyCon ++ " a)"
       ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " a => Pack" ++ tyCon ++ " " ++ tyCon ++ " (Complex a) where"
       ,"  pack" ++ tyCon ++ " " ++ spaceSep ["(x" ++ show i ++ " :+ y" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " = MkComplex" ++ tyCon ++ " (pack" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (pack" ++ tyCon ++ " " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ")"
       ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " a => Unpack" ++ tyCon ++ " " ++ tyCon ++ " (Complex a) where"
       ,"  unpack" ++ tyCon ++ " (MkComplex" ++ tyCon ++ " s t) = case unpack" ++ tyCon ++ " s of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> case unpack" ++ tyCon ++ " t of (" ++ commaSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") -> (" ++ commaSep ["x" ++ show i ++ " :+ y" ++ show i | i <- [0..vecCount-1]] ++ ")"
       ,"instance Broadcast " ++ tyCon ++ " a => Broadcast " ++ tyCon ++ " (Complex a) where"
       ,"  broadcast (x :+ y) = MkComplex" ++ tyCon ++ " (broadcast x) (broadcast y)"
       ]
    ++ (if vecCount == 2
        then ["instance SplitShortVector " ++ tyCon ++ " a => SplitShortVector " ++ tyCon ++ " (Complex a) where"
             ,"  splitShortVector (MkComplex" ++ tyCon ++ " s t) = case splitShortVector s of (Identity x0, Identity x1) -> case splitShortVector t of (Identity y0, Identity y1) -> (Identity (x0 :+ y0), Identity (x1 :+ y1))"
             ,"  joinShortVector (Identity (x0 :+ y0)) (Identity (x1 :+ y1)) = MkComplex" ++ tyCon ++ " (joinShortVector (Identity x0) (Identity x1)) (joinShortVector (Identity y0) (Identity y1))"
             ]
        else
          let halfTyCon = "X" ++ show (vecCount `quot` 2)
          in ["instance SplitShortVector " ++ tyCon ++ " a => SplitShortVector " ++ tyCon ++ " (Complex a) where"
             ,"  splitShortVector (MkComplex" ++ tyCon ++ " s t) = case splitShortVector s of (x0, x1) -> case splitShortVector t of (y0, y1) -> (MkComplex" ++ halfTyCon ++ " x0 y0, MkComplex" ++ halfTyCon ++ " x1 y1)"
             ,"  joinShortVector (MkComplex" ++ halfTyCon ++ " x0 y0) (MkComplex" ++ halfTyCon ++ " x1 y1) = MkComplex" ++ tyCon ++ " (joinShortVector x0 x1) (joinShortVector y0 y1)"
             ]
       )
    ++ ["instance UnboxSIMD " ++ tyCon ++ " a => UnboxSIMD " ++ tyCon ++ " (Complex a) where"
       ,"  unsafeIndexUnboxedSIMD (VUB.V_Complex (VUB.V_2 _ u v)) !i = MkComplex" ++ tyCon ++ " (unsafeIndexUnboxedSIMD u i) (unsafeIndexUnboxedSIMD v i)"
       ,"  unsafeReadUnboxedSIMD (VUB.MV_Complex (VUB.MV_2 _ u v)) !i = do { x <- unsafeReadUnboxedSIMD u i; y <- unsafeReadUnboxedSIMD v i; pure (MkComplex" ++ tyCon ++ " x y) }"
       ,"  unsafeWriteUnboxedSIMD (VUB.MV_Complex (VUB.MV_2 _ u v)) !i (MkComplex" ++ tyCon ++ " x y) = do { unsafeWriteUnboxedSIMD u i x; unsafeWriteUnboxedSIMD v i y }"
       ,"data instance " ++ tyCon ++ " () = MkUnit" ++ tyCon
       ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " () where"
       ,"  pack" ++ tyCon ++ " " ++ spaceSep (replicate vecCount "_") ++ " = MkUnit" ++ tyCon
       ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " () where"
       ,"  unpack" ++ tyCon ++ " MkUnit" ++ tyCon ++ " = (" ++ commaSep (replicate vecCount "()") ++ ")"
       ,"instance Broadcast " ++ tyCon ++ " () where"
       ,"  broadcast _ = MkUnit" ++ tyCon
       ]
    ++ (if vecCount == 2
        then ["instance SplitShortVector " ++ tyCon ++ " () where"
             ,"  splitShortVector MkUnit" ++ tyCon ++ " = (Identity (), Identity ())"
             ,"  joinShortVector _ _ = MkUnit" ++ tyCon
             ]
        else
          let halfTyCon = "X" ++ show (vecCount `quot` 2)
          in ["instance SplitShortVector " ++ tyCon ++ " () where"
             ,"  splitShortVector MkUnit" ++ tyCon ++ " = (MkUnit" ++ halfTyCon ++ ", MkUnit" ++ halfTyCon ++ ")"
             ,"  joinShortVector MkUnit" ++ halfTyCon ++ " MkUnit" ++ halfTyCon ++ " = MkUnit" ++ tyCon
             ]
       )
    ++ concatMap genTuple [2..maxTupleLen]
    ++ ["instance (Pack" ++ tyCon ++ " " ++ tyCon ++ " a, Unpack" ++ tyCon ++ " " ++ tyCon ++ " a) => MonoMap " ++ tyCon ++ " a where"
       ,"  monoMap f !v = case unpack" ++ tyCon ++ " v of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> pack" ++ tyCon ++ " " ++ spaceSep ["(f x" ++ show i ++ ")" | i <- [0..vecCount-1]]
       ,"instance (Pack" ++ tyCon ++ " " ++ tyCon ++ " a, Unpack" ++ tyCon ++ " " ++ tyCon ++ " a) => MonoZipWith " ++ tyCon ++ " a where"
       ,"  monoZipWith f !u !v = case unpack" ++ tyCon ++ " u of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> case unpack" ++ tyCon ++ " v of (" ++ commaSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") -> pack" ++ tyCon ++ " " ++ spaceSep ["(f x" ++ show i ++ " y" ++ show i ++ ")" | i <- [0..vecCount-1]]
       ]
    ++ ["instance MkTuple " ++ tyCon ++ " where"]
    ++ ["  mkTuple" ++ show i ++ " = MkTuple" ++ show i ++ tyCon | i <- [2..maxTupleLen]]
    ++ ["instance DeconstructTuple " ++ tyCon ++ " where"]
    ++ ["  deconstructTuple" ++ show i ++ " (MkTuple" ++ show i ++ tyCon ++ " " ++ spaceSep ["v" ++ show j | j <- [0..i-1]] ++ ") = (" ++ commaSep ["v" ++ show j | j <- [0..i-1]] ++ ")" | i <- [2..maxTupleLen]]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance NumF " ++ tyCon ++ " a => Num (" ++ tyCon ++ " a)"]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance FractionalF " ++ tyCon ++ " a => Fractional (" ++ tyCon ++ " a)"]
    ++ ["deriving via WrappedMulti " ++ tyCon ++ " a instance FloatingF " ++ tyCon ++ " a => Floating (" ++ tyCon ++ " a)"]
  where
    tyCon = 'X' : show vecCount
    genType name primCon !bitsPerElem others
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
            halfTyCon = if vecCount == 2 then "Identity" else "X" ++ show (vecCount `quot` 2)
            mainDef = if bitCount < 128 || maxBits == 0
                      then ["data instance " ++ tyCon ++ " " ++ name ++ " = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep (replicate vecCount ('!':name))
                           ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  pack" ++ tyCon ++ " = Mk" ++ name ++ tyCon ++ "WithElems"
                           ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  unpack" ++ tyCon ++ " (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ")"
                           ,"instance Broadcast " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  broadcast !x = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep (replicate vecCount "x")
                           ]
                           ++ if vecCount == 2
                              then ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                                   ,"  splitShortVector (Mk" ++ name ++ tyCon ++ "WithElems x0 x1) = (Identity x0, Identity x1)"
                                   ,"  joinShortVector (Identity x0) (Identity x1) = Mk" ++ name ++ tyCon ++ "WithElems x0 x1"
                                   ]
                              else ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                                   ,"  splitShortVector (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ", Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ")"
                                   ,"  joinShortVector (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ") (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]]
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
                           ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                           ,"  unpack" ++ tyCon ++ " (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = " ++ concat ["case unpack" ++ shortVecName ++ "# v" ++ show i ++ " of (# " ++ commaSep ["x" ++ show (i * shortVecSize + j) | j <- [0..shortVecSize - 1]] ++ " #) -> " | i <- [0..shortVecCount - 1]] ++ "(" ++ commaSep [primCon ++ " x" ++ show i | i <- [0..vecCount-1]] ++ ")"
                           ,"instance Broadcast " ++ tyCon ++ " " ++ name ++ " where"
                           ,if shortVecCount == 1
                            then "  broadcast (" ++ primCon ++ " x) = Mk" ++ name ++ tyCon ++ suffix ++ " (broadcast" ++ name ++ "X" ++ show shortVecSize ++ "# x)"
                            else "  broadcast (" ++ primCon ++ " x) = let !v = broadcast" ++ name ++ "X" ++ show shortVecSize ++ "# x in Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep (replicate shortVecCount "v")
                           ]
                           ++ if vecCount == 2
                              then ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                                   ,"  splitShortVector v = coerce (unpack" ++ tyCon ++ " v)"
                                   ,"  joinShortVector (Identity x0) (Identity x1) = pack" ++ tyCon ++ " x0 x1"
                                   ]
                              else if shortVecCount == 1
                                   then ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                                        ,"  splitShortVector v = case unpack" ++ tyCon ++ " v of (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") -> (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ", Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ")"
                                        ,"  joinShortVector (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..(vecCount `quot` 2)-1]] ++ ") (Mk" ++ name ++ halfTyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [vecCount `quot` 2..vecCount-1]] ++ ") = pack" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]]
                                        ]
                                   else
                                     let halfSuffix | shortVecCount == 2 = ""
                                                    | otherwise = "WithVec" ++ show vecBitCount
                                     in ["instance SplitShortVector " ++ tyCon ++ " " ++ name ++ " where"
                                        ,"  splitShortVector (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = (Mk" ++ name ++ halfTyCon ++ halfSuffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..(shortVecCount `quot` 2)-1]] ++ ", Mk" ++ name ++ halfTyCon ++ halfSuffix ++ " " ++ spaceSep ["v" ++ show i | i <- [shortVecCount `quot` 2..shortVecCount-1]] ++ ")"
                                        ,"  joinShortVector (Mk" ++ name ++ halfTyCon ++ halfSuffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..(shortVecCount `quot` 2)-1]] ++ ") (Mk" ++ name ++ halfTyCon ++ halfSuffix ++ " " ++ spaceSep ["v" ++ show i | i <- [shortVecCount `quot` 2..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]]
                                        ]
        in mainDef ++ concatMap (\f -> f name primCon bitsPerElem) others
    genNum isSigned name primCon !bitsPerElem
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["instance NumF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  addF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " + y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  subF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " - y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  mulF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " * y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  negateF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(- x" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance NumF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  addF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(plus" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  subF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(minus" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ,"  mulF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(times" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ]
                ++ if isSigned
                   then ["  negateF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(negate" ++ shortVecName ++ "# u" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                        ,"  -- Currently, there is no abs" ++ shortVecName ++ "#"
                        ]
                   else ["  -- Currently, there is no negate" ++ shortVecName ++ "#, abs" ++ shortVecName ++ "#"
                        ]
    genFractional name primCon !bitsPerElem
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["instance FractionalF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  divF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["y" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(x" ++ show i ++ " / y" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ,"  recipF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(recip x" ++ show i ++ ")" | i <- [0..vecCount-1]]
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance FractionalF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  divF (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["u" ++ show i | i <- [0..shortVecCount-1]] ++ ") (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(divide" ++ shortVecName ++ "# u" ++ show i ++ " v" ++ show i ++ ")" | i <- [0..shortVecCount-1]]
                ]
    genFloating name primCon !bitsPerElem
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["instance FloatingF " ++ tyCon ++ " " ++ name ++ " where"
                ,"  sqrtF (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(sqrt x" ++ show i ++ ")" | i <- [0..vecCount-1]]
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
    genPrim name primCon !bitsPerElem
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
            i_plus 0 = "i"
            i_plus k = "(i +# " ++ show k ++ "#)"
        in if bitCount < 128 || maxBits == 0
           then ["instance PrimSIMD " ++ tyCon ++ " " ++ name ++ " where"
                ,"  indexByteArraySIMD# ba i = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " (index" ++ name ++ "Array# ba " ++ i_plus i ++ "))" | i <- [0..vecCount-1]]
                ,"  readByteArraySIMD# mba i s0 = " ++ concat ["case read" ++ name ++ "Array# mba " ++ i_plus i ++ " s" ++ show i ++ " of (# s" ++ show (i + 1) ++ ", x" ++ show i ++ " #) -> " | i <- [0..vecCount-1]] ++ "(# s" ++ show vecCount ++ ", Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " #)"
                ,"  writeByteArraySIMD# mba i (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ ") s0 = " ++ concat ["case write" ++ name ++ "Array# mba " ++ i_plus i ++ " x" ++ show i ++ " s" ++ show i ++ " of s" ++ show (i + 1) ++ " -> " | i <- [0..vecCount-2]] ++ "write" ++ name ++ "Array# mba (i +# " ++ show (vecCount - 1) ++ "#) x" ++ show (vecCount - 1) ++ " s" ++ show (vecCount - 1)
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance PrimSIMD " ++ tyCon ++ " " ++ name ++ " where"
                ,"  indexByteArraySIMD# ba i = Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["(index" ++ name ++ "ArrayAs" ++ shortVecName ++ "# ba " ++ i_plus (i * shortVecSize) ++ ")" | i <- [0..shortVecCount-1]]
                ,"  readByteArraySIMD# mba i s0 = " ++ concat ["case read" ++ name ++ "ArrayAs" ++ shortVecName ++ "# mba " ++ i_plus (i * shortVecSize) ++ " s" ++ show i ++ " of (# s" ++ show (i + 1) ++ ", v" ++ show i ++ " #) -> " | i <- [0..shortVecCount-1]] ++ "(# s" ++ show shortVecCount ++ ", Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ " #)"
                ,"  writeByteArraySIMD# mba i (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") s0 = " ++ concat ["case write" ++ name ++ "ArrayAs" ++ shortVecName ++ "# mba " ++ i_plus (i * shortVecSize) ++ " v" ++ show i ++ " s" ++ show i ++ " of s" ++ show (i + 1) ++ " -> " | i <- [0..shortVecCount-2]] ++ "write" ++ name ++ "ArrayAs" ++ shortVecName ++ "# mba " ++ i_plus ((shortVecCount - 1) * shortVecSize) ++ " v" ++ show (shortVecCount - 1) ++ " s" ++ show (shortVecCount - 1)
                ]
    genStorable name primCon !bitsPerElem
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
            i_plus 0 = "i"
            i_plus k = "(i +# " ++ show k ++ "#)"
        in if bitCount < 128 || maxBits == 0
           then ["instance StorableSIMD " ++ tyCon ++ " " ++ name ++ " where"
                ,"  peekElemOffSIMD (Ptr addr) (I# i) = IO (\\s0 -> " ++ concat ["case read" ++ name ++ "OffAddr# addr " ++ i_plus i ++ " s" ++ show i ++ " of (# s" ++ show (i + 1) ++ ", x" ++ show i ++ " #) -> " | i <- [0..vecCount-1]] ++ "(# s" ++ show vecCount ++ ", Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " #))"
                ,"  pokeElemOffSIMD (Ptr addr) (I# i) (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ ") = IO (\\s0 -> " ++ concat ["case write" ++ name ++ "OffAddr# addr " ++ i_plus i ++ " x" ++ show i ++ " s" ++ show i ++ " of s" ++ show (i + 1) ++ " -> " | i <- [0..vecCount-2]] ++ "(# write" ++ name ++ "OffAddr# addr (i +# " ++ show (vecCount - 1) ++ "#) x" ++ show (vecCount - 1) ++ " s" ++ show (vecCount - 1) ++ ", () #))"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
                 suffix | shortVecCount == 1 = ""
                        | otherwise = "WithVec" ++ show vecBitCount
             in ["instance StorableSIMD " ++ tyCon ++ " " ++ name ++ " where"
                ,"  peekElemOffSIMD (Ptr addr) (I# i) = IO (\\s0 -> " ++ concat ["case read" ++ name ++ "OffAddrAs" ++ shortVecName ++ "# addr " ++ i_plus (i * shortVecSize) ++ " s" ++ show i ++ " of (# s" ++ show (i + 1) ++ ", v" ++ show i ++ " #) -> " | i <- [0..shortVecCount-1]] ++ "(# s" ++ show shortVecCount ++ ", Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ " #))"
                ,"  pokeElemOffSIMD (Ptr addr) (I# i) (Mk" ++ name ++ tyCon ++ suffix ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = IO (\\s0 -> " ++ concat ["case write" ++ name ++ "OffAddrAs" ++ shortVecName ++ "# addr " ++ i_plus (i * shortVecSize) ++ " v" ++ show i ++ " s" ++ show i ++ " of s" ++ show (i + 1) ++ " -> " | i <- [0..shortVecCount-2]] ++ "(# write" ++ name ++ "OffAddrAs" ++ shortVecName ++ "# addr " ++ i_plus ((shortVecCount - 1) * shortVecSize) ++ " v" ++ show (shortVecCount - 1) ++ " s" ++ show (shortVecCount - 1) ++ ", () #))"
                ]
    genTuple !n
      = ["data instance " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["!(" ++ tyCon ++ " a" ++ show i ++ ")" | i <- [0..n-1]]
        ,"instance (" ++ commaSep ["Pack" ++ tyCon ++ " " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => Pack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
        ,"  pack" ++ tyCon ++ " " ++ spaceSep ["(" ++ commaSep ["x" ++ show i ++ "_" ++ show j | j <- [0..n-1]] ++ ")" | i <- [0..vecCount-1]] ++ " = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(pack" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i ++ "_" ++ show j | i <- [0..vecCount-1]] ++ ")" | j <- [0..n-1]]
        ,"instance (" ++ commaSep ["Unpack" ++ tyCon ++ " " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => Unpack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
        ,"  unpack" ++ tyCon ++ " (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") = " ++ concat ["case unpack" ++ tyCon ++ " v" ++ show i ++ " of (" ++ commaSep ["x" ++ show j ++ "_" ++ show i | j <- [0..vecCount-1]] ++ ") -> " | i <- [0..n-1]] ++ "(" ++ commaSep ["(" ++ commaSep ["x" ++ show i ++ "_" ++ show j | j <- [0..n-1]] ++ ")" | i <- [0..vecCount-1]] ++ ")"
        ,"instance (" ++ commaSep ["Broadcast " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => Broadcast " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
        ,"  broadcast (" ++ commaSep ["x" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(broadcast x" ++ show i ++ ")" | i <- [0..n-1]]
        ] ++ (if vecCount == 2
              then ["instance (" ++ commaSep ["SplitShortVector " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => SplitShortVector " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
                   ,"  splitShortVector (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") = " ++ concat ["case splitShortVector v" ++ show i ++ " of (Identity a" ++ show i ++ ", Identity b" ++ show i ++ ") -> " | i <- [0..n-1]] ++ "(Identity (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ "), Identity (" ++ commaSep ["b" ++ show i | i <- [0..n-1]] ++ "))"
                   ,"  joinShortVector (Identity (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ")) (Identity (" ++ commaSep ["b" ++ show i | i <- [0..n-1]] ++ ")) = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(joinShortVector (Identity a" ++ show i ++ ") (Identity b" ++ show i ++ "))" | i <- [0..n-1]]
                   ]
              else
                let halfTyCon = "X" ++ show (vecCount `quot` 2)
                in ["instance (" ++ commaSep ["SplitShortVector " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => SplitShortVector " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
                   ,"  splitShortVector (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") = " ++ concat ["case splitShortVector v" ++ show i ++ " of (a" ++ show i ++ ", b" ++ show i ++ ") -> " | i <- [0..n-1]] ++ "(MkTuple" ++ show n ++ halfTyCon ++ " " ++ spaceSep ["a" ++ show i | i <- [0..n-1]] ++ ", MkTuple" ++ show n ++ halfTyCon ++ " " ++ spaceSep ["b" ++ show i | i <- [0..n-1]] ++ ")"
                   ,"  joinShortVector (MkTuple" ++ show n ++ halfTyCon ++ " " ++ spaceSep ["a" ++ show i | i <- [0..n-1]] ++ ") (MkTuple" ++ show n ++ halfTyCon ++ " " ++ spaceSep ["b" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(joinShortVector a" ++ show i ++ " b" ++ show i ++ ")" | i <- [0..n-1]]
                   ]
             )
         ++ if n <= maxTupleLenForUnboxedVector
             then ["instance (" ++ commaSep ["UnboxSIMD " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => UnboxSIMD " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
                  ,"  unsafeIndexUnboxedSIMD (VUB.V_" ++ show n ++ " _ " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") !i = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(unsafeIndexUnboxedSIMD v" ++ show i ++ " i)" | i <- [0..n-1]]
                  ,"  unsafeReadUnboxedSIMD (VUB.MV_" ++ show n ++ " _ " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") !i = do { " ++ semicolonSep ["!s" ++ show i ++ " <- unsafeReadUnboxedSIMD v" ++ show i ++ " i" | i <- [0..n-1]] ++ "; pure (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["s" ++ show i | i <- [0..n-1]] ++ ") }"
                  ,"  unsafeWriteUnboxedSIMD (VUB.MV_" ++ show n ++ " _ " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") !i (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["s" ++ show i | i <- [0..n-1]] ++ ") = do { " ++ semicolonSep ["unsafeWriteUnboxedSIMD v" ++ show i ++ " i s" ++ show i | i <- [0..n-1]] ++ " }"
                  ]
             else []
    genNewtype !name
      = ["newtype instance " ++ tyCon ++ " (" ++ name ++ " a) = Mk" ++ name ++ tyCon ++ " (" ++ tyCon ++ " a)"
        ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " a => Pack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  pack" ++ tyCon ++ " = coerce (pack" ++ tyCon ++ " @" ++ tyCon ++ " @a)"
        ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " a => Unpack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  unpack" ++ tyCon ++ " = coerce (unpack" ++ tyCon ++ " @" ++ tyCon ++ " @a)"
        ,"instance Broadcast " ++ tyCon ++ " a => Broadcast " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  broadcast = coerce (broadcast @" ++ tyCon ++ " @a)"
        ,"instance SplitShortVector " ++ tyCon ++ " a => SplitShortVector " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  splitShortVector = coerce (splitShortVector @" ++ tyCon ++ " @a)"
        ,"  joinShortVector = coerce (joinShortVector @" ++ tyCon ++ " @a)"
        ,"instance UnboxSIMD " ++ tyCon ++ " a => UnboxSIMD " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  unsafeIndexUnboxedSIMD = coerce (unsafeIndexUnboxedSIMD @" ++ tyCon ++ " @a)"
        ,"  unsafeReadUnboxedSIMD = coerce (unsafeReadUnboxedSIMD @" ++ tyCon ++ " @a)"
        ,"  unsafeWriteUnboxedSIMD = coerce (unsafeWriteUnboxedSIMD @" ++ tyCon ++ " @a)"
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
    ,"import Data.Simdy.Class"
    ] ++ gen 2 maxBits ++ gen 4 maxBits ++ gen 8 maxBits ++ gen 16 maxBits ++ gen 32 maxBits
-}

genFile :: String -> String -> Int -> Int -> [String]
genFile moduleName halfMod !n !maxBits
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
    ,"import Data.Simdy.Class"
    ,"import qualified Data.Vector.Unboxed.Base as VUB"
    ,if n == 2
     then "import Data.Functor.Identity"
     else "import " ++ halfMod
    ] ++ gen n maxBits

main :: IO ()
main = do
  createDirectoryIfMissing True "src/Data/Simdy/Class"
  writeFile "src/Data/Simdy/Class/Generated.hs" $ unlines $
    ["-- This file was created by script/Gen.hs. Do not edit by hand!"
    ,"{-# LANGUAGE PatternSynonyms #-}"
    ,"{-# LANGUAGE ViewPatterns #-}"
    ,"module Data.Simdy.Class.Generated where"]
    ++ concatMap (\n -> ["class PackX" ++ show n ++ " f a where"
                        ,"  packX" ++ show n ++ " :: " ++ concat (replicate n "a -> ") ++ "f a"
                        ,"class UnpackX" ++ show n ++ " f a where"
                        ,"  unpackX" ++ show n ++ " :: f a -> (" ++ commaSep (replicate n "a") ++ ")"
                        ]) [2,4,8,16,32]
    ++ ["class MkTuple f where"]
    ++ ["  mkTuple" ++ show n ++ " :: " ++ concat ["f a" ++ show i ++ " -> " | i <- [0..n-1]] ++ "f (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ")" | n <- [2..maxTupleLen]]
    ++ ["class DeconstructTuple f where"]
    ++ ["  deconstructTuple" ++ show n ++ " :: " ++ "f (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") -> (" ++ commaSep ["f a" ++ show i | i <- [0..n-1]] ++ ")" | n <- [2..maxTupleLen]]
    ++ concat [["pattern MkTuple" ++ show n ++ " :: (MkTuple f, DeconstructTuple f) => " ++ concat ["f a" ++ show i ++ " -> " | i <- [0..n-1]] ++ "f (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ")"
               ,"pattern MkTuple" ++ show n ++ " " ++ spaceSep ["x" ++ show i | i <- [0..n-1]] ++ " <- (deconstructTuple" ++ show n ++ " -> (" ++ commaSep ["x" ++ show i | i <- [0..n-1]] ++ ")) where"
               ,"  MkTuple" ++ show n ++ " = mkTuple" ++ show n
               ] | n <- [2..maxTupleLen]]
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
    writeFile ("src/Data/Simdy/Internal/NoSIMD/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.NoSIMD.X" ++ show i) ("Data.Simdy.Internal.NoSIMD.X" ++ show (i `quot` 2)) i 0
    writeFile ("src/Data/Simdy/Internal/SIMD128/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.SIMD128.X" ++ show i) ("Data.Simdy.Internal.SIMD128.X" ++ show (i `quot` 2)) i 128
    when (i * 64 > 128) $ writeFile ("src/Data/Simdy/Internal/SIMD256/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.SIMD256.X" ++ show i) ("Data.Simdy.Internal.SIMD" ++ (if i * 32 > 128 then "256" else "128") ++ ".X" ++ show (i `quot` 2)) i 256
    when (i * 64 > 256) $ writeFile ("src/Data/Simdy/Internal/SIMD512/X" ++ show i ++ ".hs") $ unlines $ genFile ("Data.Simdy.Internal.SIMD512.X" ++ show i) ("Data.Simdy.Internal.SIMD" ++ (if i * 32 > 256 then "512" else "256") ++ ".X" ++ show (i `quot` 2)) i 512

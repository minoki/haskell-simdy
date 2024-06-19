-- Run as 'runghc script/Gen.hs'
import System.IO
import qualified Data.List as List

maxTupleLen :: Int
maxTupleLen = 15

spaceSep :: [String] -> String
spaceSep = List.intercalate " "

commaSep :: [String] -> String
commaSep = List.intercalate ", "

gen :: Int -> Int -> [String]
gen !vecCount !maxBits
  = ["data family " ++ tyCon ++ " a"]
    ++ genType "Float" "F#" 32
    ++ genType "Double" "D#" 64
    ++ genType "Int8" "I8#" 8
    ++ genType "Int16" "I16#" 16
    ++ genType "Int32" "I32#" 32
    ++ genType "Int64" "I64#" 64
    ++ genType "Word8" "W8#" 8
    ++ genType "Word16" "W16#" 16
    ++ genType "Word32" "W32#" 32
    ++ genType "Word64" "W64#" 64
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
       ,"data instance " ++ tyCon ++ " () = MkUnit" ++ tyCon
       ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " () where"
       ,"  pack" ++ tyCon ++ " " ++ spaceSep (replicate vecCount "_") ++ " = MkUnit" ++ tyCon
       ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " () where"
       ,"  unpack" ++ tyCon ++ " MkUnit" ++ tyCon ++ " = (" ++ commaSep (replicate vecCount "()") ++ ")"
       ,"instance Broadcast " ++ tyCon ++ " () where"
       ,"  broadcast _ = MkUnit" ++ tyCon
       ]
    ++ concatMap genTuple [2..maxTupleLen]
  where
    tyCon = 'X' : show vecCount
    genType name primCon !bitsPerElem
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["data instance " ++ tyCon ++ " " ++ name ++ " = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep (replicate vecCount ('!':name))
                ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                ,"  pack" ++ tyCon ++ " = Mk" ++ name ++ tyCon ++ "WithElems"
                ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                ,"  unpack" ++ tyCon ++ " (Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = (" ++ commaSep ["x" ++ show i | i <- [0..vecCount-1]] ++ ")"
                ,"instance Broadcast " ++ tyCon ++ " " ++ name ++ " where"
                ,"  broadcast !x = Mk" ++ name ++ tyCon ++ "WithElems " ++ spaceSep (replicate vecCount "x")
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
             in ["data instance " ++ tyCon ++ " " ++ name ++ " = Mk" ++ name ++ tyCon ++ "WithVec" ++ show vecBitCount ++ " " ++ spaceSep (replicate shortVecCount (shortVecName ++ "#"))
                ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                ,"  pack" ++ tyCon ++ " " ++ spaceSep ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " = Mk" ++ name ++ tyCon ++ "WithVec" ++ show vecBitCount ++ " " ++ spaceSep ["(pack" ++ shortVecName ++ "# (# " ++ commaSep ["x" ++ show (i * shortVecSize + j) | j <- [0..shortVecSize - 1]] ++ " #))" | i <- [0..shortVecCount - 1]]
                ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                ,"  unpack" ++ tyCon ++ " (Mk" ++ name ++ tyCon ++ "WithVec" ++ show vecBitCount ++ " " ++ spaceSep ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = " ++ concat ["case unpack" ++ shortVecName ++ "# v" ++ show i ++ " of (# " ++ commaSep ["x" ++ show (i * shortVecSize + j) | j <- [0..shortVecSize - 1]] ++ " #) -> " | i <- [0..shortVecCount - 1]] ++ "(" ++ commaSep [primCon ++ " x" ++ show i | i <- [0..vecCount-1]] ++ ")"
                ,"instance Broadcast " ++ tyCon ++ " " ++ name ++ " where"
                ,if shortVecCount == 1
                  then "  broadcast (" ++ primCon ++ " x) = Mk" ++ name ++ tyCon ++ "WithVec" ++ show vecBitCount ++ " (broadcast" ++ name ++ "X" ++ show shortVecSize ++ "# x)"
                  else "  broadcast (" ++ primCon ++ " x) = let !v = broadcast" ++ name ++ "X" ++ show shortVecSize ++ "# x in Mk" ++ name ++ tyCon ++ "WithVec" ++ show vecBitCount ++ " " ++ spaceSep (replicate shortVecCount "v")
                ]
    genTuple !n
      = ["data instance " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["!(" ++ tyCon ++ " a" ++ show i ++ ")" | i <- [0..n-1]]
        ,"instance (" ++ commaSep ["Pack" ++ tyCon ++ " " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => Pack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
        ,"  pack" ++ tyCon ++ " " ++ spaceSep ["(" ++ commaSep ["x" ++ show i ++ "_" ++ show j | j <- [0..n-1]] ++ ")" | i <- [0..vecCount-1]] ++ " = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(pack" ++ tyCon ++ " " ++ spaceSep ["x" ++ show i ++ "_" ++ show j | i <- [0..vecCount-1]] ++ ")" | j <- [0..n-1]]
        ,"instance (" ++ commaSep ["Unpack" ++ tyCon ++ " " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => Unpack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
        ,"  unpack" ++ tyCon ++ " (MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["v" ++ show i | i <- [0..n-1]] ++ ") = " ++ concat ["case unpack" ++ tyCon ++ " v" ++ show i ++ " of (" ++ commaSep ["x" ++ show j ++ "_" ++ show i | j <- [0..vecCount-1]] ++ ") -> " | i <- [0..n-1]] ++ "(" ++ commaSep ["(" ++ commaSep ["x" ++ show i ++ "_" ++ show j | j <- [0..n-1]] ++ ")" | i <- [0..vecCount-1]] ++ ")"
        ,"instance (" ++ commaSep ["Broadcast " ++ tyCon ++ " a" ++ show i | i <- [0..n-1]] ++ ") => Broadcast " ++ tyCon ++ " (" ++ commaSep ["a" ++ show i | i <- [0..n-1]] ++ ") where"
        ,"  broadcast (" ++ commaSep ["x" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ spaceSep ["(broadcast x" ++ show i ++ ")" | i <- [0..n-1]]
        ]
    genNewtype !name
      = ["newtype instance " ++ tyCon ++ " (" ++ name ++ " a) = Mk" ++ name ++ tyCon ++ " (" ++ tyCon ++ " a)"
        ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " a => Pack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  pack" ++ tyCon ++ " = coerce (pack" ++ tyCon ++ " @" ++ tyCon ++ " @a)"
        ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " a => Unpack" ++ tyCon ++ " " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  unpack" ++ tyCon ++ " = coerce (unpack" ++ tyCon ++ " @" ++ tyCon ++ " @a)"
        ,"instance Broadcast " ++ tyCon ++ " a => Broadcast " ++ tyCon ++ " (" ++ name ++ " a) where"
        ,"  broadcast = coerce (broadcast @" ++ tyCon ++ " @a)"
        ]

genFile :: String -> Int -> [String]
genFile moduleName !maxBits
  = ["-- This file was created by Gen.hs. Do not edit by hand!"
    ,"{-# LANGUAGE MagicHash #-}"
    ,"{-# LANGUAGE TypeFamilies #-}"
    ,"{-# LANGUAGE UnboxedTuples #-}"
    ,"module " ++ moduleName ++ " where"
    ,"import GHC.Int"
    ,"import GHC.Word"
    ,"import Data.Monoid"
    ,"import Data.Semigroup"
    ,"import Data.Complex"
    ,"import GHC.Exts"
    ,"import Data.ShortVector.Class"
    ] ++ gen 2 maxBits ++ gen 4 maxBits ++ gen 8 maxBits ++ gen 16 maxBits ++ gen 32 maxBits

main :: IO ()
main = do
  writeFile "src/Data/ShortVector/Class/Generated.hs" $ unlines $
    ["-- This file was created by Gen.hs. Do not edit by hand!"
    ,"{-# LANGUAGE PatternSynonyms #-}"
    ,"{-# LANGUAGE ViewPatterns #-}"
    ,"module Data.ShortVector.Class.Generated where"]
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
  writeFile "src/Data/ShortVector/Internal/NoSIMD.hs" $ unlines $ genFile "Data.ShortVector.Internal.NoSIMD" 0
  writeFile "src/Data/ShortVector/Internal/SIMD128.hs" $ unlines $ genFile "Data.ShortVector.Internal.SIMD128" 128
  writeFile "src/Data/ShortVector/Internal/SIMD256.hs" $ unlines $ genFile "Data.ShortVector.Internal.SIMD256" 256
  writeFile "src/Data/ShortVector/Internal/SIMD512.hs" $ unlines $ genFile "Data.ShortVector.Internal.SIMD512" 512

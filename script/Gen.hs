-- Run as 'runghc script/Gen.hs'
import System.IO
import qualified Data.List as List

maxTupleLen :: Int
maxTupleLen = 15

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
    ++ ["newtype instance " ++ tyCon ++ " (Sum a) = MkSum" ++ tyCon ++ " (" ++ tyCon ++ " a)"]
    ++ ["newtype instance " ++ tyCon ++ " (Product a) = MkProduct" ++ tyCon ++ " (" ++ tyCon ++ " a)"]
    ++ ["newtype instance " ++ tyCon ++ " (Min a) = MkMin" ++ tyCon ++ " (" ++ tyCon ++ " a)"]
    ++ ["newtype instance " ++ tyCon ++ " (Max a) = MkMax" ++ tyCon ++ " (" ++ tyCon ++ " a)"]
    ++ ["data instance " ++ tyCon ++ " (Complex a) = MkComplex" ++ tyCon ++ " !(" ++ tyCon ++ " a) !(" ++ tyCon ++ " a)"]
    ++ ["data instance " ++ tyCon ++ " () = MkUnit" ++ tyCon ++ ""]
    ++ concatMap genTuple [2..maxTupleLen]
  where
    tyCon = 'X' : show vecCount
    genType name primCon !bitsPerElem
      = let bitCount = bitsPerElem * vecCount
            vecBitCount = min bitCount maxBits
        in if bitCount < 128 || maxBits == 0
           then ["data instance " ++ tyCon ++ " " ++ name ++ " = Mk" ++ name ++ tyCon ++ "WithElems " ++ List.intercalate " " (replicate vecCount ('!':name))
                ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                ,"  pack" ++ tyCon ++ " = Mk" ++ name ++ tyCon ++ "WithElems"
                ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                ,"  unpack" ++ tyCon ++ " (Mk" ++ name ++ tyCon ++ "WithElems " ++ List.intercalate " " ["x" ++ show i | i <- [0..vecCount-1]] ++ ") = (" ++ List.intercalate ", " ["x" ++ show i | i <- [0..vecCount-1]] ++ ")"
                ]
           else
             let shortVecSize = vecBitCount `div` bitsPerElem
                 shortVecCount = bitCount `div` vecBitCount
                 shortVecName = name ++ "X" ++ show shortVecSize
             in ["data instance " ++ tyCon ++ " " ++ name ++ " = Mk" ++ name ++ tyCon ++ "WithVec" ++ show vecBitCount ++ " " ++ List.intercalate " " (replicate shortVecCount (shortVecName ++ "#"))
                ,"instance Pack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                ,"  pack" ++ tyCon ++ " " ++ List.intercalate " " ["(" ++ primCon ++ " x" ++ show i ++ ")" | i <- [0..vecCount-1]] ++ " = Mk" ++ name ++ tyCon ++ "WithVec" ++ show vecBitCount ++ " " ++ List.intercalate " " ["(pack" ++ shortVecName ++ "# (# " ++ List.intercalate ", " ["x" ++ show (i * shortVecSize + j) | j <- [0..shortVecSize - 1]] ++ " #))" | i <- [0..shortVecCount - 1]]
                ,"instance Unpack" ++ tyCon ++ " " ++ tyCon ++ " " ++ name ++ " where"
                ,"  unpack" ++ tyCon ++ " (Mk" ++ name ++ tyCon ++ "WithVec" ++ show vecBitCount ++ " " ++ List.intercalate " " ["v" ++ show i | i <- [0..shortVecCount-1]] ++ ") = " ++ concat ["case unpack" ++ shortVecName ++ "# v" ++ show i ++ " of (# " ++ List.intercalate ", " ["x" ++ show (i * shortVecSize + j) | j <- [0..shortVecSize - 1]] ++ " #) -> " | i <- [0..shortVecCount - 1]] ++ "(" ++ List.intercalate ", " [primCon ++ " x" ++ show i | i <- [0..vecCount-1]] ++ ")"
                ]
    genTuple !n
      = ["data instance " ++ tyCon ++ " (" ++ List.intercalate ", " ["a" ++ show i | i <- [0..n-1]] ++ ") = MkTuple" ++ show n ++ tyCon ++ " " ++ List.intercalate " " ["!(" ++ tyCon ++ " a" ++ show i ++ ")" | i <- [0..n-1]]]

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
                        ,"  unpackX" ++ show n ++ " :: f a -> (" ++ List.intercalate ", " (replicate n "a") ++ ")"
                        ]) [2,4,8,16,32]
    ++ ["class MkTuple f where"]
    ++ ["  mkTuple" ++ show n ++ " :: " ++ concat ["f a" ++ show i ++ " -> " | i <- [0..n-1]] ++ "f (" ++ List.intercalate ", " ["a" ++ show i | i <- [0..n-1]] ++ ")" | n <- [2..maxTupleLen]]
    ++ ["class DeconstructTuple f where"]
    ++ ["  deconstructTuple" ++ show n ++ " :: " ++ "f (" ++ List.intercalate ", " ["a" ++ show i | i <- [0..n-1]] ++ ") -> (" ++ List.intercalate ", " ["f a" ++ show i | i <- [0..n-1]] ++ ")" | n <- [2..maxTupleLen]]
    ++ concat [["pattern MkTuple" ++ show n ++ " :: (MkTuple f, DeconstructTuple f) => " ++ concat ["f a" ++ show i ++ " -> " | i <- [0..n-1]] ++ "f (" ++ List.intercalate ", " ["a" ++ show i | i <- [0..n-1]] ++ ")"
               ,"pattern MkTuple" ++ show n ++ " " ++ List.intercalate " " ["x" ++ show i | i <- [0..n-1]] ++ " <- (deconstructTuple" ++ show n ++ " -> (" ++ List.intercalate ", " ["x" ++ show i | i <- [0..n-1]] ++ ")) where"
               ,"  MkTuple" ++ show n ++ " = mkTuple" ++ show n
               ] | n <- [2..maxTupleLen]]
  writeFile "src/Data/ShortVector/Internal/NoSIMD.hs" $ unlines $ genFile "Data.ShortVector.Internal.NoSIMD" 0
  writeFile "src/Data/ShortVector/Internal/SIMD128.hs" $ unlines $ genFile "Data.ShortVector.Internal.SIMD128" 128
  writeFile "src/Data/ShortVector/Internal/SIMD256.hs" $ unlines $ genFile "Data.ShortVector.Internal.SIMD256" 256
  writeFile "src/Data/ShortVector/Internal/SIMD512.hs" $ unlines $ genFile "Data.ShortVector.Internal.SIMD512" 512

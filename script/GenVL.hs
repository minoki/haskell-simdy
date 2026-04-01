import qualified Data.List as List

infixr 5 <+>
(<+>) :: String -> String -> String
s <+> t = s ++ ' ' : t

commaSep :: [String] -> String
commaSep = List.intercalate ", "

spaceSep :: [String] -> String
spaceSep = List.intercalate " "

properVecTypes :: [String]
properVecTypes = ["X2","X4","X8","X16","X32","X64"]

intishTypes :: [String]
intishTypes = ["Int8", "Int16", "Int32", "Int64", "Word8", "Word16", "Word32", "Word64"]

genMod :: String -> [String] -> String -> String
genMod name imports comment = unlines $
  ["-- This file was created by script/GenVL.hs. Do not edit by hand!"
  ,comment
  ,"{-# LANGUAGE CPP #-}"
  ,"{-# LANGUAGE DataKinds #-}"
  ,"{-# LANGUAGE MonoLocalBinds #-}"
  ,"{-# LANGUAGE QuantifiedConstraints #-}"
  ,"#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 0, 0)"
  ,"{-# LANGUAGE RequiredTypeArguments #-}"
  ,"#endif"
  ,"module " ++ name
  ,"  ( module M"
  ,"  , SIMD (horizontalFold)"
  ,"  , SIMDElement"
  ,"  , broadcast"
  ,"  , liftSIMD"
  ,"  , liftSIMD2"
  ,"  , selectSIMD"
  ,"  , SIMDEq"
  ,"  , (==^)"
  ,"  , (/=^)"
  ,"  , SIMDOrd"
  ,"  , (<^)"
  ,"  , (<=^)"
  ,"  , (>^)"
  ,"  , (>=^)"
  ,"  , SIMDNum"
  ,"  , SIMDFractional"
  ,"  , SIMDFloating"
  ,"  , SIMDBoolean"
  ,"  , SIMDBits"
  ,"  , SIMDMinMax"
  ,"  , SIMDFMA"
  ,"  , SIMDEnumFromZero"
  ,"  , SIMDPrim"
  ,"  , SIMDStorable"
  ,"#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 0, 0)"
  ] ++
  ["  , unaryShuffle" ++ x | x <- properVecTypes] ++
  ["  , binaryShuffle" ++ x | x <- properVecTypes] ++
  ["#endif"] ++
  ["  , unaryShuffleWith" ++ x | x <- properVecTypes] ++
  ["  , binaryShuffleWith" ++ x | x <- properVecTypes] ++
  ["  ) where"
  ,"import           Data.Bits (Bits)"
  ,"import           Data.Coerce (coerce)"
  ,"import           Data.Complex"
  ,"import           Data.Functor.Identity"
  ,"import           Data.Int"
  ,"import           Data.Primitive"
  ,"import           Data.Proxy (Proxy)"
  ,"import           Data.Semigroup"
  ,"import           Data.Simdy.Internal.Bits (Boolean, BitShift)"
  ,"import           Data.Simdy.Internal.Class hiding (broadcast, liftSIMD, liftSIMD2)"
  ,"import qualified Data.Simdy.Internal.Class as I"
  ,"import           Data.Simdy.Internal.Shuffle"
  ] ++ imports ++
  -- ["-- import qualified Data.Vector.Unboxed as VU"] ++
  ["import           Data.Word"
  ,"import           Foreign.Storable"
  ,"import           Prelude hiding (not, (==), (/=), (<), (<=), (>), (>=))"
  ,""
  ,"-- | Constraint on element types that can be stored in SIMD vectors"
  ,"-- (e.g. 'Int32', 'Float', 'Double')."
  ,"--"
  ,"-- An instance of 'SIMDElement' supports basic SIMD operations (pack\\/unpack\\/broadcast)"
  ,"class ( " ++ List.intercalate "\n      , "
   (["Pack" ++ x <+> x ++ " a" | x <- properVecTypes] ++
    ["Broadcast " ++ x  ++ " a" | x <- properVecTypes] ++
    ["SplitShortVector " ++ x  ++ " a" | x <- properVecTypes] ++
    ["SelectableF " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDElement a"
  ] ++ ["instance SIMDElement " ++ a | a <- ["Bool"] ++ intishTypes ++ ["Float","Double"]] ++
  ["instance SIMDElement a => SIMDElement (Sum a)"
  ,"instance SIMDElement a => SIMDElement (Product a)"
  ,"instance SIMDElement a => SIMDElement (Min a)"
  ,"instance SIMDElement a => SIMDElement (Max a)"
  ,"instance SIMDElement a => SIMDElement (Complex a)"
  ,"instance SIMDElement ()"
  ,"instance (SIMDElement a0, SIMDElement a1) => SIMDElement (a0, a1)"
  ,"instance (SIMDElement a0, SIMDElement a1, SIMDElement a2) => SIMDElement (a0, a1, a2)"
  ,"instance (SIMDElement a0, SIMDElement a1, SIMDElement a2, SIMDElement a3) => SIMDElement (a0, a1, a2, a3)"
  ,"instance (SIMDElement a0, SIMDElement a1, SIMDElement a2, SIMDElement a3, SIMDElement a4) => SIMDElement (a0, a1, a2, a3, a4)"
  ,"instance (SIMDElement a0, SIMDElement a1, SIMDElement a2, SIMDElement a3, SIMDElement a4, SIMDElement a5) => SIMDElement (a0, a1, a2, a3, a4, a5)"
  ,""
  ,"-- | Constraint for element types that support lane-wise equality comparison."
  ,"--"
  ,"-- @('SIMD' f, 'SIMDEq' a)@ implies @'Equatable' (f a)@."
  ,"class ( " ++ List.intercalate "\n      , "
   (["Eq a", "SIMDElement a"] ++
    ["EquatableF " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDEq a"
  ] ++ ["instance SIMDEq " ++ a | a <- intishTypes ++ ["Float","Double"]] ++
  [""
  ,"-- | Constraint for element types that support lane-wise ordering comparison."
  ,"--"
  ,"-- @('SIMD' f, 'SIMDOrd' a)@ implies @'Ordered' (f a)@."
  ,"class ( " ++ List.intercalate "\n      , "
   (["Ord a", "SIMDEq a"] ++
    ["OrderedF " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDOrd a"
  ] ++ ["instance SIMDOrd " ++ a | a <- intishTypes ++ ["Float","Double"]] ++
  [""
  ,"-- | Constraint for element types that support lane-wise arithmetic ('Num')."
  ,"--"
  ,"-- @('SIMD' f, 'SIMDNum' a)@ implies @'Num' (f a)@."
  ,"class ( " ++ List.intercalate "\n      , "
   (["Num a", "SIMDElement a"] ++
    ["NumF " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDNum a"
  ] ++ ["instance SIMDNum " ++ a | a <- intishTypes ++ ["Float","Double"]] ++
  ["-- instance (RealFloat a, SIMDNum a) => SIMDNum (Complex a)"
  ,""
  ,"-- | Constraint for element types that support lane-wise 'Fractional' operations."
  ,"--"
  ,"-- @('SIMD' f, 'SIMDFractional' a)@ implies @'Fractional' (f a)@."
  ,"class ( " ++ List.intercalate "\n      , "
   (["Fractional a", "SIMDNum a"] ++
    ["FractionalF " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDFractional a"
  ,"instance SIMDFractional Float"
  ,"instance SIMDFractional Double"
  ,"-- instance (RealFloat a, SIMDFractional a) => SIMDFractional (Complex a)"
  ,""
  ,"-- | Constraint for element types that support lane-wise 'Floating' operations."
  ,"--"
  ,"-- @('SIMD' f, 'SIMDFloating' a)@ implies @'Floating' (f a)@."
  ,"class ( " ++ List.intercalate "\n      , "
   (["Floating a", "SIMDFractional a"] ++
    ["FloatingF " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDFloating a"
  ,"instance SIMDFloating Float"
  ,"instance SIMDFloating Double"
  ,"-- instance (RealFloat a, SIMDFloating a) => SIMDFloating (Complex a)"
  ,""
  ,"-- | Constraint for element types that support lane-wise bitwise logic ('Boolean')."
  ,"--"
  ,"-- @('SIMD' f, 'SIMDBoolean' a)@ implies @'Boolean' (f a)@."
  ,"class ( " ++ List.intercalate "\n      , "
   (["Bits a", "Boolean a", "SIMDElement a"] ++
    ["BooleanF " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDBoolean a"
  ] ++ ["instance SIMDBoolean " ++ a | a <- "Bool" : intishTypes] ++
  [""
  ,"-- | Constraint for element types that support lane-wise bit shifts ('BitShift')."
  ,"--"
  ,"-- @('SIMD' f, 'SIMDBits' a)@ implies @'BitShift' (f a)@."
  ,"class ( " ++ List.intercalate "\n      , "
   (["Bits a", "BitShift a", "SIMDBoolean a"] ++
    ["BitShiftF " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDBits a"
  ] ++ ["instance SIMDBits " ++ a | a <- intishTypes] ++
  [""
  ,"-- | Constraint for element types that support lane-wise 'MinMax'."
  ,"--"
  ,"-- @('SIMD' f, 'SIMDMinMax' a)@ implies @'MinMax' (f a)@."
  ,"class ( " ++ List.intercalate "\n      , "
   (["MinMax a", "SIMDElement a"] ++
    ["MinMaxF " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDMinMax a"
  ] ++ ["instance SIMDMinMax " ++ a | a <- intishTypes ++ ["Float","Double"]] ++
  [""
  ,"-- | Constraint for element types that support lane-wise FMA in SIMD vectors."
  ,"class ( " ++ List.intercalate "\n      , "
   (["FusedMultiplyAdd a", "SIMDNum a"] ++
    ["FusedMultiplyAddF " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDFMA a"
  ,"instance HasFMA => SIMDFMA Float"
  ,"instance HasFMA => SIMDFMA Double"
  ,""
  ,"class ( " ++ List.intercalate "\n      , "
   (["Num a", "SIMDElement a"] ++
    ["EnumFromZero " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDEnumFromZero a"
  ] ++ ["instance SIMDEnumFromZero " ++ a | a <- intishTypes ++ ["Float","Double"]] ++
  [""
  ,"-- | Constraint for element types that support reading\\/writing via 'Data.Primitive.Prim'."
  ,"class ( " ++ List.intercalate "\n      , "
   (["Prim a", "SIMDElement a"] ++
    ["MultiPrim " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDPrim a"
  ] ++ ["instance SIMDPrim " ++ a | a <- intishTypes ++ ["Float","Double"]] ++
  [""
  ,"-- | Constraint for element types that support reading\\/writing via 'Foreign.Storable.Storable'."
  ,"class ( " ++ List.intercalate "\n      , "
   (["Storable a", "SIMDElement a"] ++
    ["MultiStorable " ++ x  ++ " a" | x <- properVecTypes]
   ) ++ "\n      ) => SIMDStorable a"
  ] ++ ["instance SIMDStorable " ++ a | a <- intishTypes ++ ["Float","Double"]] ++
  [""
  ,"-- | SIMD vector types. @SIMD f@ implies that @f@"
  ,"-- supports broadcasting, element-wise lifting, comparison, arithmetic, etc."
  ,"class ( KnownSIMDLength f"
  ,"      , LiftConstructor f"
  ,"      , forall a. SIMDElement a => Broadcast f a"
  ,"      , forall a b. (SIMDElement a, SIMDElement b) => LiftSIMD f a b"
  ,"      , forall a b c. (SIMDElement a, SIMDElement b, SIMDElement c) => LiftSIMD2 f a b c"
  ,"      , forall a. MaskIsLiftedBool f a"
  ,"      , forall a. SIMDElement a => Selectable (f a)"
  ,"      , forall a. SIMDEq a => Equatable (f a)"
  ,"      , forall a. SIMDOrd a => Ordered (f a)"
  ,"      , forall a. SIMDNum a => Num (f a)"
  ,"      , forall a. SIMDFractional a => Fractional (f a)"
  ,"      , forall a. SIMDFloating a => Floating (f a)"
  ,"      , forall a. SIMDBoolean a => Boolean (f a)"
  ,"      , forall a. SIMDBits a => BitShift (f a)"
  ,"      , forall a. SIMDMinMax a => MinMax (f a)"
  ,"      , forall a. SIMDFMA a => FusedMultiplyAdd (f a)"
  ,"      , forall a. SIMDEnumFromZero a => EnumFromZero_ f a"
  ,"      , forall a. SIMDPrim a => MultiPrim f a"
  ,"      , forall a. SIMDStorable a => MultiStorable f a"
  ,"      ) => SIMD f where"
  ,"  -- | Reduce all lanes of a SIMD vector using a binary combining function."
  ,"  -- The function is applied via recursive halving (splitting the vector in half"
  ,"  -- and combining until a scalar remains)."
  ,"  horizontalFold :: SIMDElement a => (forall g. SIMD g => g a -> g a -> g a) -> f a -> a"
  ,"instance SIMD Identity where"
  ,"  horizontalFold _ = runIdentity"
  ,"  {-# INLINE horizontalFold #-}"
  ,"instance SIMD X2 where"
  ,"  horizontalFold op !v = case splitShortVector v of (low, high) -> runIdentity (op low high)"
  ,"  {-# INLINE horizontalFold #-}"
  ] ++ concat [
    ["instance SIMD " ++ x ++ " where"
    ,"  horizontalFold op !v = case splitShortVector v of (low, high) -> horizontalFold op (op low high)"
    ,"  {-# INLINE horizontalFold #-}"
    ] | x <- properVecTypes List.\\ ["X2"]] ++
  [""
  ,"-- | Create a SIMD vector with all lanes set to the same value."
  ,"--"
  ,"-- Conceptually, @'broadcast' x = mkX/N/ x x x ... x@."
  ,"broadcast :: (SIMD f, SIMDElement a) => a -> f a"
  ,"broadcast = I.broadcast"
  ,"{-# INLINE broadcast #-}"
  ,""
  ,"-- | Apply a scalar function element-wise to a SIMD vector."
  ,"--"
  ,"-- In general, the resulting function does not use SIMD instructions."
  ,"liftSIMD :: (SIMD f, SIMDElement a, SIMDElement b) => (a -> b) -> f a -> f b"
  ,"liftSIMD = I.liftSIMD"
  ,"{-# INLINE [1] liftSIMD #-}"
  ,""
  ,"-- | Apply a binary scalar function element-wise to two SIMD vectors."
  ,"--"
  ,"-- In general, the resulting function does not use SIMD instructions."
  ,"liftSIMD2 :: (SIMD f, SIMDElement a, SIMDElement b, SIMDElement c) => (a -> b -> c) -> f a -> f b -> f c"
  ,"liftSIMD2 = I.liftSIMD2"
  ,"{-# INLINE [1] liftSIMD2 #-}"
  ,""
  ,"-- | Lane-wise conditional selection"
  ,"--"
  ,"-- @selectSIMD mask trueVec falseVec@ picks lanes from @trueVec@ where"
  ,"-- the mask is true and from @falseVec@ where it is false."
  ,"selectSIMD :: (SIMD f, SIMDElement a)"
  ,"           => f Bool -- ^ condition"
  ,"           -> f a -- ^ then-expression"
  ,"           -> f a -- ^ else-expression"
  ,"           -> f a"
  ,"selectSIMD = select"
  ,"{-# INLINE selectSIMD #-}"
  ,""
  ,"infix 4 ==^, /=^"
  ,""
  ,"(==^) :: (SIMD f, SIMDEq a) => f a -> f a -> f Bool"
  ,"(==^) = (==)"
  ,"{-# INLINE (==^) #-}"
  ,""
  ,"(/=^) :: (SIMD f, SIMDEq a) => f a -> f a -> f Bool"
  ,"(/=^) = (/=)"
  ,"{-# INLINE (/=^) #-}"
  ,""
  ,"infix 4 <^, <=^, >^, >=^"
  ,""
  ,"(<^) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool"
  ,"(<^) = (<)"
  ,"{-# INLINE (<^) #-}"
  ,""
  ,"(<=^) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool"
  ,"(<=^) = (<=)"
  ,"{-# INLINE (<=^) #-}"
  ,""
  ,"(>^) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool"
  ,"(>^) = (>)"
  ,"{-# INLINE (>^) #-}"
  ,""
  ,"(>=^) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool"
  ,"(>=^) = (>=)"
  ,"{-# INLINE (>=^) #-}"
  ,""
  ,"#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 0, 0)"
  ] ++ concat (List.intersperse [""] $
    [["unaryShuffle" ++ x ++ " :: " ++ x ++ " a -> forall t -> UnaryShuffle (Tuple" ++ show n ++ "ToList t) " ++ x ++ " a => " ++ x ++ " a"
    ,"unaryShuffle" ++ x ++ " v t = unaryShuffle @(Tuple" ++ show n ++ "ToList t) v"
    ,"{-# INLINE unaryShuffle" ++ x ++ " #-}"
    ] | (x,n) <- zip properVecTypes [2,4,8,16,32,64]] ++
    [["binaryShuffle" ++ x ++ " :: " ++ x ++ " a -> " ++ x ++ " a -> forall t -> BinaryShuffle (Tuple" ++ show n ++ "ToList t) " ++ x ++ " a => " ++ x ++ " a"
    ,"binaryShuffle" ++ x ++ " u v t = binaryShuffle @(Tuple" ++ show n ++ "ToList t) u v"
    ,"{-# INLINE binaryShuffle" ++ x ++ " #-}"
    ] | (x,n) <- zip properVecTypes [2,4,8,16,32,64]]) ++
  ["#endif",""] ++
  concat (List.intersperse [""] $
    [["unaryShuffleWith" ++ x ++ " :: forall " ++ spaceSep ["i" ++ show i | i <- [0..n-1]] ++ " a. UnaryShuffle '[" ++ commaSep ["i" ++ show i | i <- [0..n-1]] ++ "] " ++ x ++ " a => ((" ++ commaSep ["Proxy " ++ show i | i <- [0..n-1]] ++ ") -> (" ++ commaSep ["Proxy i" ++ show i | i <- [0..n-1]] ++ ")) -> " ++ x ++ " a -> " ++ x ++ " a"
    ,"unaryShuffleWith" ++ x ++ " _ = unaryShuffle @'[" ++ commaSep ["i" ++ show i | i <- [0..n-1]] ++ "]"
    ,"{-# INLINE unaryShuffleWith" ++ x ++ " #-}"
    ] | (x,n) <- zip properVecTypes [2,4,8,16,32,64]] ++
    [["binaryShuffleWith" ++ x ++ " :: forall " ++ spaceSep ["i" ++ show i | i <- [0..n-1]] ++ " a. BinaryShuffle '[" ++ commaSep ["i" ++ show i | i <- [0..n-1]] ++ "] " ++ x ++ " a => ((" ++ commaSep ["Proxy " ++ show i | i <- [0..n-1]] ++ ") -> (" ++ commaSep ["Proxy " ++ show i | i <- [n..2*n-1]] ++ ") -> (" ++ commaSep ["Proxy i" ++ show i | i <- [0..n-1]] ++ ")) -> " ++ x ++ " a -> " ++ x ++ " a -> " ++ x ++ " a"
    ,"binaryShuffleWith" ++ x ++ " _ = binaryShuffle @'[" ++ commaSep ["i" ++ show i | i <- [0..n-1]] ++ "]"
    ,"{-# INLINE binaryShuffleWith" ++ x ++ " #-}"
    ] | (x,n) <- zip properVecTypes [2,4,8,16,32,64]]) ++
  [""
  ,"{-# RULES"] ++
  do
    (n,f) <- [("Sum","mkSum")
             ,("getSum","getSum'")
             ,("Product","mkProduct")
             ,("getProduct","getProduct'")
             ,("Min","mkMin")
             ,("getMin","getMin'")
             ,("Max","mkMax")
             ,("getMax","getMax'")
             ]
    x <- properVecTypes
    ["\"liftSIMD/" ++ n ++ "/" ++ x ++ "\"","  liftSIMD coerce = " ++ f ++ " @" ++ x]
  ++ do
    (n,lhs,rhs) <- [("Complex","(:+)","mkComplex")
                   ,("(,)","(,)","mkTuple2")
                   ]
    x <- properVecTypes
    ["\"liftSIMD2/" ++ n ++ "/" ++ x ++ "\"","  liftSIMD2 " ++ lhs ++ " = " ++ rhs ++ " @" ++ x]
  ++ ["  #-}"]

main :: IO ()
main = do
  writeFile "src-no-simd/Data/Simdy/Internal/NoSIMD.hs" $ genMod "Data.Simdy.Internal.NoSIMD"
    ["import           Data.Simdy.Internal.NoSIMD.HalfVector ()"
    ,"import           Data.Simdy.Internal.NoSIMD.X16 as M"
    ,"import           Data.Simdy.Internal.NoSIMD.X2 as M"
    ,"import           Data.Simdy.Internal.NoSIMD.X32 as M"
    ,"import           Data.Simdy.Internal.NoSIMD.X4 as M"
    ,"import           Data.Simdy.Internal.NoSIMD.X64 as M"
    ,"import           Data.Simdy.Internal.NoSIMD.X8 as M"
    ]
    "{-|\n\
    \This module contains types and classes that do not use SIMD primitives.\n\
    \\n\
    \In general, the types and classes exported from this module are not compatible with other modules with different vector lengths (i.e. \"Data.Simdy.Internal.SIMD128\", \"Data.Simdy.Internal.SIMD256\", \"Data.Simdy.Internal.SIMD512\").\n\
    \-}"
  writeFile "src-vl128/Data/Simdy/Internal/SIMD128.hs" $ genMod "Data.Simdy.Internal.SIMD128"
    ["import           Data.Simdy.Internal.SIMD128.HalfVector ()"
    ,"import           Data.Simdy.Internal.SIMD128.X16 as M"
    ,"import           Data.Simdy.Internal.SIMD128.X2 as M"
    ,"import           Data.Simdy.Internal.SIMD128.X32 as M"
    ,"import           Data.Simdy.Internal.SIMD128.X4 as M"
    ,"import           Data.Simdy.Internal.SIMD128.X64 as M"
    ,"import           Data.Simdy.Internal.SIMD128.X8 as M"
    ]
    "{-|\n\
    \This module contains types and classes that use 128-bit vectors (x86 SSE, Arm NEON).\n\
    \\n\
    \In general, the types and classes exported from this module are not compatible with other modules with different vector lengths (i.e. \"Data.Simdy.Internal.NoSIMD\", \"Data.Simdy.Internal.SIMD256\", \"Data.Simdy.Internal.SIMD512\").\n\
    \-}"
  writeFile "src-vl256/Data/Simdy/Internal/SIMD256.hs" $ genMod "Data.Simdy.Internal.SIMD256"
    ["import           Data.Simdy.Internal.SIMD128.X2 as M"
    ,"import           Data.Simdy.Internal.SIMD256.HalfVector ()"
    ,"import           Data.Simdy.Internal.SIMD256.X16 as M"
    ,"import           Data.Simdy.Internal.SIMD256.X32 as M"
    ,"import           Data.Simdy.Internal.SIMD256.X4 as M"
    ,"import           Data.Simdy.Internal.SIMD256.X64 as M"
    ,"import           Data.Simdy.Internal.SIMD256.X8 as M"
    ]
    "{-|\n\
    \This module contains types and classes that use 256-bit vectors (x86 AVX/AVX2).\n\
    \\n\
    \In general, the types and classes exported from this module are not compatible with other modules with different vector lengths (i.e. \"Data.Simdy.Internal.NoSIMD\", \"Data.Simdy.Internal.SIMD128\", \"Data.Simdy.Internal.SIMD512\").\n\
    \-}"
  writeFile "src-vl512/Data/Simdy/Internal/SIMD512.hs" $ genMod "Data.Simdy.Internal.SIMD512"
    ["import           Data.Simdy.Internal.SIMD128.X2 as M"
    ,"import           Data.Simdy.Internal.SIMD256.X4 as M"
    ,"import           Data.Simdy.Internal.SIMD512.HalfVector ()"
    ,"import           Data.Simdy.Internal.SIMD512.X16 as M"
    ,"import           Data.Simdy.Internal.SIMD512.X32 as M"
    ,"import           Data.Simdy.Internal.SIMD512.X64 as M"
    ,"import           Data.Simdy.Internal.SIMD512.X8 as M"
    ]
    "{-|\n\
    \This module contains types and classes that use 512-bit vectors (x86 AVX-512).\n\
    \\n\
    \In general, the types and classes exported from this module are not compatible with other modules with different vector lengths (i.e. \"Data.Simdy.Internal.NoSIMD\", \"Data.Simdy.Internal.SIMD128\", \"Data.Simdy.Internal.SIMD256\").\n\
    \-}"

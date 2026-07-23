-- This file was created by script/Gen.hs. Do not edit by hand!
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Simdy.Internal.Class.Generated where
-- | Pack\/unpack 2-lane SIMD vectors from\/to individual scalar elements.
class PackX2 x a where
  mkX2 :: a -> a -> x a
  unpackX2 :: x a -> (a, a)
pattern MkX2 :: PackX2 x a => a -> a -> x a
pattern MkX2 x0 x1 <- (unpackX2 -> (x0, x1)) where
  MkX2 = mkX2
packX2 :: PackX2 x a => (a, a) -> x a
packX2 (a0, a1) = mkX2 a0 a1
toListX2 :: PackX2 x a => x a -> [a]
toListX2 v = case unpackX2 v of
  (a0, a1) -> [a0, a1]
fromListX2 :: PackX2 x a => [a] -> x a
fromListX2 [a0, a1] = mkX2 a0 a1
fromListX2 xs | length xs < 2 = error "fromListX2: List too short"
              | otherwise = error "fromListX2: List too long"
-- | Pack\/unpack 4-lane SIMD vectors from\/to individual scalar elements.
class PackX4 x a where
  mkX4 :: a -> a -> a -> a -> x a
  unpackX4 :: x a -> (a, a, a, a)
pattern MkX4 :: PackX4 x a => a -> a -> a -> a -> x a
pattern MkX4 x0 x1 x2 x3 <- (unpackX4 -> (x0, x1, x2, x3)) where
  MkX4 = mkX4
packX4 :: PackX4 x a => (a, a, a, a) -> x a
packX4 (a0, a1, a2, a3) = mkX4 a0 a1 a2 a3
toListX4 :: PackX4 x a => x a -> [a]
toListX4 v = case unpackX4 v of
  (a0, a1, a2, a3) -> [a0, a1, a2, a3]
fromListX4 :: PackX4 x a => [a] -> x a
fromListX4 [a0, a1, a2, a3] = mkX4 a0 a1 a2 a3
fromListX4 xs | length xs < 4 = error "fromListX4: List too short"
              | otherwise = error "fromListX4: List too long"
-- | Pack\/unpack 8-lane SIMD vectors from\/to individual scalar elements.
class PackX8 x a where
  mkX8 :: a -> a -> a -> a -> a -> a -> a -> a -> x a
  unpackX8 :: x a -> (a, a, a, a, a, a, a, a)
pattern MkX8 :: PackX8 x a => a -> a -> a -> a -> a -> a -> a -> a -> x a
pattern MkX8 x0 x1 x2 x3 x4 x5 x6 x7 <- (unpackX8 -> (x0, x1, x2, x3, x4, x5, x6, x7)) where
  MkX8 = mkX8
packX8 :: PackX8 x a => (a, a, a, a, a, a, a, a) -> x a
packX8 (a0, a1, a2, a3, a4, a5, a6, a7) = mkX8 a0 a1 a2 a3 a4 a5 a6 a7
toListX8 :: PackX8 x a => x a -> [a]
toListX8 v = case unpackX8 v of
  (a0, a1, a2, a3, a4, a5, a6, a7) -> [a0, a1, a2, a3, a4, a5, a6, a7]
fromListX8 :: PackX8 x a => [a] -> x a
fromListX8 [a0, a1, a2, a3, a4, a5, a6, a7] = mkX8 a0 a1 a2 a3 a4 a5 a6 a7
fromListX8 xs | length xs < 8 = error "fromListX8: List too short"
              | otherwise = error "fromListX8: List too long"
-- | Pack\/unpack 16-lane SIMD vectors from\/to individual scalar elements.
class PackX16 x a where
  mkX16 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> x a
  unpackX16 :: x a -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
pattern MkX16 :: PackX16 x a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> x a
pattern MkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 <- (unpackX16 -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)) where
  MkX16 = mkX16
packX16 :: PackX16 x a => (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a) -> x a
packX16 (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = mkX16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
toListX16 :: PackX16 x a => x a -> [a]
toListX16 v = case unpackX16 v of
  (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) -> [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15]
fromListX16 :: PackX16 x a => [a] -> x a
fromListX16 [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15] = mkX16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
fromListX16 xs | length xs < 16 = error "fromListX16: List too short"
               | otherwise = error "fromListX16: List too long"
-- | Pack\/unpack 32-lane SIMD vectors from\/to individual scalar elements.
class PackX32 x a where
  mkX32 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> x a
  unpackX32 :: x a -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
pattern MkX32 :: PackX32 x a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> x a
pattern MkX32 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 <- (unpackX32 -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31)) where
  MkX32 = mkX32
packX32 :: PackX32 x a => (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a) -> x a
packX32 (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31) = mkX32 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
toListX32 :: PackX32 x a => x a -> [a]
toListX32 v = case unpackX32 v of
  (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31) -> [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31]
fromListX32 :: PackX32 x a => [a] -> x a
fromListX32 [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31] = mkX32 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
fromListX32 xs | length xs < 32 = error "fromListX32: List too short"
               | otherwise = error "fromListX32: List too long"
-- | Pack\/unpack 64-lane SIMD vectors from\/to individual scalar elements.
class PackX64 x a where
  mkX64 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> x a
  unpackX64 :: x a -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
pattern MkX64 :: PackX64 x a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> x a
pattern MkX64 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 <- (unpackX64 -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63)) where
  MkX64 = mkX64
packX64 :: PackX64 x a => (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a) -> x a
packX64 (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60, a61, a62, a63) = mkX64 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63
toListX64 :: PackX64 x a => x a -> [a]
toListX64 v = case unpackX64 v of
  (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60, a61, a62, a63) -> [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60, a61, a62, a63]
fromListX64 :: PackX64 x a => [a] -> x a
fromListX64 [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60, a61, a62, a63] = mkX64 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63
fromListX64 xs | length xs < 64 = error "fromListX64: List too short"
               | otherwise = error "fromListX64: List too long"

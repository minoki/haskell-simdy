# simdy -- The Haskell library for SIMD programming

This library provides wrappers around GHC's SIMD primitives.

## Data types

`simdy` defines data types for SIMD vectors:

```haskell
module Data.Simdy where

data X2 a
data X4 a
data X8 a
data X16 a
data X32 a
data X64 a
```

Conceptually, these types are fixed-length lists, but with more efficient representations. For example, `X4 a` is isomorphic to `(a, a, a, a)`, but `X4 Float` may use `FloatX4#` and `X4 Double` may use two `DoubleX2#`s internally.

They are instances of the `SIMD` type class:

```haskell
module Data.Simdy where

class SIMD x
instance SIMD Identity
instance SIMD X2
instance SIMD X4
instance SIMD X8
instance SIMD X16
instance SIMD X32
instance SIMD X64
```

Vector types can be constructed using functions from `Data.Simdy.Pack`:

```haskell
module Data.Simdy.Pack where

mkX2 :: a -> a -> X2 a
packX2 :: (a, a) -> X2 a
unpackX2 :: X2 a -> (a, a)
pattern MkX2 :: a -> a -> X2 a

mkX4 :: a -> a -> a -> a -> X4 a
packX4 :: (a, a, a, a) -> X4 a
unpackX4 :: X4 a -> (a, a, a, a)
pattern MkX4 :: a -> a -> a -> a -> X4 a

mkX8 :: a -> a -> a -> a -> a -> a -> a -> a -> X8 a
packX8 :: (a, a, a, a, a, a, a, a) -> X8 a
unpackX8 :: X8 a -> (a, a, a, a, a, a, a, a)
pattern MkX8 :: a -> a -> a -> a -> a -> a -> a -> a -> X8 a

mkX16 :: a -> ... -> a -> X16 a
packX16 :: (a, ..., a) -> X16 a
unpackX16 :: X16 a -> (a, ..., a)
pattern MkX16 :: a -> ... -> a -> X16 a

mkX32 :: a -> ... -> a -> X32 a
packX32 :: (a, ..., a) -> X32 a
unpackX32 :: X32 a -> (a, ..., a)
pattern MkX32 :: a -> ... -> a -> X32 a

mkX64 :: a -> ... -> a -> X64 a
packX64 :: (a, ..., a) -> X64 a
unpackX64 :: X64 a -> (a, ..., a)
pattern MkX64 :: a -> ... -> a -> X64 a
```

## Operations

Element types such as `Float` and `Word16` are instances of the `SIMDElement` class. Basic operations are provided, including `broadcast` (similar to `replicate n x`), `liftSIMD` (lifting a unary function to a vector; similar to `map`), `liftSIMD2` (lifting a binary function to a vector; similar to `zipWith`).

```haskell
module Data.Simdy where

class SIMDElement a
broadcast :: (SIMD x, SIMDElement a) => a -> x a
liftSIMD :: (SIMD x, SIMDElement a, SIMDElement b) => (a -> b) -> x a -> x b
liftSIMD2 :: (SIMD x, SIMDElement a, SIMDElement b, SIMDElement c)
          => (a -> b -> c) -> x a -> x b -> x c
selectSIMD :: (SIMD x, SIMDElement a) => x Bool -> x a -> x a -> x a

```

Note: `liftSIMD` and `liftSIMD2` use scalar operations, so they may not be
as efficient as SIMD-native operations.

Equality and comparisons are handled by the `SIMDEq` and `SIMDOrd` classes. Comparison operators return a vector of `Bool`s (`x Bool`) instead of a plain `Bool`.

```haskell
module Data.Simdy where

class SIMDElement a => SIMDEq a
(==^) :: (SIMD x, SIMDEq a) => x a -> x a -> x Bool
(/=^) :: (SIMD x, SIMDEq a) => x a -> x a -> x Bool

class SIMDEq a => SIMDOrd a
(<^) :: (SIMD x, SIMDOrd a) => x a -> x a -> x Bool
(<=^) :: (SIMD x, SIMDOrd a) => x a -> x a -> x Bool
(>^) :: (SIMD x, SIMDOrd a) => x a -> x a -> x Bool
(>=^) :: (SIMD x, SIMDOrd a) => x a -> x a -> x Bool
```

For arithmetic operations, the standard type classes `Num`, `Fractional`, and `Floating` can be used when the element type is an instance of `SIMDNum`, `SIMDFractional`, and `SIMDFloating`, respectively.

```haskell
module Data.Simdy where

class SIMDElement a => SIMDNum a
instance (SIMD x, SIMDNum a) => Num (x a) -- pseudocode

class SIMDNum a => SIMDFractional a
instance (SIMD x, SIMDFractional a) => Fractional (x a) -- pseudocode

class SIMDFractional a => SIMDFloating a
instance (SIMD x, SIMDFloating a) => Floating (x a) -- pseudocode
```

For bitwise operations, the library defines a subset of the `Bits` class as `Boolean` and `BitShift`:

```haskell
module Data.Simdy.Bits where

class Boolean a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  xor :: a -> a -> a
  complement :: a -> a

class Boolean a => BitShift a where
  shiftL :: a -> Int -> a
  unsafeShiftL :: a -> Int -> a
  shiftR :: a -> Int -> a
  unsafeShiftR :: a -> Int -> a

module Data.Simdy where

class SIMDElement a => SIMDBoolean a
instance (SIMD x, SIMDBoolean a) => Boolean (x a) -- pseudocode

class SIMDBoolean a => SIMDBits a
instance (SIMD x, SIMDBits a) => BitShift (x a) -- pseudocode
```

## Supported compilers and architectures

This library requires a modern GHC (9.6 at minimum). SIMD functionality is only available on certain architectures. However, you can always use a non-SIMD implementation of the data types by enabling the `no-simd` package flag.

Supported architectures are:

* x86_64 with
    * LLVM backend with GHC 9.6 or newer
    * NCG backend with GHC 9.12 or newer (128-bit vectors only)
* AArch64 with
    * LLVM backend with GHC 9.12 or newer (128-bit vectors only)
    * NCG backend with GHC 10.0 or newer (128-bit vectors only)
* Non-SIMD implementation with any architecture

In general, programs using simdy should be compiled with `-fllvm` to take advantage of LLVM.

## Package flags

This library uses 128-bit SIMD vectors by default (SSE2 on x86, ASIMD on AArch64). To use 256-bit vectors with AVX or 512-bit vectors with AVX-512, enable the corresponding package flags. SIMD support from GHC 9.12+'s NCG backend can also be enabled by disabling the `llvm` package flag.

* `no-simd` (default: false)
    * Disable the use of native vector types.
* `haswell` (default: false)
    * Enable AVX2 and FMA.
    * Enable 256-bit vectors with the LLVM backend.
    * Requires Intel Haswell / AMD Ryzen or newer.
* `avx512` (default: false)
    * Enable AVX-512 (F+VL+BW+DQ are required).
    * Enable 512-bit vectors with the LLVM backend.
* `llvm` (default: true)
    * Use the LLVM backend to compile *the library*. Users also need to use `-fllvm` to take full advantage of LLVM.

## For developers

Some files are generated by a script. Run `cabal run script/Gen.hs` after cloning the repository.

-- |
-- Bitwise operation classes for SIMD vectors and scalar types.
--
-- 'Boolean' provides bitwise AND, OR, XOR, and complement.
-- 'BitShift' extends 'Boolean' with shift operations.
--
-- These classes mirror "Data.Bits" but are designed to work uniformly
-- across scalar types and SIMD vector types.
module Data.Simdy.Bits
  ( Boolean (..)
  , BitShift (..)
  , SIMDBoolean
  , SIMDBits
  ) where
import           Data.Simdy.Internal.Bits
import           Data.Simdy.Internal.Default

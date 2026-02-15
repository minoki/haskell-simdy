{-# LANGUAGE PatternSynonyms #-}
-- |
-- Pack and unpack individual scalar elements into\/from SIMD vectors.
--
-- Each vector width has:
--
-- * A class (e.g. 'PackX4') with 'mkX4' (pack from values) and 'unpackX4' (unpack to a tuple).
-- * A pattern synonym (e.g. 'MkX4') for bidirectional matching.
-- * A function (e.g. 'packX4') taking a tuple.
module Data.Simdy.Pack
  ( -- * 2-lane vectors
    PackX2 (mkX2, unpackX2)
  , pattern MkX2
  , packX2
    -- * 4-lane vectors
  , PackX4 (mkX4, unpackX4)
  , pattern MkX4
  , packX4
    -- * 8-lane vectors
  , PackX8 (mkX8, unpackX8)
  , pattern MkX8
  , packX8
    -- * 16-lane vectors
  , PackX16 (mkX16, unpackX16)
  , pattern MkX16
  , packX16
    -- * 32-lane vectors
  , PackX32 (mkX32, unpackX32)
  , pattern MkX32
  , packX32
    -- * 64-lane vectors
  , PackX64 (mkX64, unpackX64)
  , pattern MkX64
  , packX64
  ) where
import           Data.Simdy.Internal.Class

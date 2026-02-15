{-# LANGUAGE PatternSynonyms #-}
-- |
-- Lift tuple and newtype constructors over SIMD vectors.
--
-- 'LiftConstructor' allows constructing and deconstructing \"structure-of-arrays\"
-- representations: a vector of pairs is stored as a pair of vectors, etc.
--
-- Bidirectional pattern synonyms ('MkTuple2', ..., 'MkTuple6') provide convenient
-- construction and pattern matching.
module Data.Simdy.Tuple
  ( LiftConstructor (mkTuple2, mkTuple3, mkTuple4, mkTuple5, mkTuple6, deconstructTuple2, deconstructTuple3, deconstructTuple4, deconstructTuple5, deconstructTuple6)
  , pattern MkTuple2
  , pattern MkTuple3
  , pattern MkTuple4
  , pattern MkTuple5
  , pattern MkTuple6
  ) where
import           Data.Simdy.Internal.Class

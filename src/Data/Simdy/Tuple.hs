{-# LANGUAGE PatternSynonyms #-}
module Data.Simdy.Tuple
  ( LiftConstructor (mkTuple2, mkTuple3, mkTuple4, mkTuple5, mkTuple6, deconstructTuple2, deconstructTuple3, deconstructTuple4, deconstructTuple5, deconstructTuple6)
  , pattern MkTuple2
  , pattern MkTuple3
  , pattern MkTuple4
  , pattern MkTuple5
  , pattern MkTuple6
  ) where
import           Data.Simdy.Internal.Class

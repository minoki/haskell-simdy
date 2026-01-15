module Data.Simdy.Mask
  ( Boolean (..)
  , Mask
  , selectSIMD
  , Selectable (..)
  , Equatable (..)
  , (==^)
  , (/=^)
  , Ordered (..)
  , (<^)
  , (<=^)
  , (>^)
  , (>=^)
  ) where
import           Data.Simdy.Class.Bits
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default

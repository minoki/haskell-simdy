{-# LANGUAGE PatternSynonyms #-}
module Data.Simdy.Pack
  ( PackX2 (mkX2, unpackX2)
  , pattern MkX2
  , packX2
  , PackX4 (mkX4, unpackX4)
  , pattern MkX4
  , packX4
  , PackX8 (mkX8, unpackX8)
  , pattern MkX8
  , packX8
  , PackX16 (mkX16, unpackX16)
  , pattern MkX16
  , packX16
  , PackX32 (mkX32, unpackX32)
  , pattern MkX32
  , packX32
  ) where
import           Data.Simdy.Internal.Class

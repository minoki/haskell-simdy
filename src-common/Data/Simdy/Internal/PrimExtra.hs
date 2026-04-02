module Data.Simdy.Internal.PrimExtra where

foreign import ccall unsafe "hs_simdy_minimumFloat"
  minimumFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_maximumFloat"
  maximumFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_minimumNumberFloat"
  minimumNumberFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_maximumNumberFloat"
  maximumNumberFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_minimumDouble"
  minimumDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_maximumDouble"
  maximumDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_minimumNumberDouble"
  minimumNumberDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_maximumNumberDouble"
  maximumNumberDouble :: Double -> Double -> Double

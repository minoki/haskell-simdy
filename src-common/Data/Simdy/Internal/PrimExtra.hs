module Data.Simdy.Internal.PrimExtra where

foreign import ccall unsafe "hs_simdy_minimum_float"
  minimumFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_maximum_float"
  maximumFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_minimumNumber_float"
  minimumNumberFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_maximumNumber_float"
  maximumNumberFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_minimum_double"
  minimumDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_maximum_double"
  maximumDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_minimumNumber_double"
  minimumNumberDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_maximumNumber_double"
  maximumNumberDouble :: Double -> Double -> Double

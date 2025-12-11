module Interpolation (
    linear,
    newton,
    lagrange,
) where

-- TODO: implement
linear :: (Double, Double) -> (Double, Double) -> Double -> Double
linear (x0, y0) (x1, y1) x = y0 + (y1 - y0) * (x - x0) / (x1 - x0)

-- TODO: implement
newton :: Int
newton = 0

-- TODO: implement
lagrange :: Int
lagrange = 0

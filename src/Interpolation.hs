module Interpolation (
    linear,
    newton,
    lagrange,
) where

linear :: (Double, Double) -> (Double, Double) -> Double -> Double
linear (x0, y0) (x1, y1) x = y0 + (y1 - y0) * (x - x0) / (x1 - x0)

-- TODO: implement
newton :: Int
newton = 0

-- TODO: implement
lagrange :: [(Double, Double)] -> Double -> Double
lagrange points x = foldl (\acc (xi, yi) -> acc + yi * l xi x) 0 points
  where
    l xj x = foldl (\acc (xi, _) -> if xi == xj then acc else acc * (x - xi) / (xj - xi)) 1 points

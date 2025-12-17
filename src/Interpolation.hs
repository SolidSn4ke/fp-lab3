module Interpolation (
    linear,
    newton,
    newtonSameStep,
    lagrange,
) where

linear :: (Double, Double) -> (Double, Double) -> Double -> Double
linear (x0, y0) (x1, y1) x = y0 + (y1 - y0) * (x - x0) / (x1 - x0)

newton :: [(Double, Double)] -> Double -> Double
newton points x =
    sum $
        map
            ( \i -> case table !! i of
                [] -> 0
                (k : _) -> k * mul (take i points)
            )
            [0 .. length points - 1]
  where
    lvl0 = map snd points
    lvl prev =
        let
            y1 i = prev !! (i + 1)
            y0 i = prev !! i
            x1 i = fst $ points !! (i + 1 + (length points - length prev))
            x0 i = fst $ points !! i
         in
            map (\i -> (y1 i - y0 i) / (x1 i - x0 i)) [0 .. (length prev - 2)]
    dividedDiff acc
        | length (last acc) == 1 = acc
        | otherwise = dividedDiff (acc ++ [lvl (last acc)])
    table = dividedDiff [lvl0]
    mul = foldl (\acc (xi, _) -> acc * (x - xi)) 1

newtonSameStep :: [(Double, Double)] -> Double -> Double
newtonSameStep points x = foldl (\acc i -> acc + head (table !! i) * mul (fromIntegral i) / factorial (fromIntegral i)) 0 [0 .. length points - 1]
  where
    h = case points of
        (p1 : p2 : _) -> fst p2 - fst p1
    t = case points of
        (p : _) -> (x - fst p) / h
    factorial n = product [1 .. n]
    lvl prev = map (\i -> prev !! (i + 1) - prev !! i) [0 .. length prev - 2]
    diff acc
        | length (last acc) == 1 = acc
        | otherwise = diff $ acc ++ [lvl (last acc)]
    table = diff [map snd points]
    mul n = foldl (\acc i -> acc * (t - i)) 1 [0 .. n - 1]

lagrange :: [(Double, Double)] -> Double -> Double
lagrange points x = foldl (\acc (xi, yi) -> acc + yi * l xi) 0 points
  where
    l xj = foldl (\acc (xi, _) -> if xi == xj then acc else acc * (x - xi) / (xj - xi)) 1 points

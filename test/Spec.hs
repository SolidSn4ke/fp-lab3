module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Interpolation

eps :: Double
eps = 1e-9

approxEq :: Double -> Double -> Double -> Bool
approxEq e a b = abs (a - b) < e

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Interpolation tests"
        [ linearTests,
          lagrangeTests,
          newtonTests,
          newtonSameStepTests,
          polynomialExactnessTests
        ]

linearTests :: TestTree
linearTests =
    testGroup
        "linear"
        [ testCase "midpoint" $
            linear (0, 0) (2, 2) 1 @?= 1
        ]

lagrangeTests :: TestTree
lagrangeTests =
    testGroup
        "lagrange"
        [ testCase "matches nodes exactly" $
            let pts = [(-1, 1), (0, 0), (1, 1)]
             in map (\(x, _) -> lagrange pts x) pts
                    @?= map snd pts,
          testCase "reconstructs x^2" $
            let pts = [(-1, 1), (0, 0), (1, 1)]
                f x = x * x
             in assertBool "approx equal" $
                    approxEq eps (lagrange pts 0.5) (f 0.5)
        ]

newtonTests :: TestTree
newtonTests =
    testGroup
        "newton"
        [ testCase "matches nodes exactly" $
            let pts = [(-1, 1), (0, 0), (1, 1)]
             in map (\(x, _) -> newton pts x) pts
                    @?= map snd pts,
          testCase "matches Lagrange" $
            let pts = [(-1, 1), (0, 0), (1, 1)]
                x = 0.3
             in assertBool "newton == lagrange" $
                    approxEq eps (newton pts x) (lagrange pts x)
        ]

newtonSameStepTests :: TestTree
newtonSameStepTests =
    testGroup
        "newtonSameStep"
        [ testCase "matches nodes exactly" $
            let pts = [(-3, 8), (-2, 4), (-1, 1), (0, 0), (1, 1)]
             in map (\(x, _) -> newtonSameStep pts x) pts
                    @?= map snd pts,
          testCase "matches general Newton" $
            let pts = [(-3, 8), (-2, 4), (-1, 1), (0, 0), (1, 1)]
                xs = [-2.5, -1.5, -0.5, 0.5]
             in map
                    ( \x ->
                        approxEq
                            eps
                            (newtonSameStep pts x)
                            (newton pts x)
                    )
                    xs
                    @?= replicate (length xs) True
        ]

polynomialExactnessTests :: TestTree
polynomialExactnessTests =
    testGroup
        "polynomial exactness"
        [ testCase "cubic polynomial reconstructed exactly" $
            let f x = x ** 3 - 2 * x ** 2 + x - 1
                pts = [(x, f x) | x <- [-2, -1, 0, 1]]
                xs = [-1.5, -0.5, 0.5]
             in map (\x -> approxEq eps (newton pts x) (f x)) xs
                    @?= replicate (length xs) True
        ]

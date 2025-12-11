module Main (main) where

import Commands
import Interpolation
import Subscribers
import System.IO
import Text.Regex.Posix

main :: IO ()
main = do
    putStrLn "Program is running. Enter \"help\" to see list of commands"
    inputLoop []

inputPrompt :: IO ()
inputPrompt = do
    putStr "> "
    hFlush stdout

inputLoop :: [Subscriber] -> IO ()
inputLoop subs = do
    inputPrompt
    input <- getLine
    resolve input
  where
    resolve str
        | str == "" = inputLoop subs
        | str == "exit" = putStrLn "Closing application"
        | str == "EOF" = do
            putStrLn $ "< " ++ "Stopping streaming mode"
            inputLoop []
        | (2 == length (words str)) && ((length . filter (=~ "-?([1-9]\\d*[.,]\\d+|[1-9]\\d*|0[.,]\\d+|0)") $ words str) == 2) = do
            let x = read $ head (words str) :: Double
            let y = read $ words str !! 1 :: Double
            mapM_ (\sub -> action sub (state sub) (x, y)) subs
            inputLoop subs
        | otherwise = do
            newSubs <- invoker (words str) subs
            inputLoop newSubs

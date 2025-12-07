module Main (main) where

import Commands
import Interpolation
import System.IO

main :: IO ()
main = do
    putStrLn "Program is running. Enter \"help\" to see list of commands"
    inputLoop

inputPrompt :: IO ()
inputPrompt = do
    putStr "> "
    hFlush stdout

inputLoop :: IO ()
inputLoop = do
    inputPrompt
    input <- getLine
    resolve input
  where
    resolve str
        | str == "" = inputLoop
        | str == "exit" = putStrLn "Closing application"
        | otherwise = do
                putStrLn $ (++) "< " $ invoker $ words str
                inputLoop

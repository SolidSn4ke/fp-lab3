module Commands (invoker) where

import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe)
import Interpolation
import Subscribers
import System.Exit (exitSuccess)
import Text.Printf (printf)

data Command = Command {name :: String, execute :: [String] -> [Subscriber] -> IO [Subscriber], description :: String}

listOfCommands :: [Command]
listOfCommands = [helpCommand, linearCommand, lagrangeCommand, newtonCommand, eofCommand, exitCommand]
  where
    helpCommand = Command{name = "help", execute = executeHelp, description = "help - see list of commands"}
    linearCommand = Command{name = "linear", execute = executeLinear, description = "linear <step> - linear interpolation with a specific step. Works in streaming mode. Enter \"eof\" to stop"}
    lagrangeCommand = Command{name = "lagrange", execute = executeLagrange, description = "lagrange <n> <step> - lagrange interpolation with n points and a specific step. Works in streaming mode. Enter \"eof\" to stop"}
    newtonCommand = Command{name = "newton", execute = executeNewton, description = "newton <n> <step> - newton interpolation with n points and a specific step. Works in streaming mode. Enter \"eof\" to stop"}
    eofCommand = Command{name = "eof", execute = executeEOF, description = "eof - stop streaming mode"}
    exitCommand = Command{name = "exit", execute = executeExit, description = "exit - close application"}

invoker :: [String] -> [Subscriber] -> IO [Subscriber]
invoker args subs = execute command args subs
  where
    command = fromMaybe Command{execute = notFound, name = "", description = ""} $ find (\c -> name c == commandName) listOfCommands
    commandName = case args of
        [] -> ""
        (s : _) -> map toLower s
    notFound _ s = do
        putStrLn $ printf "Command \"%s\" not found. Enter \"help\" to see list of commands" commandName
        return s

executeHelp :: [String] -> [Subscriber] -> IO [Subscriber]
executeHelp _ subs = do
    putStrLn $ foldl (\acc c -> acc ++ "\n" ++ description c) "Avaliable commands:" listOfCommands
    return subs

executeExit :: [String] -> [Subscriber] -> IO [Subscriber]
executeExit _ _ = do
    putStrLn "< Closing application"
    exitSuccess

executeEOF :: [String] -> [Subscriber] -> IO [Subscriber]
executeEOF _ _ = do
    putStrLn "< Stopping streaming mode"
    return []

executeLinear :: [String] -> [Subscriber] -> IO [Subscriber]
executeLinear args subs =
    if length args < 2
        then putStrLn "< Too few args to execute" >> return subs
        else do
            let step = read (args !! 1) :: Double
            st <- newIORef ([], 0)
            let sub =
                    Subscriber
                        { subName = "linear",
                          action = linearAction step,
                          state = st
                        }
            putStrLn "< Subscriber has been added"
            return (sub : subs)
  where
    linearAction :: Double -> IORef ([(Double, Double)], Double) -> (Double, Double) -> IO ()
    linearAction step_ stateRef point = do
        (points, start) <- readIORef stateRef
        if null points
            then modifyIORef stateRef (\(p, _) -> (point : p, fst point))
            else do
                let prev = case points of
                        [] -> (0, 0)
                        (p : _) -> p
                if fst point < start + step_
                    then return ()
                    else do
                        putStrLn $ "< linear: " ++ show (linear prev point (start + step_))
                        modifyIORef stateRef (\(p, s) -> (p, s + step_))
                        if fst point >= start + 2 * step_
                            then linearAction step_ stateRef point
                            else modifyIORef stateRef (first (point :))

executeLagrange :: [String] -> [Subscriber] -> IO [Subscriber]
executeLagrange args subs =
    if length args < 3
        then putStrLn "< Too few args to execute" >> return subs
        else do
            let n = read (args !! 1) :: Int
            let step = read (args !! 2) :: Double
            st <- newIORef ([], 0)
            let sub =
                    Subscriber
                        { subName = "lagrange",
                          action = lagrangeAction n step,
                          state = st
                        }
            putStrLn "< Subscriber has been added"
            return (sub : subs)
  where
    lagrangeAction :: Int -> Double -> IORef ([(Double, Double)], Double) -> (Double, Double) -> IO ()
    lagrangeAction n_ step_ stateRef point = do
        (points, start) <- readIORef stateRef
        resolve (length points) points start
      where
        resolve len points start
            | len == 0 = do modifyIORef stateRef (\(p, _) -> (point : p, fst point))
            | len < n_ = do modifyIORef stateRef (first (point :))
            | otherwise = do
                let prevs = take n_ points
                if fst point < start + step_
                    then return ()
                    else do
                        putStrLn $ "< lagrange: " ++ show (lagrange prevs (start + step_))
                        modifyIORef stateRef (\(p, s) -> (p, s + step_))
                        if fst point >= start + 2 * step_
                            then lagrangeAction n_ step_ stateRef point
                            else modifyIORef stateRef (first (point :))

executeNewton :: [String] -> [Subscriber] -> IO [Subscriber]
executeNewton args subs =
    if length args < 3
        then putStrLn "< Too few args to execute" >> return subs
        else do
            let n = read (args !! 1) :: Int
            let step = read (args !! 2) :: Double
            st <- newIORef ([], 0)
            let sub =
                    Subscriber
                        { subName = "newton",
                          action = newtonAction n step,
                          state = st
                        }
            putStrLn "< Subscriber has been added"
            return (sub : subs)
  where
    newtonAction :: Int -> Double -> IORef ([(Double, Double)], Double) -> (Double, Double) -> IO ()
    newtonAction n_ step_ stateRef point = do
        (points, start) <- readIORef stateRef
        resolve (length points) points start
      where
        resolve len points start
            | len == 0 = do modifyIORef stateRef (\(p, _) -> (point : p, fst point))
            | len < n_ = do modifyIORef stateRef (first (point :))
            | otherwise = do
                let prevs = take n_ points
                if fst point < start + step_
                    then return ()
                    else do
                        putStrLn $ "< newton: " ++ show (if checkStep prevs then newtonSameStep prevs (start + step_) else newton prevs (start + step_))
                        modifyIORef stateRef (\(p, s) -> (p, s + step_))
                        if fst point >= start + 2 * step_
                            then newtonAction n_ step_ stateRef point
                            else modifyIORef stateRef (first (point :))
        checkStep points =
            let
                p1 = case points of
                    [] -> (0, 0)
                    (p : _) -> p
                p2 = points !! 1
                step = fst p2 - fst p1
             in
                map fst points == [fst p1, fst p1 + step .. fst p1 + step * fromIntegral (n_ - 1)]

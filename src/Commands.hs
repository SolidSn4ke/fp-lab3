module Commands (invoker) where

import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe)
import Interpolation
import Subscribers
import Text.Printf (printf)
import Data.Bifunctor (first)

data Command = Command {name :: String, execute :: [String] -> [Subscriber] -> IO [Subscriber], description :: String}

listOfCommands :: [Command]
listOfCommands = [helpCommand, linearCommand, newtonCommand, exitCommand]
  where
    helpCommand = Command{name = "help", execute = executeHelp, description = "help - see list of commands"}
    linearCommand = Command{name = "linear", execute = executeLinear, description = "linear <step> - linear interpolation with a specific step. Works in streaming mode. Enter \"EOF\" to stop"}
    newtonCommand = Command{name = "newton", description = "newton <step> - newton interpolation with a specific step. Works in streaming mode. Enter \"EOF\" to stop"}
    exitCommand = Command{name = "exit", description = "exit - close application"}

invoker :: [String] -> [Subscriber] -> IO [Subscriber]
invoker args subs = execute command args subs
  where
    command = fromMaybe Command{execute = notFound} $ find (\c -> name c == commandName) listOfCommands
    commandName = head args
    notFound _ s = do
        putStrLn $ printf "Command \"%s\" not found. Enter \"help\" to see list of commands" commandName
        return s

executeHelp :: [String] -> [Subscriber] -> IO [Subscriber]
executeHelp _ subs = do
    putStrLn $ foldl (\acc c -> acc ++ "\n" ++ description c) "Avaliable commands:" listOfCommands
    return subs

executeLinear :: [String] -> [Subscriber] -> IO [Subscriber]
executeLinear args subs =
    if length args < 2
        then putStrLn "too few args" >> return subs
        else do
            let step = read (args !! 1) :: Double
            st <- newIORef ([], 0)
            let sub =
                    Subscriber
                        { subName = "linear",
                          action = linearAction step,
                          state = st
                        }
            putStrLn "ok"
            return (sub : subs)
  where
    linearAction :: Double ->  IORef ([(Double, Double)], Double) -> (Double, Double) -> IO ()
    linearAction step_ stateRef dot = do
        (dots, start) <- readIORef stateRef
        if null dots
            then modifyIORef stateRef (\(d, s) -> (dot : d, fst dot))
            else do
                let prev = head dots
                if fst dot < start + step_
                    then return ()
                    else do
                      putStrLn $ "< linear: " ++ show (linear prev dot (start + step_))
                      modifyIORef stateRef (\(d, s) -> (d, s + step_))
                      if fst dot >= start + 2 * step_
                        then linearAction step_ stateRef dot
                        else modifyIORef stateRef (first (dot :))

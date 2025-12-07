module Commands (invoker) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

data Command = Command {name :: String, execute :: [String] -> String, description :: String}

listOfCommands :: [Command]
listOfCommands = [helpCommand, linearCommand, newtonCommand, exitCommand]
  where
    helpCommand = Command{name = "help", execute = executeHelp, description = "test"}
    linearCommand = Command{name = "linear", description = "linear"}
    newtonCommand = Command{name = "newton", description = "newton"}
    exitCommand = Command{name = "exit", description = "exit"}

invoker :: [String] -> String
invoker args = execute command args
  where
    command = fromMaybe Command{execute = notFound} $ find (\c -> name c == commandName) listOfCommands
    commandName = head args
    notFound _ = printf "Command \"%s\" not found. Enter \"help\" to see list of commands" commandName

executeHelp :: [String] -> String
executeHelp _ = foldl (\acc c -> acc ++ "\n" ++ description c) "" listOfCommands

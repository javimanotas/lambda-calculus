module Repl ( Result(..), Command(..), isCommand, parseCommand, descriptions ) where

import LambdaExpr

data Result = Definition String LambdaExpr
            | Expr LambdaExpr

data Command = Quit
              | Help
              | Load
              | Print
              | Save
              | Remove
              deriving (Eq, Enum, Bounded)

type Args = [String]


commands :: [(String, Command)]
commands = zip [":q", ":?", ":l", ":p", ":s", ":rm"] $ enumFrom minBound


descriptions :: [String]
descriptions = [ ":?                     help"
               , ":q                     quit"
               , ":p  <name>             prints a defined value without evaluating it"
               , ":l  <file1> <file2>... loads the files into the current REPL sesion"
               , ":s  <file>             saves all the defined variables into a file"
               , ":rm <var1> <var2>...   deletes saved variables from the enviroment"]


isCommand :: String -> Bool
isCommand (':':_) = True
isCommand _       = False


parseCommand :: String -> (Maybe Command, Args)
parseCommand line = (command `lookup` commands, args)
    where
        (command:args) = words line
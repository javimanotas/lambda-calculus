module Repl ( Result(..), CommandT(..), isCommand, parseCommand, descriptions ) where

import LambdaExpr

data Result = Definition String LambdaExpr
            | Expr LambdaExpr

data CommandT = Quit
              | Help
              | Load
              | Print
              | Save
              | Remove
              deriving (Enum, Bounded)

commands :: [(String, CommandT)]
commands = zip [":q", ":?", ":l", ":p", ":s", ":rm"] $ enumFrom minBound

descriptions :: [String]
descriptions = [ ":?                    help"
               , ":q                    quit"
               , ":p <name>             prints a defined value without evaluating it"
               , ":l <file1> <file2>... loads the files into the current REPL sesion"
               , ":s file               saves all the defined variables into a file"
               , ":rm <Var1> <Var2>...  deletes saved variables from the enviroment"]

type Args = [String]

type Command = (CommandT, [String])

isCommand :: String -> Bool
isCommand (':':_) = True
isCommand _       = False

parseCommand :: String -> Maybe Command
parseCommand line = (, args) <$> command `lookup` commands
    where
        (command:args) = words line
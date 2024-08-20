module Repl ( Command(..), isCommand, parseCommand, descriptions ) where

data Command = Quit
             | Help
             | Load
             | Print
             | Norm
             | Save
             | Remove
             | Env
             deriving (Eq, Enum, Bounded)

type Args = [String]


commands :: [(String, Command)]
commands = zip [":q", ":?", ":l", ":p", ":norm", ":s", ":rm", ":env"] $ enumFrom minBound

descriptions :: [String]
descriptions = [ ":?                      help"
               , ":q                      quit"
               , ":p  <name1> <name2>...  prints defined values without evaluating them"
               , ":norm <var1> <var2>     assings to the variables the normal form of their content (usefull for displaying matches)"
               , ":l  <file1> <file2>...  loads the files into the current REPL sesion"
               , ":s  <file>              saves all the defined variables into a file"
               , ":rm <var1> <var2>...    deletes saved variables from the enviroment"
               , ":rm -all                deletes all saved variables from the enviroment"
               , ":env                    shows all the variables of the enviroment"]

isCommand :: String -> Bool
isCommand (':':_) = True
isCommand _       = False

parseCommand :: String -> (Maybe Command, Args)
parseCommand line = case words line of
    (command:args) -> (command `lookup` commands, args)
    _ -> error "Unreachable pattern not detected by the compiler"
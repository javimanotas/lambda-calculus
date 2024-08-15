{-# LANGUAGE FlexibleContexts #-}
module Parser ( parseLine, InputResult(..) ) where

import LambdaExpr
import qualified Enviroment as Env
import Eval

import Control.Monad
import qualified Data.Set as Set
import Text.Parsec
import Control.Arrow

type Parser = Parsec String ()

data InputResult = Definition String LambdaExpr
                 | Evaluate LambdaExpr


parseLine :: String -> Env.Env -> Either String InputResult
parseLine line env = do
    parsed <- left show $ parse parser "" line
    replaceDefinitions env parsed


parser :: Parser InputResult
parser = between spaces eof parse
    where
        lambda = str "\\" <|> str "λ"
        dot    = str "."
        expr   = abstr <|> appl

        abstr = do
            params <- between lambda dot (many1 $ identifier lower)
            body <- expr
            return $ foldr Abstr body params
        
        appl  = foldl1 Appl <$> many1 ((Var <$> identifier letter) <|> between (str "(") (str ")") expr)

        parse = try (Definition <$> definition <*> expr) <|> (Evaluate <$> expr)


definition :: Parser String
definition = do
    hd <- upper -- Ensure the first char is a capital letter
    tl <- many letter
    spaces
    string "="
    spaces
    return $ hd:tl


identifier :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m Identifier
identifier f = read <$> many1 f <* spaces


str :: Stream s m Char => String -> ParsecT s u m ()
str x = string x >> spaces


replaceDefinitions :: Env.Env -> InputResult -> Either String InputResult
replaceDefinitions env (Definition s l) = Definition s <$> replace' env l
replaceDefinitions env (Evaluate l)     = Evaluate <$> replace' env l


replace' :: Env.Env -> LambdaExpr -> Either String LambdaExpr
replace' env expr = foldM replaceVar expr $ definedVars expr
    where
        definedVars :: LambdaExpr -> Set.Set Identifier
        definedVars (Var x)
            | isDefinition x    = Set.singleton x
            | otherwise         = Set.empty
        definedVars (Appl x y)  = definedVars x `Set.union` definedVars y
        definedVars (Abstr _ y) = definedVars y
        
        replaceVar :: LambdaExpr -> Identifier -> Either String LambdaExpr
        replaceVar lambda var = case Env.get (show var) env of
            Nothing -> Left $ "Undefined variable " ++ show var
            Just expr -> Right $ replace var expr lambda
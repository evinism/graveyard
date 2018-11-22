module ParserExperiment 
    ( statementParser
    ) where

import Text.Parsec
import Text.Parsec.Char (upper)

type Identifier = String
data Expression = 
    IdExpr Identifier 
    | AppExpr Expression Expression 
    | AbstrExpr Identifier Expression
    deriving (Show)

nameParser :: Parsec String () Identifier
nameParser = (count 1 lower) <|> (many1 upper)

identParser :: Parsec String () Expression
identParser = do
    name <- nameParser
    return $ IdExpr name

abstractionParser :: Parsec String () Expression
abstractionParser = do
    string "\\"
    ident <- nameParser
    string "."
    expr <- expressionParser
    return $ AbstrExpr ident expr

parenExprParser :: Parsec String () Expression
parenExprParser = do
    string "("
    expr <- expressionParser
    string ")"
    return expr

nonApplicationParser :: Parsec String () Expression
nonApplicationParser = abstractionParser <|> identParser <|> parenExprParser

expressionParser :: Parsec String () Expression
expressionParser = chainl1 nonApplicationParser $ return AppExpr

statementParser :: Parsec String () Expression
statementParser = do
    expr <- expressionParser
    eof
    return expr

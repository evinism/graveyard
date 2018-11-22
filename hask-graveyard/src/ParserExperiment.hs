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

lambdaCharParser :: Parsec String () ()
lambdaCharParser = do
    string "\\"
    return ()

dotParser :: Parsec String () ()
dotParser = do
    string "."
    return ()

openParenParser :: Parsec String () ()
openParenParser = do
    string "("
    return ()

closeParenParser :: Parsec String () ()
closeParenParser = do
    string ")"
    return ()

nameParser :: Parsec String () Identifier
nameParser = (count 1 lower) <|> (many1 upper)

identParser :: Parsec String () Expression
identParser = do
    name <- nameParser
    return $ IdExpr name

abstractionParser :: Parsec String () Expression
abstractionParser = do
    lambdaCharParser
    ident <- nameParser
    dotParser
    expr <- expressionParser
    return $ AbstrExpr ident expr

parenExprParser :: Parsec String () Expression
parenExprParser = do
    openParenParser
    expr <- expressionParser
    closeParenParser
    return expr

nonApplicationParser :: Parsec String () Expression
nonApplicationParser = abstractionParser  <|> identParser <|> parenExprParser

expressionParser :: Parsec String () Expression
expressionParser = do
    nonApps <- many1 nonApplicationParser
    return $ case nonApps of
        [] -> error "wat"
        [x] -> x
        x:xs -> foldl AppExpr x xs

statementParser = do
    expr <- expressionParser
    eof
    return expr
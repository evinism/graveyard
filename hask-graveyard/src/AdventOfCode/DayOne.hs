module AdventOfCode.DayOne where

import AdventOfCode.Util
import Text.Parsec
import Data.Functor (($>))
import Data.Set (Set, insert, member, empty)

-- Start Parser
data Parity = Positive | Negative deriving (Show)

parseLine :: Parsec String () Int
parseLine = do
    par <- (string "+" $> Positive) <|> (string "-" $> Negative)
    val <- read <$> many1 digit
    return $ case par of
        Positive -> val
        Negative -> -val

parseInput :: Parsec String () [ Int ]
parseInput = parseLine `sepEndBy` (string "\n")

runD1Parser :: String -> [ Int ]
runD1Parser str = case parse parseInput "Input" str of
    Right vals -> vals
    Left _ -> error "Nope"

-- Parser Done --

solutiond1p1 :: String -> String
solutiond1p1 str = show $ foldr (+) 0 $ runD1Parser str

rund1p1 :: IO ()
rund1p1 = runSoln solutiond1p1 "data/AdventOfCode/d1p1.txt" 


-- Visited, current --
type FreqState = (Set Int)

findNextRepeated :: FreqState -> Int -> [ Int ] -> Int
findNextRepeated _ _ [] = error "end of list!!" -- This is dumb but whatever
findNextRepeated set current (delta : deltaTail) =
    let nextFreq = current + delta in
        if member current set
            then current
            else findNextRepeated (insert current set) nextFreq deltaTail

findRepeatedState :: [ Int ] -> Int
findRepeatedState = (findNextRepeated empty 0)

solutiond1p2 :: String -> String
solutiond1p2 = show . findRepeatedState . cycle . runD1Parser


rund1p2 :: IO ()
rund1p2 = runSoln solutiond1p2 "data/AdventOfCode/d1p1.txt" 


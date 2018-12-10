module AdventOfCode.DayTwo where

import qualified Data.Map.Strict as M
import AdventOfCode.Util
import Data.List.Extras.Argmax (argmax)

type CharacterVect = M.Map Char Int
addCharacterToVect :: Char -> CharacterVect -> CharacterVect
addCharacterToVect char charVect = 
    M.insertWith (+) char 1 charVect

wordCharacterVect :: String -> CharacterVect
wordCharacterVect = foldr addCharacterToVect M.empty

hasNRepeating :: Int -> String -> Bool
hasNRepeating n word =
    M.size (M.filter (== n) $ wordCharacterVect word) > 0

nRepeatingCount :: Int -> [String] -> Int
nRepeatingCount n wordList = length $ filter (hasNRepeating n) wordList

checksum :: [String] -> Int
checksum wordList = 
    (nRepeatingCount 2 wordList) * (nRepeatingCount 3 wordList)

solutiond2p1 :: String -> String
solutiond2p1 = show . checksum . lines
    
rund2p1 :: IO ()
rund2p1 = runSoln solutiond2p1 "data/AdventOfCode/d2p1.txt" 

-- Second Part --

getEditDistance :: String -> String -> Int
getEditDistance [] [] = 0
getEditDistance (charA : restA) (charB : restB) =
    let dist = if charA /= charB then 1 else 0
        in dist + getEditDistance restA restB
getEditDistance xs ys = (length xs) + (length ys)

pair :: (Eq a) => [a] -> [(a, a)]
pair xs = filter (\(a, b) -> a /= b) [(x,y) | x <- xs, y <- xs]

findFirstSingleDistance :: [String] -> (String, String)
findFirstSingleDistance xs = 
    let paired = pair xs in
        argmax (\(x, y) -> - (getEditDistance x y)) paired

solutiond2p2 :: String -> String
solutiond2p2 = show . findFirstSingleDistance . lines

rund2p2 :: IO ()
rund2p2 = runSoln solutiond2p2 "data/AdventOfCode/d2p1.txt" 
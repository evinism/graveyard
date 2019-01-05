module AdventOfCode.DayThree where
import Text.Parsec

type Id = Int
type Vect = (Int, Int)

data Entry = Entry
    { entryId :: Id
    , topLeft :: Vect
    , bottomRight :: Vect
    }

parseNumber :: Parsec String () Int
parseNumber = read <$> many1 digit

parseLine :: Parsec String () Entry
parseLine = do
    string "#"
    id' <- parseNumber 
    string " @ "
    patchPosX <- parseNumber
    string ","
    patchPosY <- parseNumber
    string ": "
    patchSizeX <- parseNumber
    string "X"
    patchSizeY <- parseNumber
    return $ Entry id' (patchPosX, patchPosY) (patchPosX + patchSizeX, patchPosY + patchSizeY)

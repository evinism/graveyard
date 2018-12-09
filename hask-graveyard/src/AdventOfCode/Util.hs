module AdventOfCode.Util where

runSoln :: (String -> String) -> String -> IO ()
runSoln solution fname = do
    fcontents <- readFile fname
    putStr $ (solution fcontents) ++ "\n"

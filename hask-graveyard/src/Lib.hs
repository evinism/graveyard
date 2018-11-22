module Lib
    ( someFunc
    ) where

import qualified Data.Text as T

greeting :: T.Text
greeting = "Hello, world!"

someFunc :: IO ()
someFunc = putStrLn $ T.unpack greeting 

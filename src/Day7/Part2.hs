module Day7.Part2 where

import           Control.Arrow
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Ord

solve :: String -> String
solve = undefined

parse :: String -> String
parse = undefined

main :: IO ()
main = do
  text <- readFile "input.txt"
  putStrLn $ show $ solve text

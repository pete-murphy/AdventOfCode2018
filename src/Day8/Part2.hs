module Day8.Part2 where

import           Data.Tree

solve :: Tree Int -> Int
solve = undefined

parse :: String -> Tree Int
parse = undefined

main :: IO ()
main = do
  text <- readFile "src/Day8/input.txt"
  putStrLn $ show $ solve $ parse text

module Day5.Part1 where

import           Data.Char

parse :: String -> String
parse = go []
  where
    go [] (y:ys) = go [y] ys
    go acc [] = reverse $ tail acc
    go acc@(a:as) (y:ys)
      | y `reactsWith` a = go as ys
      | otherwise = go (y : acc) ys
    reactsWith x y =
      (isUpper x && toLower x == y) || (isLower x && toUpper x == y)

solve :: String -> Int
solve = length . parse

main :: IO ()
main = do
  text <- readFile "src/Day5/input.txt"
  putStrLn $ show $ solve text

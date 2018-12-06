module Day5.Part1 where

import           Data.Char

parse :: String -> String
parse = foldr react ""

react :: Char -> String -> String
react c [] = [c]
react c (x:xs)
  | c `reactsWith` x = xs
  | otherwise = (c : x : xs)

go :: String -> String -> String
go [] (y:ys) = go [y] ys
go acc [] = reverse $ tail acc
go acc@(a:as) (y:ys)
  | y `reactsWith` a = go as ys
  | otherwise = go (y : acc) ys

reactsWith :: Char -> Char -> Bool
reactsWith x y = toLower x == y && x /= y

solve :: String -> Int
solve = length . parse

main :: IO ()
main = do
  text <- readFile "src/Day5/input.txt"
  putStrLn $ show $ solve text

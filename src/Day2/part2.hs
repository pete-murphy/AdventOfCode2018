module Day2.Part2 where

import           Data.List
import           Data.Maybe

diff :: String -> String -> String
diff [] _ = []
diff (x:xs) (y:ys)
  | x == y = x : diff xs ys
  | otherwise = diff xs ys

mapDiff :: String -> [String] -> Maybe String
mapDiff str strs =
  listToMaybe <$> filter (\xs -> length xs == 25) $ map (diff str) strs

solve' :: String -> [Maybe String]
solve' xs = go [] xs'
  where
    xs' = lines xs
    go acc []     = acc
    go acc (y:ys) = go (mapDiff y xs' : acc) ys

solve :: String -> String
solve = head . catMaybes . solve'

main :: IO ()
main = do
  text <- readFile "src/Day2/input.txt"
  putStrLn $ show $ head $ catMaybes $ solve' text

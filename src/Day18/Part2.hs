module Day18.Part2 where

import           Control.Arrow
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Day18.Part1   hiding (main, solve)
import           Util

solve' :: String -> [Int]
solve' t = resourceValue <$> iterate tick b
  where
    b = parse t

type Start = Int

accum :: Int -> [Int] -> (Start, [Int])
accum m = go 1 S.empty
  where
    go n set (x:xs)
      {- `m` is a magic number 🧙 -}
      | x `S.member` set && n > m = (n, x : takeWhile (\y -> y /= x) xs)
      | otherwise = go (n + 1) (S.insert x set) xs

solve = valueAfter 1000000000 . accum 500 . solve'

valueAfter :: Int -> (Int, [Int]) -> Int
valueAfter x (n, ys) = (!!) ys $ (x - n) `mod` length ys

main :: IO ()
main = do
  text <- readFile "src/Day18/input.txt"
  putStrLn $ show $ solve text

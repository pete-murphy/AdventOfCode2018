module Day7.Part1 where

import           Control.Arrow
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Ord

solve :: [(Char, String)] -> String
solve input = go [] (allChars input) []
    -- | In the base case, return the accumulated string
  where
    go acc [] [] = acc
    go acc (x:xs) unmatched =
      case M.lookup x deps of
        Nothing -> go (acc ++ [x]) (unmatched ++ xs) []
        Just cs
          | all (`elem` acc) cs -> go (acc ++ [x]) (unmatched ++ xs) []
          | otherwise -> go acc xs (unmatched ++ [x])
    deps = mapDependents input

mapDependents :: [(Char, String)] -> Map Char String
mapDependents = M.map (sort) . M.fromListWith (++)

allChars :: [(Char, String)] -> String
allChars = nub . sort . foldMap (uncurry (:))

parse :: String -> [(Char, String)]
parse = map parseLine . lines
  where
    parseLine :: String -> (Char, String)
    parseLine = select . words
    select (_:y:_:_:_:_:_:(x:_):_) = (x, y)

main :: IO ()
main = do
  text <- readFile "src/Day7/input.txt"
  putStrLn $ show $ solve $ parse text

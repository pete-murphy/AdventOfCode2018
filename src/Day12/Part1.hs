{-# LANGUAGE TupleSections #-}

module Day12.Part1 where

import           Control.Arrow
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe

data Pot
  = P
  | N
  deriving (Eq, Ord)

instance Show Pot where
  show P = "#"
  show N = "."

parse :: String -> [Pot]
parse []       = []
parse ('#':xs) = P : parse xs
parse ('.':xs) = N : parse xs
parse (_:xs)   = parse xs

parseInitialState :: String -> [Pot]
parseInitialState = parse . head . lines

parseRules :: String -> Pattern
parseRules = M.fromList . map parseLine . (drop 2) . lines
  where
    parseLine = (head &&& head . last) . map parse . splitOn " => "

bookend :: [Pot] -> [Pot]
bookend s = reverse $ N : N : N : (reverse $ N : N : N : s)

unbookend :: [Pot] -> [Pot]
unbookend s = drop 1 $ take (length s - 2) s

type Pattern = Map [Pot] Pot

evolve :: Pattern -> [Pot] -> [Pot]
evolve m ps = map ((M.!) m) (toFives $ bookend ps)
  where
    toFives :: [a] -> [[a]]
    toFives xs
      | length xs == 5 = [xs]
      | otherwise = [take 5 xs] ++ toFives (drop 1 xs)

evolve' :: Pattern -> [Pot] -> [Pot]
evolve' m ps = map (fromMaybe N . (M.!?) m) (toFives $ bookend ps)
  where
    toFives :: [a] -> [[a]]
    toFives xs
      | length xs == 5 = [xs]
      | otherwise = [take 5 xs] ++ toFives (drop 1 xs)

countP :: [Pot] -> Int
countP []     = 0
countP (P:xs) = 1 + countP xs
countP (_:xs) = countP xs

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

main :: IO ()
main = do
  text <- readFile "src/Day12/sample.txt"
  let initState = parseInitialState text
      patts = parseRules text
   in putStrLn $ concatMap show $ nTimes 0 (evolve' patts) initState

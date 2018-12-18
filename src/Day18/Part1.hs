{-# LANGUAGE LambdaCase #-}

module Day18.Part1 where

import           Control.Monad
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Util

data Acre
  = Open
  | Trees
  | Lumberyard
  deriving (Eq)

type Coord = (Int, Int)

type Board = Map Coord Acre

findNeighbors :: Set Coord -> Coord -> [Coord]
findNeighbors keys (u, v) = do
  x <- [u - 1 .. u + 1]
  y <- [v - 1 .. v + 1]
  guard $ (x, y) /= (u, v) && (x, y) `S.member` keys
  pure (x, y)

newAcre :: Board -> Coord -> Acre
newAcre b c =
  case b M.! c of
    Open
      | count Trees >= 3 -> Trees
      | otherwise -> Open
    Trees
      | count Lumberyard >= 3 -> Lumberyard
      | otherwise -> Trees
    Lumberyard
      | count Lumberyard >= 1 && count Trees >= 1 -> Lumberyard
      | otherwise -> Open
  where
    count :: Acre -> Int
    count a = length $ filter (== a) neighbors
    neighbors = map ((M.!) b) $ findNeighbors (M.keysSet b) c

tick :: Board -> Board
tick b = tick' (M.assocs b) M.empty
  where
    tick' [] acc          = acc
    tick' ((c, a):xs) acc = tick' xs $ M.insert c (newAcre b c) acc

charToAcre :: Char -> Acre
charToAcre =
  \case
    '.' -> Open
    '|' -> Trees
    '#' -> Lumberyard

resourceValue :: Board -> Int
resourceValue b = uncurry (*) $ M.foldr countTotals (0, 0) b
  where
    countTotals :: Acre -> (Int, Int) -> (Int, Int)
    countTotals a total@(trees, lumberyard) =
      case a of
        Trees      -> (trees + 1, lumberyard)
        Lumberyard -> (trees, lumberyard + 1)
        _          -> total

parse :: String -> Board
parse = M.fromList . withIndices . map (map charToAcre) . lines

solve :: String -> Int
solve t = resourceValue $ nTimes 10 tick b
  where
    b = parse t

main :: IO ()
main = do
  text <- readFile "src/Day18/input.txt"
  putStrLn $ show $ solve text

module Day18.Part1 where

import           Data.Map (Map)
import qualified Data.Map as M

data Acre
  = Open
  | Trees
  | Lumberyard
  deriving (Eq)

type Coord = (Int, Int)

type Board = Map Coord Acre

solve :: String -> Int
solve = undefined

main :: IO ()
main = do
  text <- readFile "src/Day18/input.txt"
  putStrLn $ show $ solve text

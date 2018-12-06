module Day6.Part1 where

import           Control.Arrow
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Set        (Set)
import qualified Data.Set

type Coord = (Int, Int)

newtype Location = L
  { getL :: Coord
  } deriving (Eq, Ord, Show)

type Territory = Set Coord

-- expandAll :: [[Coord]] -> [[Coord]]
-- expandAll css =
--
-- expand :: Coord -> [Coord]
-- expand (x, y) = [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]
findBounds :: [Location] -> (Coord, Coord)
findBounds xs =
  let xMin = fst $ getL $ minimum xs
      yMin = snd $ minimumVal $ getL <$> xs
      xMax = fst $ getL $ maximum xs
      yMax = snd $ maximumVal $ getL <$> xs
   in ((xMin, yMin), (xMax, yMax))

isInBounds :: Coord -> ((Int, Int), (Int, Int)) -> Bool
isInBounds (x, y) ((xMin, yMin), (xMax, yMax)) =
  x > xMin && x < xMax && y > yMin && y < yMax

findClosest :: Coord -> Maybe Location
findClosest = undefined

parse :: String -> Location
parse = L . (head &&& last) . map read . splitOn ", "

----- |||| ***************
solve :: String -> Int
solve _ = 17

----- |||| ***************
minimumValBy :: (b -> b -> Ordering) -> [(a, b)] -> (a, b)
minimumValBy c = minimumBy (c `on` snd)

maximumValBy :: (b -> b -> Ordering) -> [(a, b)] -> (a, b)
maximumValBy c = maximumBy (c `on` snd)

minimumVal :: Ord b => [(a, b)] -> (a, b)
minimumVal = minimumValBy compare

maximumVal :: Ord b => [(a, b)] -> (a, b)
maximumVal = maximumValBy compare

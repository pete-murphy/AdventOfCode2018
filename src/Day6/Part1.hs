module Day6.Part1 where

import           Control.Arrow
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Ord

type Coord = (Int, Int)

newtype Location = L
  { getL :: Coord
  } deriving (Eq, Ord, Show)

findClosest :: [Location] -> Coord -> Maybe Location
findClosest ls (cx, cy) =
  case sortOn dist ls of
    (x:y:_)
      | dist x == dist y -> Nothing
      | otherwise -> Just x
  where
    dist = uncurry (+) . (abs . subtract cx *** abs . subtract cy) . getL

findLargest :: [Location] -> [Maybe Location] -> Int
findLargest ls =
  length .
  last .
  sortOn length .
  group . sort . filter (isCandidate $ findBounds ls) . catMaybes

findBounds :: [Location] -> (Coord, Coord)
findBounds xs =
  let xMin = fst $ getL $ minimum xs
      yMin = snd $ minimumVal $ getL <$> xs
      xMax = fst $ getL $ maximum xs
      yMax = snd $ maximumVal $ getL <$> xs
   in ((xMin, yMin), (xMax, yMax))

genTerritory :: (Coord, Coord) -> [Coord]
genTerritory ((xMin, yMin), (xMax, yMax)) = do
  x <- [xMin .. xMax]
  y <- [yMin .. yMax]
  pure (x, y)

isCandidate :: (Coord, Coord) -> Location -> Bool
isCandidate ((xMin, yMin), (xMax, yMax)) (L (x, y)) =
  x > xMin && x < xMax && y > yMin && y < yMax

parseLine :: String -> Location
parseLine = L . (head &&& last) . map read . splitOn ", "

solve :: String -> Int
solve input =
  (findLargest allLocs) . map (findClosest allLocs) . genTerritory . findBounds $
  allLocs
  where
    getLocs = map parseLine . lines
    allLocs = getLocs input

minimumValBy :: (b -> b -> Ordering) -> [(a, b)] -> (a, b)
minimumValBy c = minimumBy (c `on` snd)

maximumValBy :: (b -> b -> Ordering) -> [(a, b)] -> (a, b)
maximumValBy c = maximumBy (c `on` snd)

minimumVal :: Ord b => [(a, b)] -> (a, b)
minimumVal = minimumValBy compare

maximumVal :: Ord b => [(a, b)] -> (a, b)
maximumVal = maximumValBy compare

main :: IO ()
main = do
  text <- readFile "input.txt"
  putStrLn $ show $ solve text

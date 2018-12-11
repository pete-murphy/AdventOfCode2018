{-# LANGUAGE TupleSections #-}

module Day11.Part1 where

import           Data.List
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe

type Coord = (Int, Int)

type Serial = Int

type PowerLevel = Int

calcPL :: Coord -> Serial -> PowerLevel
calcPL (cx, cy) s = ((((rackId * cy + s) * rackId) `div` 100) `mod` 10 - 5)
  where
    rackId = cx + 10

-- | `grid` should take a serial number and return a grid of Map Coord PowerLevel
grid :: Serial -> Map Coord PowerLevel
grid s =
  M.fromList $ do
    x <- [1 .. 300]
    y <- [1 .. 300]
    pure $
      let c = (x, y)
       in (c, calcPL c s)

-- | `calcSquarePL` should take a coordinate in the grid, return the total power level for
--   all fuel cells in the (3x3) square wherein the given coordinate is the left,
--   topmost cell
calcSquarePL :: Map Coord PowerLevel -> Coord -> PowerLevel
calcSquarePL m c = sum pls
  where
    pls :: [PowerLevel]
    pls = catMaybes $ traverse M.lookup (mkSquare c) m
    mkSquare :: Coord -> [Coord]
    mkSquare (cx, cy) = do
      x <- [cx .. (cx + 2)]
      y <- [cy .. (cy + 2)]
      pure (x, y)

solve :: Serial -> Coord
solve s = head $ reverse $ sortOn (calcSquarePL (grid s)) candidates
  where
    candidates :: [Coord]
    candidates = do
      x <- [1 .. 298]
      y <- [1 .. 298]
      pure (x, y)

main :: IO ()
main = putStrLn (show $ solve 7803)

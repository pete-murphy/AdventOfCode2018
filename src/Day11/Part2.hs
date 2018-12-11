module Day11.Part2 where

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
--   all fuel cells in the (n x n) square wherein the given coordinate is the left,
--   topmost cell
calcSquarePL :: Map Coord PowerLevel -> Int -> Coord -> PowerLevel
calcSquarePL m n c = sum pls
  where
    pls :: [PowerLevel]
    pls = catMaybes $ traverse M.lookup (mkSquare c) m
    mkSquare :: Coord -> [Coord]
    mkSquare (cx, cy) = do
      x <- [cx .. cx + n - 1]
      y <- [cy .. cy + n - 1]
      pure (x, y)

solve' :: Serial -> Int -> Map (Coord, Int) PowerLevel
solve' s n = foldl (foldingFn s n) M.empty candidates
  where
    candidates :: [Coord]
    candidates = do
      x <- [1 .. 300 - n + 1]
      y <- [1 .. 300 - n + 1]
      pure (x, y)

foldingFn ::
     Serial
  -> Int
  -> Map (Coord, Int) PowerLevel
  -> Coord
  -> Map (Coord, Int) PowerLevel
foldingFn s n m c = M.insert (c, n) v m
  where
    v = calcSquarePL (grid s) n c

solve :: Serial -> (Coord, Int)
solve s = fst $ last $ sortOn snd $ concatMap (M.toList . solve' s) [1 .. 300]

main :: IO ()
main = putStrLn (show $ solve 7803)

{-# LANGUAGE TupleSections #-}

module Day11.Part2 where

import           Control.Arrow
import           Data.List
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Ord

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

type TopLeft = Coord

type Size = Int

calcMaxPLByTL :: Map Coord PowerLevel -> TopLeft -> (Size, PowerLevel)
calcMaxPLByTL m (cx, cy) =
  maximumBy (comparing snd) $
  (id *** sum) <$> do
    size <- [1 .. (300 - (max cx cy) + 1)]
    pure $
      (size, ) $ do
        x <- [cx .. cx + size - 1]
        y <- [cy .. cy + size - 1]
        pure $ m M.! (x, y)

solve :: Serial -> (TopLeft, Size)
solve s = (\(x, y, _) -> (x, y)) $ foldl foo ((1, 1), 0, 0) candidates
  where
    foo :: (TopLeft, Size, PowerLevel) -> TopLeft -> (TopLeft, Size, PowerLevel)
    foo old@(tl, size, pl) tl'
      | v > pl = (tl', u, v)
      | otherwise = old
      where
        (u, v) = calcMaxPLByTL m tl'
        m = grid s
    candidates :: [TopLeft]
    candidates = do
      x <- [1 .. 300]
      y <- [1 .. 300]
      pure (x, y)
    g :: Map Coord PowerLevel
    g = grid s

main :: IO ()
main = putStrLn (show $ solve 7803)

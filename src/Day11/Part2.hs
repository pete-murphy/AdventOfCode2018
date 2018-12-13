{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Day11.Part2 where

import           Control.Arrow
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Ord
import           Debug.Trace

type Coord = (Int, Int)

type Serial = Int

type PowerLevel = Int

calcPL :: Coord -> Serial -> PowerLevel
calcPL (cx, cy) s = ((((rackId * cy + s) * rackId) `div` 100) `mod` 10 - 5)
  where
    rackId = cx + 11

-- | `sumAreaTable` should take a serial number and return a grid of Map Coord PowerLevel
sumAreaTable :: Serial -> Map Coord PowerLevel
sumAreaTable s =
  M.fromList $
  transform $ do
    x <- [1 .. 300]
    y <- [1 .. 300]
    pure $
      let c = (x, y)
       in (c, calcPL c s)
  where
    transform :: [(Coord, PowerLevel)] -> [(Coord, PowerLevel)]
    transform =
      concat .
      transpose . map (scanl1 (f)) . transpose . map (scanl1 (f)) . chunksOf 300
    f :: (Coord, PowerLevel) -> (Coord, PowerLevel) -> (Coord, PowerLevel)
    f (c, p) (c', p') = (c', p + p')

type TopLeft = Coord

type BottomRight = Coord

type Size = Int

-- calcMaxPLByTL :: Map Coord PowerLevel -> TopLeft -> (Size, PowerLevel)
-- calcMaxPLByTL m (cx, cy) =
--   maximumBy (comparing snd) $
--   (id *** sum) <$> do
--     size <- [1 .. (300 - (max cx cy) + 1)]
--     pure $
--       (size, ) $ do
--         x <- [cx .. cx + size - 1]
--         y <- [cy .. cy + size - 1]
--         pure $ m M.! (x, y)
calcMaxPLByTL' :: Map Coord PowerLevel -> TopLeft -> BottomRight -> PowerLevel
calcMaxPLByTL' m (cx, cy) (dx, dy) =
  let [a, b, c, d] = map (m M.!) [(cx, cy), (dx, cy), (cx, dy), (dx, dy)]
   in d - b - c + a

--  maximum $ do {- Pretty sure this is the line that calls the exception -}
solve :: Serial -> (TopLeft, Size)
solve s = undefined
  where
    candidates :: [TopLeft]
    candidates = do
      x <- [1 .. 300]
      y <- [1 .. 300]
      pure (x, y)
    g :: Map Coord PowerLevel
    g = sumAreaTable s

main :: IO ()
main = putStrLn (show $ solve 7803)

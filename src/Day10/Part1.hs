{-# LANGUAGE TupleSections #-}

module Day10.Part1 where

import           Control.Arrow
import           Control.Monad.State
import           Data.Array
import           Data.Bifunctor
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Ord
import           Debug.Trace
import           Linear.V2

type Coord = (Int, Int)

type Pos = Coord

type Vel = Coord

parse :: String -> [(Pos, Vel)]
parse = map parseLine . lines
  where
    parseLine = format . map read . filter (any isDigit) . splitOneOf "<,>"
    format (px:py:vx:vy:_) = ((px, py), (vx, vy))

run :: [(Pos, Vel)] -> [(Pos, Vel)]
run = map (\((px, py), (vx, vy)) -> ((px + vx, py + vy), (vx, vy)))

findBounds :: [(Pos, Vel)] -> (Pos, Pos)
findBounds cs = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    ps = map fst cs
    xs = map fst ps
    ys = map snd ps

--  area :: (Pos, Pos) -> Int
--  area ((x, y), (x', y')) = (x' - x) * (y' - y)
--
--  preRun :: [(Pos, Vel)] -> [(Pos, Vel)]
--  preRun lights =
--    until
--      ((\(old, new) -> areaBounds new > areaBounds old) . (id &&& run))
--      run
--      lights
--    where
--      areaBounds = area . findBounds
--
--  render :: [(Pos, Vel)] -> Array Coord String
--  render lts =
--    accumArray (++) "" ((0, 0), (w, h)) $
--    map (, "X") $ map (bimap (subtract x) (subtract y) . fst) $ lts
--    where
--      ((x, y), (x', y')) = findBounds lts
--      w = x' - x
--      h = y' - y
render' :: [(Pos, Vel)] -> [String]
render' lts =
  trace (show (h, w)) $
  map
    (concat .
     map
       (\(a, b) ->
          if (a, b) `elem` pts
            then "#"
            else ".")) $
  groupBy ((\x y -> snd x == snd y)) $ do
    b <- [y .. (y')]
    a <- [x .. (x')]
    pure $ (a, b)
  where
    pts = map fst $ run $ run $ run lts
    ((x, y), (x', y')) = findBounds lts
    w = x' - x
    h = y' - y

main :: IO ()
main = do
  text <- readFile "src/Day10/sample.txt"
  mapM_ (putStrLn . show) $ render' $ parse text

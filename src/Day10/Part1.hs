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
run = map ((uncurry (+) *** uncurry (+)) &&& snd)

findBounds :: [(Pos, Vel)] -> (Pos, Pos)
findBounds cs = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    ps = map fst cs
    xs = map fst ps
    ys = map snd ps

area :: (Pos, Pos) -> Int
area ((x, y), (x', y')) = (x' - x) * (y' - y)

preRun lights =
  until
    ((\(old, new) -> areaBounds new > areaBounds old) . (id &&& run))
    run
    lights
  where
    areaBounds = area . findBounds

render :: [(Pos, Vel)] -> Array Coord String
render lts =
  array ((0, 0), (w, h)) $
  map (, "X") $ map (bimap (subtract x) (subtract y) . fst) $ preRun lts
  where
    ((x, y), (x', y')) = findBounds lts
    w = x' - x
    h = y' - y

main :: IO ()
main = do
  text <- readFile "src/Day10/sample.txt"
  mapM_ (putStrLn . show) $ parse text

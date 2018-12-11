{-# LANGUAGE TupleSections #-}

module Day10.Part2 where

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

area :: (Pos, Pos) -> Int
area ((x, y), (x', y')) = (x' - x) * (y' - y)

preRun :: [(Pos, Vel)] -> String
preRun lights =
  until'
    0
    ((\(old, new) -> areaBounds new > areaBounds old) .
     (preRun' 2 &&& preRun' 3))
    run
    lights
  where
    areaBounds = area . findBounds

until' :: Int -> (a -> Bool) -> (a -> a) -> a -> String
until' n p f = go n
  where
    go n x
      | p x = show n
      | otherwise = go (n + 1) (f x)

preRun' :: Int -> [(Pos, Vel)] -> [(Pos, Vel)]
preRun' 0 xs = xs
preRun' n xs = preRun' (n - 1) (run xs)

main :: IO ()
main =
  readFile "src/Day10/input.txt" >>= \text ->
    putStrLn (preRun $ parse text) >> (putStrLn "Done")

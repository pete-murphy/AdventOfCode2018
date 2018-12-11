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

area :: (Pos, Pos) -> Int
area ((x, y), (x', y')) = (x' - x) * (y' - y)

preRun :: [(Pos, Vel)] -> [(Pos, Vel)]
preRun lights =
  until
    ((\(old, new) -> areaBounds new > areaBounds old) .
     (preRun' 2 &&& preRun' 3))
    run
    lights
  where
    areaBounds = area . findBounds

preRun' :: Int -> [(Pos, Vel)] -> [(Pos, Vel)]
preRun' 0 xs = xs
preRun' n xs = preRun' (n - 1) (run xs)

findBounds'' :: [Pos] -> (Pos, Pos)
findBounds'' ps = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    xs = map fst ps
    ys = map snd ps

render'' :: [Pos] -> [String]
render'' pts =
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
    ((x, y), (x', y')) = findBounds'' pts
    w = x' - x
    h = y' - y

run' :: Int -> StateT [(Pos, Vel)] IO [Pos]
run' n = do
  replicateM n $ do
    xs <- get
    let xs' = map (\((p, p'), vs@(v, v')) -> ((p + v, p' + v'), vs)) xs
    lift $ mapM_ (putStrLn . show) $ render'' (map fst xs')
    put xs'
  xs'' <- get
  pure $ map fst xs''

main :: IO ()
main =
  readFile "src/Day10/input.txt" >>= \text ->
    runStateT (run' 3) (preRun $ parse text) >> (putStrLn "Done")

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

turns :: Int -> StateT (Int, Int) IO String
turns n = do
  void $
    replicateM n $ do
      lift $ putStr "P: "
      p' <- lift (read <$> getLine :: IO Int)
      lift $ putStrLn $ "C: "
      (c, p) <- get
      if even (p')
        then put (p, c + 1)
        else put (p + 1, c)
  (c'', p'') <- get
  pure $
    if c'' > p''
      then "Computer wins"
      else "You win"

turn :: StateT (Int, Int) IO ()
turn =
  (lift $ putStr "P: ") >> lift (read <$> getLine :: IO Int) >>= \p' ->
    (lift $ putStrLn $ "C: ") >> get >>= \(c, p) ->
      if even (p')
        then put (p, c + 1)
        else put (p + 1, c)

turns' :: Int -> StateT (Int, Int) IO String
turns' n =
  replicateM n turn >> get >>= \(c'', p'') ->
    pure $
    if c'' > p''
      then "Computer wins"
      else "You win"

main :: IO ()
main =
  readFile "src/Day10/input.txt" >>= \text ->
    runStateT (run' 3) (preRun $ parse text) >> (putStrLn "Done")

main' :: IO ()
main' = do
  text <- readFile "src/Day10/sample.txt"
  mapM_ (putStrLn . show) $ render' $ parse text

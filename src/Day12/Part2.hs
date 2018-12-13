module Day12.Part2 where

import           Control.Arrow
import           Data.Array
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe

data Pot
  = P
  | N
  deriving (Eq, Ord)

instance Show Pot where
  show P = "#"
  show N = "."

parse :: String -> [Pot]
parse []       = []
parse ('#':xs) = P : parse xs
parse ('.':xs) = N : parse xs
parse (_:xs)   = parse xs

parseInitialState :: String -> [(Int, Pot)]
parseInitialState = zip [0 ..] . parse . head . lines

type Pattern = Map [Pot] Pot

parseRules :: String -> Pattern
parseRules = M.fromList . map parseLine . (drop 2) . lines
  where
    parseLine = (head &&& head . last) . map parse . splitOn " => "

pad :: [(Int, Pot)] -> [(Int, Pot)]
pad ps' = pad' $ reverse (pad'' $ reverse ps')
  where
    pad' ps@((i, P):_)     = ((i - 3), N) : ((i - 2), N) : ((i - 1), N) : ps
    pad' ps@(_:(i, P):_)   = ((i - 3), N) : ((i - 2), N) : ps
    pad' ps@(_:_:(i, P):_) = ((i - 3), N) : ps
    pad' ps                = ps
    pad'' ps@((i, P):_)     = ((i + 3), N) : ((i + 2), N) : ((i + 1), N) : ps
    pad'' ps@(_:(i, P):_)   = ((i + 3), N) : ((i + 2), N) : ps
    pad'' ps@(_:_:(i, P):_) = ((i + 3), N) : ps
    pad'' ps                = ps

evolve' :: Pattern -> [(Int, Pot)] -> [(Int, Pot)]
evolve' m ps = map (id *** fromMaybe N . (M.!?) m) (toFives $ pad ps)

toFives :: [(Int, Pot)] -> [(Int, [Pot])]
toFives xs@(_:_:(i, _):_)
  | length xs == 5 = [(i, xs')]
  | otherwise = [(i, take 5 xs')] ++ toFives (drop 1 xs)
  where
    xs' = map snd xs

countP :: [(Int, Pot)] -> Int
countP []          = 0
countP ((i, P):xs) = i + countP xs
countP (_:xs)      = countP xs

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

nTimes' :: (a -> a) -> a -> Int -> a
nTimes' _ x 0 = x
nTimes' f x 1 = f x
nTimes' f x n = f $ nTimes' f x (n - 1)

main :: IO ()
main = do
  text <- readFile "src/Day12/input.txt"
  let initState = parseInitialState text
      patts = parseRules text
   in putStrLn $ show $ countP $ nTimes 202 (evolve' patts) initState

main_ :: IO ()
main_ = do
  text <- readFile "src/Day12/input.txt"
  let initState = parseInitialState text
      patts = parseRules text
   in mapM_ putStrLn $
      map
        (concatMap (show . snd) . nTimes' (evolve' patts) initState)
        [1 .. 200]

main' :: IO ()
main' = do
  text <- readFile "src/Day12/sample.txt"
  let initState = parseInitialState text
      patts = parseRules text
   in putStrLn $ concatMap (show . snd) $ nTimes 20 (evolve' patts) initState

main'' :: IO ()
main'' = do
  text <- readFile "src/Day12/sample.txt"
  let initState = parseInitialState text
      patts = parseRules text
   in mapM_ putStrLn $
      map (concatMap (show . snd) . nTimes' (evolve' patts) initState) [1 .. 20]

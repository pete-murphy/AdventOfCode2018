module Day16.Part2 where

import           Data.List
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Day16.Part1 hiding (main, parse, solve)
import qualified Day16.Part1 as P1

opsString :: [String]
opsString =
  [ "addr"
  , "addi"
  , "mulr"
  , "muli"
  , "banr"
  , "bani"
  , "borr"
  , "bori"
  , "setr"
  , "seti"
  , "gtir"
  , "gtri"
  , "gtrr"
  , "eqir"
  , "eqri"
  , "eqrr"
  ]

match :: [Sample] -> [(Int, [String])]
match = foldl foo []
  where
    foo :: [(Int, [String])] -> Sample -> [(Int, [String])]
    foo xs samp@(Sample _ (Instruction n _ _ _) _)
      | length matches >= 1 = (n, matches) : xs
      | otherwise = xs
      where
        matches = nameMatchingOps samp

nameMatchingOps :: Sample -> [String]
nameMatchingOps = go opsString ops
  where
    go _ [] _ = []
    go (name:names) (f:fs) samp@(Sample b ins result)
      | f b ins == result = name : go names fs samp
      | otherwise = go names fs samp

matchMap :: [Sample] -> Map Int [String]
matchMap = M.fromListWith intersect . match

solve :: Map Int [String] -> Map Int String
solve m = M.fromList $ map (fmap head) $ go ([], (M.toList m))
  where
    go :: ([(Int, [String])], [(Int, [String])]) -> [(Int, [String])]
    go (uniqs, xs)
      | null xs = uniqs
      | otherwise =
        go
          ( (uniqs ++ uniqs')
          , (map (fmap $ flip (\\) (concatMap snd (uniqs ++ uniqs'))) xs'))
      where
        (uniqs', xs') = splitOnUniq xs

splitOnUniq :: [(Int, [String])] -> ([(Int, [String])], [(Int, [String])])
splitOnUniq = span ((==) 1 . length . snd) . sortOn (length . snd)

main :: IO ()
main = do
  text <- readFile "src/Day16/input.txt"
  mapM_ (putStrLn . show) $ M.toList $ solve $ matchMap $ P1.parse text

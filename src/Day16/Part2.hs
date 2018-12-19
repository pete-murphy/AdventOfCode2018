module Day16.Part2 where

import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
import           Day16.Part1     hiding (main, parse, solve)
import qualified Day16.Part1     as P1

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

opsMap :: Map Int Operation
opsMap =
  M.fromList
    [ (0, muli)
    , (1, borr)
    , (2, gtri)
    , (3, eqri)
    , (4, gtrr)
    , (5, eqir)
    , (6, addi)
    , (7, setr)
    , (8, mulr)
    , (9, addr)
    , (10, bori)
    , (11, bani)
    , (12, seti)
    , (13, eqrr)
    , (14, banr)
    , (15, gtir)
    ]

nameMatchingOps :: Sample -> [String]
nameMatchingOps = go opsString ops
  where
    go _ [] _ = []
    go (name:names) (f:fs) samp@(Sample b ins result)
      | f b ins == result = name : go names fs samp
      | otherwise = go names fs samp

matchMap :: [Sample] -> Map Int [String]
matchMap = M.fromListWith intersect . match

splitOnUniq :: [(Int, [String])] -> ([(Int, [String])], [(Int, [String])])
splitOnUniq = span ((==) 1 . length . snd) . sortOn (length . snd)

parse :: String -> [Instruction]
parse =
  map ((\(w:x:y:z:_) -> Instruction w x y z) . parseLine) .
  lines . last . splitOn "\n\n\n\n"
  where
    parseLine :: String -> [Int]
    parseLine = map read . words

runProgram :: Registers -> [Instruction] -> Registers
runProgram rs [] = rs
runProgram rs ((Instruction opCode a b c):xs) =
  runProgram ((opsMap M.! opCode) rs (Instruction undefined a b c)) xs

main :: IO ()
main = do
  text <- readFile "src/Day16/input.txt"
  putStrLn $ show $ runProgram (V.fromList [0, 0, 0, 0]) (parse text)

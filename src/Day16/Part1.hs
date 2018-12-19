module Day16.Part1 where

import           Control.Arrow
import           Data.Bits
import           Data.List.Split
import           Data.Vector     (Vector)
import qualified Data.Vector     as V

data Instruction = Instruction
  { opcode :: Int
  , inputA :: Int
  , inputB :: Int
  , output :: Int
  }

data Sample = Sample
  { before      :: Registers
  , instruction :: Instruction
  , after       :: Registers
  }

type Registers = Vector Int

type Operation = Registers -> Instruction -> Registers

-- | Operations
addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr ::
     Operation
addr rs (Instruction _ a b c) = rs V.// [(c, rs V.! a + rs V.! b)]

addi rs (Instruction _ a b c) = rs V.// [(c, rs V.! a + b)]

mulr rs (Instruction _ a b c) = rs V.// [(c, rs V.! a * rs V.! b)]

muli rs (Instruction _ a b c) = rs V.// [(c, rs V.! a * b)]

banr rs (Instruction _ a b c) = rs V.// [(c, rs V.! a .&. rs V.! b)]

bani rs (Instruction _ a b c) = rs V.// [(c, rs V.! a .&. b)]

borr rs (Instruction _ a b c) = rs V.// [(c, rs V.! a .|. rs V.! b)]

bori rs (Instruction _ a b c) = rs V.// [(c, rs V.! a .|. b)]

setr rs (Instruction _ a _ c) = rs V.// [(c, rs V.! a)]

seti rs (Instruction _ a _ c) = rs V.// [(c, a)]

gtir rs (Instruction _ a b c) =
  rs V.//
  [ ( c
    , if (a > rs V.! b)
        then 1
        else 0)
  ]

gtri rs (Instruction _ a b c) =
  rs V.//
  [ ( c
    , if (rs V.! a > b)
        then 1
        else 0)
  ]

gtrr rs (Instruction _ a b c) =
  rs V.//
  [ ( c
    , if (rs V.! a > rs V.! b)
        then 1
        else 0)
  ]

eqir rs (Instruction _ a b c) =
  rs V.//
  [ ( c
    , if (a == rs V.! b)
        then 1
        else 0)
  ]

eqri rs (Instruction _ a b c) =
  rs V.//
  [ ( c
    , if (rs V.! a == b)
        then 1
        else 0)
  ]

eqrr rs (Instruction _ a b c) =
  rs V.//
  [ ( c
    , if (rs V.! a == rs V.! b)
        then 1
        else 0)
  ]

ops :: [Operation]
ops =
  [ addr
  , addi
  , mulr
  , muli
  , banr
  , bani
  , borr
  , bori
  , setr
  , seti
  , gtir
  , gtri
  , gtrr
  , eqir
  , eqri
  , eqrr
  ]

parseSample :: String -> Sample
parseSample s = Sample b (Instruction w x y z) a
  where
    b =
      V.fromList .
      (read :: String -> [Int]) . last . splitOn "Before: " . head . lines $
      s
    (w:x:y:z:_) =
      map (read :: String -> Int) . words . (flip (!!) 1) . lines $ s
    a =
      V.fromList .
      (read :: String -> [Int]) . last . splitOn "After: " . last . lines $
      s

parse :: String -> [Sample]
parse = map parseSample . splitOn "\n\n" . head . splitOn "\n\n\n"

countMatchingOps :: Sample -> Int
countMatchingOps = go ops
  where
    go [] _ = 0
    go (f:fs) samp@(Sample b ins result)
      | f b ins == result = 1 + go fs samp
      | otherwise = go fs samp

solve :: [Sample] -> Int
solve = foldl foo 0
  where
    foo :: Int -> Sample -> Int
    foo n s
      | countMatchingOps s >= 3 = n + 1
      | otherwise = n

main :: IO ()
main = do
  text <- readFile "src/Day16/input.txt"
  putStrLn $ show $ solve $ parse text

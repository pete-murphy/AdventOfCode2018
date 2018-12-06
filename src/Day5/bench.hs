module Day5.Benchmark where

import           Criterion.Main
import           Day5.Part1     as P1
import           Day5.Part2     as P2

main :: IO ()
main =
  defaultMain
    [ bench "part 1" $ nfIO $ P1.solve <$> readFile "src/Day5/input.txt"
    , bench "part2" $ nfIO $ P2.solve <$> readFile "src/Day5/input.txt"
    ]

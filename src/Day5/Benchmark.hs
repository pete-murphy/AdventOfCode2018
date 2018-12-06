module Day5.Benchmark where

import           Criterion.Main
import qualified Day5.Part1     as P1
import qualified Day5.Part2     as P2

main :: IO ()
main =
  defaultMain
    [ bench "part 1" $ nfIO $ P1.solve <$> readFile "input.txt"
--    , bench "part 2" $ nfIO $ P2.solve <$> readFile "input.txt"
    ]

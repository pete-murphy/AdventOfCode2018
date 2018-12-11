module Day11Spec where

import qualified Day11.Part1 as P1
import qualified Day11.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

fc1 = ((122, 79), 57)

fc2 = ((217, 196), 39)

fc3 = ((101, 153), 71)

spec :: Spec
spec = do
  describe "Part1" $ do
    it "calculating power level" $ do
      sampleInput <- "src/Day11/sample.txt"
      P1.calcPL fc1 `shouldBe` (-5)
      P1.calcPL fc2 `shouldBe` 0
      P1.calcPL fc3 `shouldBe` 4

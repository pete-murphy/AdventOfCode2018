module Day1Spec where

import qualified Day1.Part1 as P1
import qualified Day1.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "sample input" $ do
      input <- readFile "sample/day1.txt"
      P1.solve input `shouldBe` 4
  describe "Part1" $ do
    it "sample input" $ do
      input <- readFile "src/Day1/input.txt"
      P1.solve input `shouldBe` 516

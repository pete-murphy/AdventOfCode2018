module Day3Spec where

import qualified Day3.Part1 as P1
import qualified Day3.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "sample input" $ do
      input <- readFile "src/Day3/sample.txt"
      P1.solve input `shouldBe` 4
    it "actual input" $ do
      input <- readFile "src/Day3/input.txt"
      P1.solve input `shouldBe` 115348
  describe "Part2" $ do
    it "sample input" $ do
      input <- readFile "src/Day3/sample.txt"
      P2.solve input `shouldBe` 3
    it "actual input" $ do
      input <- readFile "src/Day3/input.txt"
      P2.solve input `shouldBe` 188

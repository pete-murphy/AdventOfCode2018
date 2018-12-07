module Day6Spec where

import qualified Day6.Part1 as P1
import qualified Day6.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "sample input" $ do
      sampleInput <- readFile "src/Day6/sample.txt"
      P1.solve sampleInput `shouldBe` 17
  describe "Part2" $ do
    it "actual input" $ do
      input <- readFile "src/Day6/input.txt"
      P2.solve input `shouldBe` 34096

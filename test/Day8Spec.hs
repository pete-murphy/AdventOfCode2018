module Day8Spec where

import qualified Day8.Part1 as P1
import qualified Day8.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "actual input" $ do
      sampleInput <- readFile "src/Day8/input.txt"
      (P1.solve . P1.parse) sampleInput `shouldBe` 46096
  describe "Part2" $ do
    it "sample input" $ do
      sampleInput <- readFile "src/Day8/sample.txt"
      (P2.solve . P1.parse) sampleInput `shouldBe` 66

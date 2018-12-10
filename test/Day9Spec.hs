module Day9Spec where

import qualified Day9.Part1 as P1
import qualified Day9.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "actual input" $ do
      sampleInput <- (map P1.parse . lines) <$> readFile "src/Day9/sample.txt"
      let (ex1:ex2:ex3:ex4:ex5:_) = P1.solve <$> sampleInput
       in (ex1, ex2) `shouldBe` (8317, 146373)
--  describe "Part2" $ do
--    it "sample input" $ do
--      sampleInput <- readFile "src/Day8/sample.txt"
--      (P2.solve . P1.parse) sampleInput `shouldBe` 66

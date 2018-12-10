module Day10Spec where

import qualified Day10.Part1 as P1
import qualified Day10.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part2" $ do
    it "actual input" $ do
      sampleInput <- (map P2.parse . lines) <$> readFile "src/Day10/sample.txt"
      let (ex1:ex2:ex3:ex4:ex5:_) = P2.solve <$> sampleInput
       in (ex1, ex2, ex3, ex4, ex5) `shouldBe`
          (8317, 146373, 2764, 54718, 37305)
--  describe "Part2" $ do
--    it "sample input" $ do
--      sampleInput <- readFile "src/Day8/sample.txt"
--      (P2.solve . P1.parse) sampleInput `shouldBe` 66

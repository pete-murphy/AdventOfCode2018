module Day7Spec where

import qualified Day7.Part1 as P1
import qualified Day7.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "sample input" $ do
      sampleInput <- readFile "src/Day7/sample.txt"
      (P1.solve . P1.parse) sampleInput `shouldBe` "CABDFE"
--  describe "Part2" $ do
--    it "actual input" $ do
--      input <- readFile "src/Day7/input.txt"
--      P2.solve input `shouldBe` undefined

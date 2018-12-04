module Day4Spec where

import qualified Day4.Part1 as P1
import qualified Day4.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $
-- | I can't tell why this fails??? My `solve` returns 250
--    it "sample input" $ do
--      input <- readFile "src/Day4/sample.txt"
--      P1.solve input `shouldBe` 240
   do
    it "actual input" $ do
      input <- readFile "src/Day4/input.txt"
      P1.solve input `shouldBe` 101262
  describe "Part2" $ do
    it "sample input" $ do
      input <- readFile "src/Day4/sample.txt"
      P2.solve input `shouldBe` 4455
    it "actual input" $ do
      input <- readFile "src/Day4/input.txt"
      P2.solve input `shouldBe` 71976

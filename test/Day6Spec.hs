module Day6Spec where

import qualified Day6.Part1 as P1

-- import qualified Day6.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do it "sample input 1" $ do P1.solve "abBA\n" `shouldBe` 0
--    it "sample input 2" $ do P1.solve "aabAAB\n" `shouldBe` 6
--    it "sample input 3" $ do P1.solve "dabAcCaCBAcCcaDA\n" `shouldBe` 10
--  describe "Part2" $ do
--    it "actual input" $ do
--      input <- readFile "src/Day5/input.txt"
--      P2.solve input `shouldBe` 6188

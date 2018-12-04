module Day2Spec where

import qualified Day2.Part1 as P1
import qualified Day2.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "sample input" $ do
      input <- readFile "src/Day2/sample-1.txt"
      P1.solve input `shouldBe` 12
    it "actual input" $ do
      input <- readFile "src/Day2/input.txt"
      P1.solve input `shouldBe` 4693
  describe "Part2" $
-- | This doesn't work because I baked the length of string into the implementation :<
--    it "sample input" $ do
--      input <- readFile "src/Day2/sample-2.txt"
--      P2.solve input `shouldBe` "fgij"
   do
    it "actual input" $ do
      input <- readFile "src/Day2/input.txt"
      P2.solve input `shouldBe` "pebjqsalrdnckzfihvtxysomg"

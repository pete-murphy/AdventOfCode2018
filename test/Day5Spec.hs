module Day5Spec where

import qualified Day5.Part1 as P1

-- import qualified Day5.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "sample input" $ do
      input <- readFile "src/Day5/sample.txt"
      P1.solve input `shouldBe` undefined
{-
  describe "Part2" $ do
    it "sample input" $ do
      input <- readFile "src/Day5/sample.txt"
      P2.solve input `shouldBe` undefined
-}

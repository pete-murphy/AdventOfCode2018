module Day13Spec where

import qualified Day13.Part1 as P1
import qualified Day13.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "small sample tick" $ do
      sample <- readFile "src/Day13/sample/2.txt"
      P1.solve sample `shouldBe` (7, 3)
  describe "Part2" $ do
    it "sample" $ do
      sample <- readFile "src/Day13/sample/3.txt"
      P2.solve sample `shouldBe` (6, 4)

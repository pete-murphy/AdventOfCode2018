module Day18Spec where

import qualified Day18.Part1 as P1
import qualified Day18.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "small sample tick" $ do
      sample <- readFile "src/Day18/"
      P1.solve sample `shouldBe` 1147
--  describe "Part2" $ do
--    it "sample" $ do
--      sample <- readFile "src/Day18/"
--      P2.solve sample `shouldBe` (6, 4)

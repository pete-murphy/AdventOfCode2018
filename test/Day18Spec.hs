module Day18Spec where

import qualified Day18.Part1 as P1
import qualified Day18.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "sample" $ do
      sample <- readFile "src/Day18/sample.txt"
      P1.solve sample `shouldBe` 1147
  describe "Part2" $ do
    let ys = [1, 4, 5, 3, 2, 1, 5, 3, 2, 1]
    it "accum" $ do P2.accum 0 ys `shouldBe` (6, [1, 5, 3, 2])
    it "valueAfter" $ do P2.valueAfter 10 (P2.accum 0 ys) `shouldBe` 5

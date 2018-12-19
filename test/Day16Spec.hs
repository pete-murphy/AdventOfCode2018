module Day16Spec where

import qualified Day16.Part1 as P1

-- import qualified Day16.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "countMatchingOps sample" $ do
      sample <- readFile "src/Day16/sample.txt"
      (P1.countMatchingOps $ P1.parseSample sample) `shouldBe` 3
--  describe "Part2" $ do
--    let ys = [1, 4, 5, 3, 2, 1, 5, 3, 2, 1]
--    it "accum" $ do P2.accum 0 ys `shouldBe` (6, [1, 5, 3, 2])
--    it "valueAfter" $ do P2.valueAfter 10 (P2.accum 0 ys) `shouldBe` 5

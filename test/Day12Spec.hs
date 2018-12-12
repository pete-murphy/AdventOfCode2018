module Day12Spec where

import qualified Day12.Part1 as P1
import qualified Day12.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "calculating power level" $ do
      calcPL' fc10 `shouldBe` 4
      calcPL' fc11 `shouldBe` (-5)
      calcPL' fc12 `shouldBe` 0
      calcPL' fc13 `shouldBe` 4
    it "max PL of example grid (serial no. 18)" $ do
      P1.solve 18 `shouldBe` (33, 45)
--  describe "Part2" $ do
--    it "max PL of example grid (serial no. 18)" $ do
--      P2.solve 18 `shouldBe` ((90, 269), 16)

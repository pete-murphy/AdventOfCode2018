module Day11Spec where

import qualified Day11.Part1 as P1
import qualified Day11.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

fc0 = ((3, 5), 8)

fc1 = ((122, 79), 57)

fc2 = ((217, 196), 39)

fc3 = ((101, 153), 71)

spec :: Spec
spec = do
  describe "Part1" $ do
    it "calculating power level" $ do
      calcPL' fc0 `shouldBe` 4
      calcPL' fc1 `shouldBe` (-5)
      calcPL' fc2 `shouldBe` 0
      calcPL' fc3 `shouldBe` 4
    it "max PL of example grid (serial no. 18)" $ do
      P1.solve 18 `shouldBe` (33, 45)
  where
    calcPL' = uncurry P1.calcPL

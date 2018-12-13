module Day13Spec where

import qualified Day13.Part1 as P1
import qualified Day13.Part2 as P2
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "small sample tick" $ do P1.tick ex1 `shouldBe` ex2
    it "max PL of example grid (serial no. 18)" $ do
      P1.solve 18 `shouldBe` (33, 45)
--  describe "Part2" $ do
--    it "max PL of example grid (serial no. 18)" $ do
--      P2.solve 18 `shouldBe` ((90, 269), 16)

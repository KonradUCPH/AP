module PointSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

-- import module to test
import Curves

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- Testcases
spec :: Spec
spec = do
  describe "Equality of Points" $ do
    it "Point is Equal" $ do
      point (0.0, 0.0) == point(0.0, 0.0001) `shouldBe` True
    it "Point is not Equal" $ do
      point (0.0, 0.0) == point(0.0, 0.1) `shouldBe` False
    it "Point is Unequal" $ do
      point (0.0, 0.0) /= point(0.0, 0.0001) `shouldBe` False
    it "Point is not Unequal" $ do
      point (0.0, 0.0) /= point(0.0, 0.1) `shouldBe` True
  describe "Coordinate of Points" $ do
    it "get X coord" $ do
      pointX (point (1.0, 0.0)) `shouldBe` 1.0
    it "get Y coord" $ do
      pointY (point (0.0, 1.0)) `shouldBe` 1.0
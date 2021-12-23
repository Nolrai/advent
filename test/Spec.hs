module Main (main) where

import Test.QuickCheck
import Test.Hspec
import Data.Matrix as X
import Advent (testGM, dumbPower, smartPower)

main :: IO ()
main = hspec $ do
  describe "lanternFishDay'" $ do
    it "should do the same thing as lanternFishDay" $
       property (testGM . map ((`mod` 9) . abs) . unfish)

  describe "smartPower" $ do
    it "should be the same as dumbPower" $ do
      property (\ (M m) (Positive n) -> smartPower m n `shouldBe` dumbPower m n)

newtype Fish = Fish {unfish :: [Int]}
newtype M = M (Matrix Int)

instance Arbitrary M where
  arbitrary = M . X.fromList <$> replicateM 3 (repicateM 3)
  shrink (M m) = M . X.fromList . toList <$> filter (\ l -> length l == 3 && all ((== 3) . length) l) (shrink (X.toList m))

instance Arbitrary Fish where
  arbitrary = Fish <$> listOf (chooseInt (0,8))
  shrink f = Fish . map ((`mod` 9) . abs) <$> shrink (unfish f)
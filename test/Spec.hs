{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude, DerivingStrategies, GeneralisedNewtypeDeriving, FlexibleInstances #-}

module Main (main) where

import Relude
import Test.QuickCheck
import Test.Hspec
import Data.Matrix as X
import qualified Data.List as L
import Data.Vector as V
import Advent (lanternFishDay, lanternFishDay', lanternFishDays, fishToVector, dumbPower, smartPower)

main :: IO ()
main = hspec $ do
  describe "lanternFishDay'" $ do
    it "should produce the same results as lanternFishDay' modulo fishToVector" $ do
      property (\ (Fish fish) -> lanternFishDay' (fishToVector fish) `shouldBe` fishToVector (lanternFishDay fish))

  describe "fishToVector" $ do
    it "should preserve total fish" $ do
      property (\ (Fish fish) -> V.sum (fishToVector fish) == L.length fish)
    it "should be same as paraelle filters " $ do
      property (\ (Fish fish) -> fishToVector fish `shouldBe` V.generate 9 (\ n -> L.length . L.filter (== n) $ fish))
    it "ignore order" $ do
      property (\ (Fish fish) -> fishToVector (L.reverse fish) `shouldBe` fishToVector fish)

  describe "smartPower" $ do
    it "should be the same as dumbPower" $ do
      property (\ (M m) (Positive n) -> smartPower m n `shouldBe` dumbPower m n)
  
  describe "lanternFishDays" $ do
    it "should do the same thing as repeated lanternFishDay" $ do
      property $ \ (Fish fish) (Positive (Sized size)) -> 
        let (start :: Vector Int) = fishToVector fish in 
        let lhs = (`lanternFishDays` start) <$> [0 .. (5 * size - 1)] in
        let rhs = Relude.take (5 * size) $ iterate lanternFishDay' start in
          lhs `shouldBe` rhs

newtype Sized a = Sized a
  deriving newtype (Show, Num, Ord, Eq)

instance Arbitrary (Sized Int) where
  arbitrary = Sized <$> getSize
  shrink (Sized size) = Sized <$> shrink size

newtype Fish = Fish {unfish :: [Int]}
  deriving newtype (Show)
  
newtype M = M (Matrix Int)
  deriving newtype (Show)

instance Arbitrary M where
  arbitrary = M . X.fromLists <$> Relude.replicateM 3 (Relude.replicateM 3 arbitrary)
  shrink (M m) = (M . X.fromLists) `L.map` L.filter (\ l -> L.length l == 3 && L.all ((== 3) . L.length) l) (shrink (X.toLists m))

instance Arbitrary Fish where
  arbitrary = Fish <$> listOf (chooseInt (0,8))
  shrink f = Fish . L.map ((`mod` 9) . abs) <$> shrink (unfish f)
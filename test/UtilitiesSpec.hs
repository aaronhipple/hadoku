module UtilitiesSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Utilities

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "chunk" $ do
    it "generates chunks from a list" $
      chunk 2 ([1, 2, 3, 4, 5, 6] :: [Int]) `shouldBe` [[1, 2], [3, 4], [5, 6]]
    it "generates chunks from another list" $
      chunk 3 ([1, 2, 3, 4, 5, 6] :: [Int]) `shouldBe` [[1, 2, 3], [4, 5, 6]]
  describe "quicksort" $ do
    it "sorts a list" $ quicksort ([1, 3, 2] :: [Int]) `shouldBe` [1, 2, 3]
    it "returns an empty list intact" $
      quicksort ([] :: [Int]) `shouldBe` ([] :: [Int])

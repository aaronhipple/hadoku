module UtilitiesSpec (main, spec) where

import Test.Hspec
import Utilities

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "chunk" $ do
    it "generates chunks from a list" $ do
      chunk 2 [1, 2, 3, 4, 5, 6] `shouldBe` [[1, 2], [3, 4], [5, 6]]
    it "generates chunks from another ist" $ do
      chunk 3 [1, 2, 3, 4, 5, 6] `shouldBe` [[1, 2, 3], [4, 5, 6]]

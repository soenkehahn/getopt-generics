{-# LANGUAGE DeriveGeneric #-}

module System.Console.GetOpt.Generics.ModifierSpec where

import           Test.Hspec

import           System.Console.GetOpt.Generics.Modifier
import           Util
import           WithCli

spec :: Spec
spec = do
  describe "insertWith" $ do
    it "combines existing values with the given function" $ do
      insertWith (++) (1 :: Integer) "bar" [(1, "foo")]
        `shouldBe` [(1, "foobar")]

  describe "getVersion" $ do
    it "returns the version" $ do
      let modifiers = unsafeModifiers [AddVersionFlag "1.0.0"]
      getVersion modifiers `shouldBe` Just "1.0.0"

data Foo
  = Foo {
    bar :: String
  }
  deriving (Generic)

data Overlap
  = Overlap {
    foo :: String,
    fooo :: String
  }
  deriving (Generic)

{-# LANGUAGE DeriveGeneric #-}

module System.Console.GetOpt.Generics.ModifierSpec where

import           Data.Proxy
import           Generics.SOP
import qualified GHC.Generics
import           Test.Hspec

import           System.Console.GetOpt.Generics.Modifier

spec :: Spec
spec = do
  describe "deriveShortOptions" $ do
    it "includes modifiers for short options" $ do
      deriveShortOptions (Proxy :: Proxy Foo) `shouldBe` [AddShortOption "bar" 'b']

    it "doesn't include modifiers for short options in case of overlaps" $ do
      deriveShortOptions (Proxy :: Proxy Overlap) `shouldBe` []

  describe "insertWith" $ do
    it "combines existing values with the given function" $ do
      insertWith (++) (1 :: Integer) "bar" [(1, "foo")]
        `shouldBe` [(1, "foobar")]

data Foo
  = Foo {
    bar :: String
  }
  deriving (GHC.Generics.Generic)

instance Generic Foo
instance HasDatatypeInfo Foo

data Overlap
  = Overlap {
    foo :: String,
    fooo :: String
  }
  deriving (GHC.Generics.Generic)

instance Generic Overlap
instance HasDatatypeInfo Overlap

{-# LANGUAGE DeriveGeneric #-}

module System.Console.GetOpt.Generics.HintSpec where

import           Data.Proxy
import           Generics.SOP
import qualified GHC.Generics
import           Test.Hspec

import           System.Console.GetOpt.Generics.Hint

spec :: Spec
spec = do
  describe "deriveShortOptions" $ do
    it "includes hints for short options" $ do
      deriveShortOptions (Proxy :: Proxy Foo) `shouldBe` [Short "bar" 'b']

    it "doesn't include hints for short options in case of overlaps" $ do
      deriveShortOptions (Proxy :: Proxy Overlap) `shouldBe` []

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

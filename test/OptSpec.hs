{-# LANGUAGE DeriveGeneric #-}

module OptSpec where

import           Generics.SOP
import qualified GHC.Generics
import           Test.Hspec

import           Opt

spec :: Spec
spec = do
  describe "foo" $ do
    it "foos" $ do
      foo (B 4) `shouldBe` "B 4"
      foo (A "bla" 23) `shouldBe` "A {huhu = \"bla\", bla = 23}"

data Tee
  = B Int
  | A {huhu :: String, bla :: Int}
  deriving (GHC.Generics.Generic)

instance Generic Tee
instance HasDatatypeInfo Tee

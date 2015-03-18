{-# LANGUAGE DeriveGeneric #-}

module System.Console.Args.GenericsSpec where

import           Generics.SOP
import qualified GHC.Generics
import           System.Environment
import           Test.Hspec

import           System.Console.Args.Generics

spec :: Spec
spec = do
  describe "withArguments" $ do
    it "parses command line arguments" $ do
      withArgs (words "--bar 4 --baz foo") $ withArguments $ \ options ->
        options `shouldBe` Foo (Just 4) "foo" False

    it "allows optional arguments" $ do
      withArgs (words "--baz foo") $ withArguments $ \ options ->
        options `shouldBe` Foo Nothing "foo" False

data Foo
  = Foo {
    bar :: Maybe Int,
    baz :: String,
    huhu :: Bool
  }
  deriving (GHC.Generics.Generic, Show, Eq)

instance Generic Foo
instance HasDatatypeInfo Foo

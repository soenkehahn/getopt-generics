{-# LANGUAGE DeriveGeneric #-}

module System.Console.GetOpt.Generics.ModifierSpec where

import           Data.Char
import           Data.Proxy
import qualified GHC.Generics
import           Test.Hspec
import           Test.QuickCheck hiding (Result)

import           System.Console.GetOpt.Generics
import           System.Console.GetOpt.Generics.Modifier

spec :: Spec
spec = do
  describe "deriveShortOptions" $ do
    it "includes modifiers for short options" $ do
      deriveShortOptions (Proxy :: Proxy Foo) `shouldBe` [AddShortOption "bar" 'b']

    it "doesn't include modifiers for short options in case of overlaps" $ do
      deriveShortOptions (Proxy :: Proxy Overlap) `shouldBe` []

  describe "mkShortModifiers" $ do
    it "returns only lower-case ascii alpha characters as short options" $ do
      property $ \ strings ->
        mkShortModifiers strings `shouldSatisfy` all (\ (AddShortOption _ c) ->
          isLower c && isAscii c && isAlpha c)

  describe "insertWith" $ do
    it "combines existing values with the given function" $ do
      insertWith (++) (1 :: Integer) "bar" [(1, "foo")]
        `shouldBe` [(1, "foobar")]

  context "when used with Modifiers" $ do
    describe "parseArguments" $ do
      it "allows to specify a flag specific help" $ do
        let OutputAndExit output =
              parseArguments "header" [AddOptionHelp "bar" "bar help text"]
                (words "--help") :: Result Foo
        output `shouldContain` "--bar=string  bar help text"

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

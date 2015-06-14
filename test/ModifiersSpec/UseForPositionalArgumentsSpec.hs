{-# LANGUAGE DeriveGeneric #-}

module ModifiersSpec.UseForPositionalArgumentsSpec where

import           Data.List
import qualified GHC.Generics as GHC
import           Test.Hspec

import           System.Console.GetOpt.Generics
import           Util

data WithPositionalArguments
  = WithPositionalArguments {
    positionalArguments :: [String],
    someFlag :: Bool
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic WithPositionalArguments
instance HasDatatypeInfo WithPositionalArguments

data WithMultiplePositionalArguments
  = WithMultiplePositionalArguments {
    positionalArguments1 :: [String],
    positionalArguments2 :: [String],
    someOtherFlag :: Bool
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic WithMultiplePositionalArguments
instance HasDatatypeInfo WithMultiplePositionalArguments

spec :: Spec
spec = do
  it "allows positionalArguments" $ do
    modsParse
      [UseForPositionalArguments "positionalArguments" "type"]
      "foo bar --some-flag"
        `shouldBe` Success (WithPositionalArguments ["foo", "bar"] True)

  it "disallows to specify the option used for positional arguments" $ do
    modsParse
      [UseForPositionalArguments "positionalArguments" "type"]
      "--positional-arguments foo"
        `shouldBe`
      (Errors ["unrecognized option `--positional-arguments'"]
        :: Result WithPositionalArguments)

  it "complains about fields that don't have type [String]" $ do
    modsParse
      [UseForPositionalArguments "someFlag" "type"]
      "doesn't matter"
        `shouldBe`
      (Errors ["UseForPositionalArguments can only be used for fields of type [String] not Bool"]
        :: Result WithPositionalArguments)

  it "includes the type of positional arguments in the help output in upper-case" $ do
    let OutputAndExit output = modsParse
          [UseForPositionalArguments "positionalArguments" "foo"]
          "--help" :: Result WithPositionalArguments
    output `shouldSatisfy` ("prog-name [OPTIONS] [FOO]\n" `isPrefixOf`)

  it "complains about multiple PositionalArguments fields" $ do
    let modifiers =
          UseForPositionalArguments "positionalArguments1" "foo" :
          UseForPositionalArguments "positionalArguments2" "bar" :
          []
    (modsParse modifiers [] :: Result WithMultiplePositionalArguments)
      `shouldBe` Errors ["UseForPositionalArguments can only be used once"]

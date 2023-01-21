{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ModifiersSpec.UseForPositionalArgumentsSpec where

import           Data.List
import           System.Environment
import           Test.Hspec

import           Util
import           WithCli
import           WithCli.Pure

data WithPositionalArguments
  = WithPositionalArguments {
    positionalArguments :: [String],
    someFlag :: Bool
  }
  deriving (Generic, Show, Eq)

instance HasArguments WithPositionalArguments

data WithMultiplePositionalArguments
  = WithMultiplePositionalArguments {
    positionalArgumentsA :: [String],
    positionalArgumentsB :: [String],
    someOtherFlag :: Bool
  }
  deriving (Generic, Show, Eq)

instance HasArguments WithMultiplePositionalArguments

spec :: Spec
spec = do
  it "allows positionalArguments" $ do
    modsParse
      [UseForPositionalArguments "positionalArguments" "type"]
      "foo bar --some-flag"
        `shouldBe` Success (WithPositionalArguments ["foo", "bar"] True)

  it "disallows specifying the option used for positional arguments" $ do
    modsParse
      [UseForPositionalArguments "positionalArguments" "type"]
      "--positional-arguments foo"
        `shouldBe`
      (Errors "unrecognized option `--positional-arguments'\n"
        :: Result WithPositionalArguments)

  it "complains about fields that don't have type [String]" $ do
    modsParse
      [UseForPositionalArguments "someFlag" "type"]
      "doesn't matter"
        `shouldBe`
      (Errors "UseForPositionalArguments can only be used for fields of type [String] not Bool\n"
        :: Result WithPositionalArguments)

  it "includes the type of positional arguments in the help output in upper-case" $ do
    let OutputAndExit output = modsParse
          [UseForPositionalArguments "positionalArguments" "foo"]
          "--help" :: Result WithPositionalArguments
    output `shouldSatisfy` ("prog-name [OPTIONS] [FOO]\n" `isPrefixOf`)

  it "complains about multiple PositionalArguments fields" $ do
    let modifiers =
          UseForPositionalArguments "positionalArgumentsA" "foo" :
          UseForPositionalArguments "positionalArgumentsB" "bar" :
          []
    (modsParse modifiers [] :: Result WithMultiplePositionalArguments)
      `shouldBe` Errors "UseForPositionalArguments can only be used once\n"

  context "when used without selector" $ do
    it "automatically uses positional arguments for [Int]" $ do
      withArgs (words "1 2 3") $
        withCli $ \ (xs :: [Int]) -> do
          xs `shouldBe` [1, 2, 3]

    it "automatically uses positional arguments for [String]" $ do
      withArgs (words "foo bar") $
        withCli $ \ (xs :: [String]) -> do
          xs `shouldBe` (words "foo bar")

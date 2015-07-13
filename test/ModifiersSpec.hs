{-# LANGUAGE DeriveGeneric #-}

module ModifiersSpec where

import           Data.List
import           Test.Hspec

import           System.Console.GetOpt.Generics
import           System.Console.GetOpt.GenericsSpec
import           Util

spec :: Spec
spec = do
  describe "AddShortOption" $ do
    it "allows modifiers for short options" $ do
      modsParse [AddShortOption "camel-case" 'x'] "-x foo"
        `shouldBe` Success (CamelCaseOptions "foo")

    it "allows modifiers in camelCase" $ do
      modsParse [AddShortOption "camelCase" 'x'] "-x foo"
        `shouldBe` Success (CamelCaseOptions "foo")

    let parse' :: String -> Result CamelCaseOptions
        parse' = modsParse [AddShortOption "camelCase" 'x']
    it "includes the short option in the help" $ do
      let OutputAndExit output = parse' "--help"
      output `shouldContain` "-x STRING"

  describe "RenameOption" $ do
    it "allows to rename options" $ do
      modsParse [RenameOption "camelCase" "bla"] "--bla foo"
        `shouldBe` Success (CamelCaseOptions "foo")

    let parse' = modsParse [RenameOption "camelCase" "foo", RenameOption "camelCase" "bar"]
    it "allows to shadow earlier modifiers with later modifiers" $ do
      parse' "--bar foo" `shouldBe` Success (CamelCaseOptions "foo")
      let Errors errs = parse' "--foo foo"
      show errs `shouldContain` "unknown argument: foo"

    it "contains renamed options in error messages" $ do
      let Errors errs = parse' []
      show errs `shouldNotContain` "camelCase"
      show errs `shouldContain` "camel-case"

    it "allows to address fields in Modifiers in slugified form" $ do
      modsParse [RenameOption "camel-case" "foo"] "--foo bar"
        `shouldBe` Success (CamelCaseOptions "bar")

  describe "AddVersionFlag" $ do
    it "implements --version" $ do
      let OutputAndExit output = modsParse [AddVersionFlag "1.0.0"] "--version" :: Result Foo
      output `shouldBe` "prog-name version 1.0.0\n"

    it "--help takes precedence over --version" $ do
      let OutputAndExit output = modsParse [AddVersionFlag "1.0.0"] "--version --help" :: Result Foo
      output `shouldSatisfy` ("show help and exit" `isInfixOf`)

    it "--version shows up in help output" $ do
      let OutputAndExit output = modsParse [AddVersionFlag "1.0.0"] "--help" :: Result Foo
      output `shouldSatisfy` ("show version and exit" `isInfixOf`)

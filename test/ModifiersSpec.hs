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
      modsParse [AddShortOption "camelCase" 'x'] "-x foo"
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

    context "when shadowing earlier modifiers with later modifiers" $ do
      let parse' = modsParse
            [RenameOption "camelCase" "foo", RenameOption "camelCase" "bar"]
      it "uses the later renaming" $ do
        parse' "--bar foo" `shouldBe` Success (CamelCaseOptions "foo")

      it "disregards the earlier renaming" $ do
        let Errors errs = parse' "--foo foo"
        errs `shouldContain` ["unrecognized option `--foo'\n"]

    it "contains renamed options in error messages" $ do
      let Errors errs = modsParse
            [RenameOption "camelCase" "foo"]
            "" :: Result CamelCaseOptions
      -- _ <- error $ show errs
      show errs `shouldNotContain` "camelCase"
      show errs `shouldNotContain` "camel-case"
      show errs `shouldContain` "foo"

    it "" $ do
      modsParse [RenameOption "bar" "one", RenameOption "baz" "two"] "--one 1 --two foo"
        `shouldBe` Success (Foo (Just 1) "foo" False)

  describe "AddVersionFlag" $ do
    it "implements --version" $ do
      let OutputAndExit output = modsParse [AddVersionFlag "1.0.0"] "--version" :: Result Foo
      output `shouldBe` "prog-name version 1.0.0"

    it "--help takes precedence over --version" $ do
      let OutputAndExit output = modsParse [AddVersionFlag "1.0.0"] "--version --help" :: Result Foo
      output `shouldSatisfy` ("show help and exit" `isInfixOf`)

    it "--version shows up in help output" $ do
      let OutputAndExit output = modsParse [AddVersionFlag "1.0.0"] "--help" :: Result Foo
      output `shouldSatisfy` ("show version and exit" `isInfixOf`)

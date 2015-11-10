{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WithCli.Pure.RecordSpec where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           Data.Foldable (forM_)
import           Data.List (isPrefixOf, isSuffixOf)
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           Util
import           WithCli.Pure

spec :: Spec
spec = do
  part1
  part2
  part3
  part4
  part5

data Foo
  = Foo {
    bar :: Maybe Int,
    baz :: String,
    bool :: Bool
  }
  deriving (Generic, Show, Eq)

instance HasArguments Foo

data NotAllowed
  = NotAllowed1
  | NotAllowed2
  deriving (Generic, Show, Eq)

instance HasArguments NotAllowed

part1 :: Spec
part1 = do
  describe "withCliPure (record types)" $ do
    it "allows optional arguments" $ do
      parse "--baz foo" `shouldBe`
        Success (Foo Nothing "foo" False)

    it "allows boolean flags" $ do
      parse "--bool --baz foo" `shouldBe`
        Success (Foo Nothing "foo" True)

    it "allows to overwrite String options" $ do
      parse "--baz one --baz two"
        `shouldBe` Success (Foo Nothing "two" False)

    context "with invalid arguments" $ do
      it "prints out an error" $ do
        let Errors messages = parse "--no-such-option" :: Result Foo
        messages `shouldBe`
          "unrecognized option `--no-such-option'\n" ++
          "missing option: --baz=STRING\n"

      it "prints errors for missing options" $ do
        let Errors messages = parse [] :: Result Foo
        messages `shouldBe` "missing option: --baz=STRING\n"

      it "prints out an error for unparseable options" $ do
        let Errors messages = parse "--bar foo --baz huhu" :: Result Foo
        messages `shouldBe` "cannot parse as INTEGER (optional): foo\n"

      it "complains about unused positional arguments" $ do
        (parse "--baz foo unused" :: Result Foo)
          `shouldBe` Errors "unknown argument: unused\n"

      it "complains about invalid overwritten options" $ do
        let Errors messages = parse "--bar foo --baz huhu --bar 12" :: Result Foo
        messages `shouldBe` "cannot parse as INTEGER (optional): foo\n"

    context "--help" $ do
      it "implements --help" $ do
        let OutputAndExit output = parse "--help" :: Result Foo
        mapM_ (output `shouldContain`) $
          "--bar=INTEGER" : "optional" :
          "--baz=STRING" :
          "--bool" :
          []
        lines output `shouldSatisfy` (not . ("" `elem`))

      it "contains help message about --help" $ do
        let OutputAndExit output = parse "--help" :: Result Foo
        output `shouldContain` "show help and exit"

      it "does not contain trailing spaces" $ do
        output <-
          hCapture_ [stdout] $
          handle (\ ExitSuccess -> return ()) $
          handleResult $ ((parse "--help" :: Result Foo) >> return ())
        forM_ (lines output) $ \ line -> do
          line `shouldSatisfy` (not . (" " `isSuffixOf`))

      it "complains when the options datatype is not allowed" $ do
        let Errors messages = parse "--help" :: Result NotAllowed
        messages `shouldSatisfy` ("getopt-generics doesn't support sum types" `isPrefixOf`)

      it "outputs a header including \"[OPTIONS]\"" $ do
        let OutputAndExit output = parse "--help" :: Result Foo
        output `shouldSatisfy` ("prog-name [OPTIONS]\n" `isPrefixOf`)

data ListOptions
  = ListOptions {
    multiple :: [Int]
  }
  deriving (Generic, Show, Eq)

instance HasArguments ListOptions

part2 :: Spec
part2 = do
  describe "parseArguments" $ do
    it "allows to interpret multiple uses of the same option as lists" $ do
      parse "--multiple 23 --multiple 42"
        `shouldBe` Success (ListOptions [23, 42])

    it "complains about invalid list arguments" $ do
      let Errors errs =
            parse "--multiple foo --multiple 13" :: Result ListOptions
      errs `shouldBe` "cannot parse as INTEGER (multiple possible): foo\n"

data CamelCaseOptions
  = CamelCaseOptions {
    camelCase :: String
  }
  deriving (Generic, Show, Eq)

instance HasArguments CamelCaseOptions

part3 :: Spec
part3 = do
  describe "parseArguments" $ do
    it "turns camelCase selectors to lowercase and seperates with a dash" $ do
      parse "--camel-case foo" `shouldBe` Success (CamelCaseOptions "foo")

    it "help does not contain camelCase flags" $ do
      let OutputAndExit output :: Result CamelCaseOptions
            = parse "--help"
      output `shouldNotContain` "camelCase"
      output `shouldContain` "camel-case"

    it "error messages don't contain camelCase flags" $ do
      let Errors errs :: Result CamelCaseOptions
            = parse "--bla"
      show errs `shouldNotContain` "camelCase"
      show errs `shouldContain` "camel-case"

data WithUnderscore
  = WithUnderscore {
    _withUnderscore :: String
  }
  deriving (Generic, Show, Eq)

instance HasArguments WithUnderscore

part4 :: Spec
part4 = do
  describe "parseArguments" $ do
    it "ignores leading underscores in field names" $ do
      parse "--with-underscore foo"
        `shouldBe` Success (WithUnderscore "foo")

data WithoutSelectors
  = WithoutSelectors String Bool Int
  deriving (Eq, Show, Generic)

instance HasArguments WithoutSelectors

part5 :: Spec
part5 = do
  describe "parseArguments" $ do
    context "WithoutSelectors" $ do
      it "populates fields without selectors from positional arguments" $ do
        parse "foo true 23"
          `shouldBe` Success (WithoutSelectors "foo" True 23)

      it "has good help output for positional arguments" $ do
        let OutputAndExit output = parse "--help" :: Result WithoutSelectors
        output `shouldSatisfy` ("prog-name [OPTIONS] STRING BOOL INTEGER" `isPrefixOf`)

      it "has good error messages for missing positional arguments" $ do
        (parse "foo" :: Result WithoutSelectors)
          `shouldBe` Errors
            ("missing argument of type BOOL\n" ++
             "missing argument of type INTEGER\n")

      it "complains about additional positional arguments" $ do
        (parse "foo true 5 bar" :: Result WithoutSelectors)
          `shouldBe` Errors "unknown argument: bar\n"

      it "allows to use tuples" $ do
        (parse "42 bar" :: Result (Int, String))
          `shouldBe` Success (42, "bar")

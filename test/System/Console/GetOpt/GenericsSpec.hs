{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Console.GetOpt.GenericsSpec where

import           Prelude ()
import           Prelude.Compat

import           Data.Foldable (forM_)
import           Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import           Data.Typeable
import qualified GHC.Generics as GHC
import           System.Environment
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib
import           Test.QuickCheck hiding (Result(..))

import           System.Console.GetOpt.Generics

spec :: Spec
spec = do
  part1
  part2
  part3
  part4
  part5
  part6
  part7
  part8

parse :: (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  String -> Result a
parse = modsParse []

modsParse :: (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  [Modifier] -> String -> Result a
modsParse modifiers = parseArguments "prog-name" modifiers . words

data Foo
  = Foo {
    bar :: Maybe Int,
    baz :: String,
    bool :: Bool
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic Foo
instance HasDatatypeInfo Foo

data NotAllowed
  = NotAllowed1
  | NotAllowed2
  deriving (GHC.Generic, Show, Eq)

instance Generic NotAllowed
instance HasDatatypeInfo NotAllowed

part1 :: Spec
part1 = do
  describe "getArguments" $ do
    it "parses command line arguments" $ do
      withArgs (words "--bar 4 --baz foo") $
        getArguments `shouldReturn` Foo (Just 4) "foo" False

  describe "parseArguments" $ do
    it "allows optional arguments" $ do
      parse "--baz foo" `shouldBe`
        Success (Foo Nothing "foo" False)

    it "allows boolean flags" $ do
      parse "--bool --baz foo" `shouldBe`
        Success (Foo Nothing "foo" True)

    context "with invalid arguments" $ do
      it "prints out an error" $ do
        let Errors messages = parse "--no-such-option" :: Result Foo
        messages `shouldBe`
          ["unrecognized option `--no-such-option'\n",
           "missing option: --baz=STRING"]

      it "prints errors for missing options" $ do
        let Errors [message] = parse [] :: Result Foo
        message `shouldBe` "missing option: --baz=STRING"

      it "prints out an error for unparseable options" $ do
        let Errors [message] = parse "--bar foo --baz huhu" :: Result Foo
        message `shouldBe` "cannot parse as INTEGER (optional): foo"

      it "complains about unused positional arguments" $ do
        (parse "--baz foo unused" :: Result Foo)
          `shouldBe` Errors ["unknown argument: unused"]

      it "complains about invalid overwritten options" $ do
        let Errors [message] = parse "--bar foo --baz huhu --bar 12" :: Result Foo
        message `shouldBe` "cannot parse as INTEGER (optional): foo"

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
        let OutputAndExit output = parse "--help" :: Result Foo
        forM_ (lines output) $ \ line ->
          line `shouldSatisfy` (not . (" " `isSuffixOf`))

      it "complains when the options datatype is not allowed" $ do
        let Errors [message] = parse "--help" :: Result NotAllowed
        message `shouldSatisfy` ("getopt-generics doesn't support sum types" `isPrefixOf`)

      it "outputs a header including \"[OPTIONS]\"" $ do
        let OutputAndExit output = parse "--help" :: Result Foo
        output `shouldSatisfy` ("prog-name [OPTIONS]\n" `isPrefixOf`)

  describe "parseArguments" $ do
    it "allows to overwrite String options" $ do
      parse "--baz one --baz two"
        `shouldBe` Success (Foo Nothing "two" False)

data ListOptions
  = ListOptions {
    multiple :: [Int]
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic ListOptions
instance HasDatatypeInfo ListOptions

part2 :: Spec
part2 = do
  describe "parseArguments" $ do
    it "allows to interpret multiple uses of the same option as lists" $ do
      parse "--multiple 23 --multiple 42"
        `shouldBe` Success (ListOptions [23, 42])

    it "complains about invalid list arguments" $ do
      let Errors errs =
            parse "--multiple foo --multiple 13" :: Result ListOptions
      errs `shouldBe` ["cannot parse as INTEGER (multiple possible): foo"]

data CamelCaseOptions
  = CamelCaseOptions {
    camelCase :: String
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic CamelCaseOptions
instance HasDatatypeInfo CamelCaseOptions

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

    context "AddShortOption" $ do
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

    context "RenameOption" $ do
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

data WithUnderscore
  = WithUnderscore {
    _withUnderscore :: String
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic WithUnderscore
instance HasDatatypeInfo WithUnderscore

part4 :: Spec
part4 = do
  describe "parseArguments" $ do
    it "ignores leading underscores in field names" $ do
      parse "--with-underscore foo"
        `shouldBe` Success (WithUnderscore "foo")

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

part5 :: Spec
part5 = do
  describe "parseArguments" $ do
    context "UseForPositionalArguments" $ do
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
          (Errors ["unrecognized option `--positional-arguments'\n"]
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

data CustomFields
  = CustomFields {
    custom :: Custom,
    customList :: [Custom],
    customMaybe :: Maybe Custom
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic CustomFields
instance HasDatatypeInfo CustomFields

data Custom
  = CFoo
  | CBar
  | CBaz
  deriving (Show, Eq, Typeable)

instance Option Custom where
  argumentType Proxy = "custom"
  parseArgument x = case x of
    "foo" -> Just CFoo
    "bar" -> Just CBar
    "baz" -> Just CBaz
    _ -> Nothing

part6 :: Spec
part6 = do
  describe "parseArguments" $ do
    context "CustomFields" $ do
      it "allows easy implementation of custom field types" $ do
        parse "--custom foo --custom-list bar --custom-maybe baz"
          `shouldBe` Success (CustomFields CFoo [CBar] (Just CBaz))

data WithoutSelectors
  = WithoutSelectors String Bool Int
  deriving (Eq, Show, GHC.Generic)

instance Generic WithoutSelectors
instance HasDatatypeInfo WithoutSelectors

part7 :: Spec
part7 = do
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
          `shouldBe` Errors (
            "missing argument of type BOOL" :
            "missing argument of type INTEGER" :
            [])

      it "complains about additional positional arguments" $ do
        (parse "foo true 5 bar" :: Result WithoutSelectors)
          `shouldBe` Errors ["unknown argument: bar"]

      it "allows to use tuples" $ do
        (parse "42 bar" :: Result (Int, String))
          `shouldBe` Success (42, "bar")

  describe "Option.Bool" $ do
    describe "parseArgument" $ do
      it "parses 'true' case-insensitively" $ do
        forM_ ["true", "True", "tRue", "TRUE"] $ \ true ->
          parseArgument true `shouldBe` Just True

      it "parses 'false' case-insensitively" $ do
        forM_ ["false", "False", "falSE", "FALSE"] $ \ true ->
          parseArgument true `shouldBe` Just False

      it "parses every positive integer as true" $ do
        property $ \ (n :: Int) ->
          n > 0 ==>
          parseArgument (show n) `shouldBe` Just True

      it "parses every non-positive integer as false" $ do
        property $ \ (n :: Int) ->
          n <= 0 ==>
          parseArgument (show n) `shouldBe` Just False

      it "doesn't parse 'foo'" $ do
        parseArgument "foo" `shouldBe` (Nothing :: Maybe Bool)

  describe "Option.Double" $ do
    it "parses doubles" $ do
      parseArgument "1.2" `shouldBe` Just (1.2 :: Double)

    it "renders as NUMBER in help and error output" $ do
      argumentType (Proxy :: Proxy Double) `shouldBe` "NUMBER"

    it "parses doubles that start with a dot" $ do
      parseArgument ".4" `shouldBe` Just (0.4 :: Double)

  describe "Option.Float" $ do
    it "parses floats" $ do
      parseArgument "1.2" `shouldBe` Just (1.2 :: Float)

    it "renders as NUMBER in help and error output" $ do
      argumentType (Proxy :: Proxy Float) `shouldBe` "NUMBER"

part8 :: Spec
part8 = do
  describe "parseArgument" $ do
    context "--version" $ do
      it "implements --version" $ do
        let OutputAndExit output = modsParse [AddVersionFlag "1.0.0"] "--version" :: Result Foo
        output `shouldBe` "prog-name version 1.0.0\n"

      it "--help takes precedence over --version" $ do
        let OutputAndExit output = modsParse [AddVersionFlag "1.0.0"] "--version --help" :: Result Foo
        output `shouldSatisfy` ("show help and exit" `isInfixOf`)

      it "--version shows up in help output" $ do
        let OutputAndExit output = modsParse [AddVersionFlag "1.0.0"] "--help" :: Result Foo
        output `shouldSatisfy` ("show version and exit" `isInfixOf`)

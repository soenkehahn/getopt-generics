{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Console.GetOpt.GenericsSpec where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           Data.Foldable (forM_)
import           Data.List (isPrefixOf, isSuffixOf)
import           Data.Typeable
import qualified GHC.Generics as GHC
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
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

    it "allows optional arguments" $ do
      withArgs (words "--baz foo") $
        getArguments `shouldReturn` Foo Nothing "foo" False

    it "allows boolean flags" $ do
      withArgs (words "--bool --baz foo") $
        getArguments `shouldReturn` Foo Nothing "foo" True

    context "with invalid arguments" $ do
      it "doesn't execute the action" $ do
        let main = withArgs (words "--invalid") $ do
              _ :: Foo <- getArguments
              throwIO (ErrorCall "action")
        main `shouldThrow` (== ExitFailure 1)

      it "prints out an error" $ do
        output <- hCapture_ [stderr] $ handle (\ (_ :: SomeException) -> return ()) $
          withArgs (words "--no-such-option") $ do
            _ :: Foo <- getArguments
            return ()
        output `shouldBe` "unrecognized option `--no-such-option'\nmissing option: --baz=string\n"

      it "prints errors for missing options" $ do
        output <- hCapture_ [stderr] $ handle (\ (_ :: SomeException) -> return ()) $
          withArgs [] $ do
            _ :: Foo <- getArguments
            return ()
        output `shouldContain` "missing option: --baz=string"
        output `shouldSatisfy` ("\n" `isSuffixOf`)

      it "prints out an error for unparseable options" $ do
        output <- hCapture_ [stderr] $ handle (\ (_ :: SomeException) -> return ()) $
          withArgs (words "--bar foo --baz huhu") $ do
            _ :: Foo <- getArguments
            return ()
        output `shouldBe` "cannot parse as integer (optional): foo\n"

      it "complains about unused positional arguments" $ do
        (parseArguments "prog-name" [] (words "--baz foo unused") :: Result Foo)
          `shouldBe` Errors ["unknown argument: unused"]

      it "complains about invalid overwritten options" $ do
        output <- hCapture_ [stderr] $ handle (\ (_ :: SomeException) -> return ()) $
          withArgs (words "--bar foo --baz huhu --bar 12") $ do
            _ :: Foo <- getArguments
            return ()
        output `shouldBe` "cannot parse as integer (optional): foo\n"

    context "--help" $ do
      it "implements --help" $ do
        output <- capture_ $ withArgs ["--help"] $
          handle (\ (_ :: SomeException) -> return ()) $ do
            _ :: Foo <- getArguments
            return ()
        mapM_ (output `shouldContain`) $
          "--bar=integer" : "optional" :
          "--baz=string" :
          "--bool" :
          []
        lines output `shouldSatisfy` (not . ("" `elem`))

      it "throws ExitSuccess" $ do
        withArgs ["--help"] (getArguments :: IO Foo)
          `shouldThrow` (== ExitSuccess)

      it "contains help message about --help" $ do
        output <- capture_ $ withArgs ["--help"] $
          handle (\ (_ :: SomeException) -> return ()) $ do
            _ :: Foo <- getArguments
            return ()
        output `shouldContain` "show help and exit"

      it "does not contain trailing spaces" $ do
        output <- capture_ $ withArgs ["--help"] $
          handle (\ (_ :: SomeException) -> return ()) $ do
            _ :: Foo <- getArguments
            return ()
        forM_ (lines output) $ \ line ->
          line `shouldSatisfy` (not . (" " `isSuffixOf`))

      it "throws an exception when the options datatype is not allowed" $ do
        output <- hCapture_ [stderr] $
          withArgs ["--help"] $
          handle (\ (_ :: SomeException) -> return ()) $ do
             _ :: NotAllowed <- getArguments
             return ()
        output `shouldContain` "getopt-generics doesn't support sum-types"
        lines output `shouldSatisfy` (not . ("" `elem`))

      it "outputs a header including \"[OPTIONS]\"" $ do
        let OutputAndExit output =
              parseArguments "prog-name" [] ["--help"] :: Result Foo
        output `shouldSatisfy` ("prog-name [OPTIONS]\n" `isPrefixOf`)

  describe "parseArguments" $ do
    it "allows to overwrite String options" $ do
      parseArguments "header" [] (words "--baz one --baz two")
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
  describe "getArguments" $ do
    it "allows to interpret multiple uses of the same option as lists" $ do
      withArgs (words "--multiple 23 --multiple 42") $ do
        getArguments `shouldReturn` ListOptions [23, 42]

    it "complains about invalid list arguments" $ do
        output <- hCapture_ [stderr] $
          withArgs (words "--multiple foo --multiple 13") $
          handle (\ (_ :: SomeException) -> return ()) $ do
            _ :: ListOptions <- getArguments
            return ()
        output `shouldBe` "cannot parse as integer (multiple possible): foo\n"

data CamelCaseOptions
  = CamelCaseOptions {
    camelCase :: String
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic CamelCaseOptions
instance HasDatatypeInfo CamelCaseOptions

part3 :: Spec
part3 = do
  describe "getArguments" $ do
    it "turns camelCase selectors to lowercase and seperates with a dash" $ do
        withArgs (words "--camel-case foo") $ do
          getArguments `shouldReturn` CamelCaseOptions "foo"

  describe "parseArguments" $ do
    it "help does not contain camelCase flags" $ do
      let OutputAndExit output :: Result CamelCaseOptions
            = parseArguments "prog-name" [] ["--help"]
      output `shouldNotContain` "camelCase"
      output `shouldContain` "camel-case"

    it "error messages don't contain camelCase flags" $ do
      let Errors errs :: Result CamelCaseOptions
            = parseArguments "prog-name" [] ["--bla"]
      show errs `shouldNotContain` "camelCase"
      show errs `shouldContain` "camel-case"

    context "AddShortOption" $ do
      it "allows modifiers for short options" $ do
        parseArguments "prog-name" [AddShortOption "camel-case" 'x'] (words "-x foo")
          `shouldBe` Success (CamelCaseOptions "foo")

      it "allows modifiers in camelCase" $ do
        parseArguments "prog-name" [AddShortOption "camelCase" 'x'] (words "-x foo")
          `shouldBe` Success (CamelCaseOptions "foo")

      let parse :: [String] -> Result CamelCaseOptions
          parse = parseArguments "prog-name" [AddShortOption "camelCase" 'x']
      it "includes the short option in the help" $ do
        let OutputAndExit output = parse ["--help"]
        output `shouldContain` "-x string"

    context "RenameOption" $ do
      it "allows to rename options" $ do
        parseArguments "prog-name" [RenameOption "camelCase" "bla"] (words "--bla foo")
          `shouldBe` Success (CamelCaseOptions "foo")

      let parse = parseArguments "prog-name"
            [RenameOption "camelCase" "foo", RenameOption "camelCase" "bar"]
      it "allows to shadow earlier modifiers with later modifiers" $ do
        parse (words "--bar foo")
          `shouldBe` Success (CamelCaseOptions "foo")
        let Errors errs = parse (words "--foo foo")
        show errs `shouldContain` "unknown argument: foo"

      it "contains renamed options in error messages" $ do
        let Errors errs = parse []
        show errs `shouldNotContain` "camelCase"
        show errs `shouldContain` "camel-case"

      it "allows to address fields in Modifiers in slugified form" $ do
        parseArguments "prog-name" [RenameOption "camel-case" "foo"] (words "--foo bar")
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
      parseArguments "prog-name" [] (words "--with-underscore foo")
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
        parseArguments "prog-name"
          [UseForPositionalArguments "positionalArguments" "type"]
          (words "foo bar --some-flag")
            `shouldBe` Success (WithPositionalArguments ["foo", "bar"] True)

      it "disallows to specify the option used for positional arguments" $ do
        parseArguments "prog-name"
          [UseForPositionalArguments "positionalArguments" "type"]
          (words "--positional-arguments foo")
            `shouldBe`
          (Errors ["unrecognized option `--positional-arguments'\n"]
            :: Result WithPositionalArguments)

      it "complains about fields that don't have type [String]" $ do
        parseArguments "prog-name"
          [UseForPositionalArguments "someFlag" "type"]
          (words "doesn't matter")
            `shouldBe`
          (Errors ["UseForPositionalArguments can only be used for fields of type [String] not Bool"]
            :: Result WithPositionalArguments)

      it "includes the type of positional arguments in the help output in upper-case" $ do
        let OutputAndExit output = parseArguments "prog-name"
              [UseForPositionalArguments "positionalArguments" "foo"]
              (words "--help") :: Result WithPositionalArguments
        output `shouldSatisfy` ("prog-name [OPTIONS] [FOO]\n" `isPrefixOf`)

      it "complains about multiple PositionalArguments fields" $ do
        let modifiers =
              UseForPositionalArguments "positionalArguments1" "foo" :
              UseForPositionalArguments "positionalArguments2" "bar" :
              []
        (parseArguments "prog-name" modifiers [] :: Result WithMultiplePositionalArguments)
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
        parseArguments "prog-name" []
            (words "--custom foo --custom-list bar --custom-maybe baz")
          `shouldBe`
            Success (CustomFields CFoo [CBar] (Just CBaz))

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
        parseArguments "prog-name" [] (words "foo true 23")
          `shouldBe` Success (WithoutSelectors "foo" True 23)

      it "has good help output for positional arguments" $ do
        let OutputAndExit output = parseArguments "prog-name" [] ["--help"] :: Result WithoutSelectors
        output `shouldSatisfy` ("prog-name [OPTIONS] STRING BOOL INTEGER" `isPrefixOf`)

      it "has good error messages for missing positional arguments" $ do
        (parseArguments "prog-name" [] (words "foo") :: Result WithoutSelectors)
          `shouldBe` Errors (
            "missing argument of type BOOL" :
            "missing argument of type INTEGER" :
            [])

      it "complains about additional positional arguments" $ do
        (parseArguments "prog-name" [] (words "foo true 5 bar") :: Result WithoutSelectors)
          `shouldBe` Errors ["unknown argument: bar"]

      it "allows to use tuples" $ do
        (parseArguments "prog-name" [] (words "42 bar") :: Result (Int, String))
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

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Console.GetOpt.GenericsSpec where

import           Control.Exception
import           Generics.SOP
import qualified GHC.Generics                 as GHC
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           System.Console.GetOpt.Generics

data Foo
  = Foo {
    bar :: Maybe Int,
    baz :: String,
    bool :: Bool
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic Foo
instance HasDatatypeInfo Foo

spec :: Spec
spec = do
  describe "withArguments" $ do
    it "parses command line arguments" $ do
      withArgs (words "--bar 4 --baz foo") $ withArguments $ \ options ->
        options `shouldBe` Foo (Just 4) "foo" False

    it "allows optional arguments" $ do
      withArgs (words "--baz foo") $ withArguments $ \ options ->
        options `shouldBe` Foo Nothing "foo" False

    it "allows boolean flags" $ do
      withArgs (words "--bool --baz foo") $ withArguments $ \ options ->
        options `shouldBe` Foo Nothing "foo" True

    context "invalid arguments" $ do
      it "doesn't execute the action" $ do
        let main = withArgs (words "--invalid") $
              withArguments $ \ (_ :: Foo) ->
                throwIO (ErrorCall "action")
        main `shouldThrow` (== ExitFailure 1)

      it "prints out an error" $ do
        output <- hCapture_ [stderr] $ handle (\ (_ :: SomeException) -> return ()) $
          withArgs (words "--no-such-option") $
            withArguments $ \ (_ :: Foo) -> return ()
        output `shouldContain` "unrecognized"
        output `shouldContain` "--no-such-option"

      it "prints errors for missing options" $ do
        output <- hCapture_ [stderr] $ handle (\ (_ :: SomeException) -> return ()) $
          withArgs [] $
            withArguments $ \ (_ :: Foo) -> return ()
        output `shouldContain` "missing option: --baz=string"

      it "prints out an error for unparseable options" $ do
        output <- hCapture_ [stderr] $ handle (\ (_ :: SomeException) -> return ()) $
          withArgs (words "--bar foo --baz huhu") $
            withArguments $ \ (_ :: Foo) -> return ()
        output `shouldContain` "not an integer: foo"

      it "complains about invalid overwritten options" $ do
        output <- hCapture_ [stderr] $ handle (\ (_ :: SomeException) -> return ()) $
          withArgs (words "--bar foo --baz huhu --bar 12") $
            withArguments $ \ (_ :: Foo) -> return ()
        output `shouldContain` "not an integer: foo"

    it "implements --help" $ do
      output <- capture_ $
        withArgs ["--help"] $ withArguments $ \ (_ :: Foo) ->
          throwIO (ErrorCall "action")
      mapM_ (output `shouldContain`) $
        "--bar=integer" : "optional" :
        "--baz=string" :
        "--bool" :
        []

  next1

data ListOptions
  = ListOptions {
    multiple :: [String]
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic ListOptions
instance HasDatatypeInfo ListOptions

next1 :: Spec
next1 = do
  describe "withArguments" $ do
    it "allows to interpret multiple uses of the same option as lists" $ do
      withArgs (words "--multiple foo --multiple bar") $
        withArguments $ \ options ->
          options `shouldBe` ListOptions ["foo", "bar"]
  next2

data CamelCaseOptions
  = CamelCaseOptions {
    camelCase :: String
  }
  deriving (GHC.Generic, Show, Eq)

instance Generic CamelCaseOptions
instance HasDatatypeInfo CamelCaseOptions

next2 :: Spec
next2 = do
  describe "withArguments" $ do
    it "turns camelCase selectors to lowercase and seperates with a dash" $ do
        withArgs (words "--camel-case foo") $ withArguments $ \ options ->
          options `shouldBe` CamelCaseOptions "foo"

  describe "parseArguments" $ do
    it "allows hints for short options" $ do
      parseArguments "header" [Short "camel-case" 'x'] (words "-x foo")
        `shouldBe` Success (CamelCaseOptions "foo")

    it "allows hints in camelCase" $ do
      parseArguments "header" [Short "camelCase" 'x'] (words "-x foo")
        `shouldBe` Success (CamelCaseOptions "foo")

{-# LANGUAGE ScopedTypeVariables #-}

module SimpleCLI.ResultSpec where

import           Control.Exception
import           Data.List
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec
import           Test.QuickCheck hiding (Result(..))

import           SimpleCLI.Result

spec :: Spec
spec = do
  describe "Result" $ do
    context ">>" $ do
      it "collects errors" $ do
        (Errors ["foo"] >> Errors ["bar"] :: Result ())
          `shouldBe` Errors ["foo", "bar"]

  describe "errors" $ do
    it "removes trailing newlines" $ do
      (errors ["foo\n", "bar", "baz\n"] :: Result ()) `shouldBe`
        Errors ["foo", "bar", "baz"]

  describe "outputAndExit" $ do
    it "removes trailing spaces" $ do
      (outputAndExit "foo \nbar" :: Result ()) `shouldBe`
        OutputAndExit "foo\nbar"

    it "removes trailing spaces at the end" $ do
      (outputAndExit "foo " :: Result ()) `shouldBe`
        OutputAndExit "foo"

    it "quickcheck" $ do
      property $ \ s ->
        let OutputAndExit output = outputAndExit s
        in output `shouldSatisfy` (not . (" \n" `isInfixOf`))

    it "only strips spaces" $ do
      property $ \ s ->
        let OutputAndExit output = outputAndExit s
        in
          filter (/= ' ') output
            `shouldBe`
          filter (/= ' ') s

  describe "handleResult" $ do
    it "appends '\\n' at the end of error messages if missing" $ do
      output <- hCapture_ [stderr] $ do
        handle (\ (_ :: ExitCode) -> return ()) $ do
          _ <- handleResult (Errors ["foo", "bar\n", "baz"])
          return ()
      output `shouldBe` "foo\nbar\nbaz\n"

    context "OutputAndExit" $ do
      it "throws ExitSuccess" $ do
        handleResult (OutputAndExit "foo")
          `shouldThrow` (== ExitSuccess)

    context "Errors" $ do
      it "throws an ExitFailure" $ do
        handleResult (Errors ["foo"])
          `shouldThrow` (== ExitFailure 1)

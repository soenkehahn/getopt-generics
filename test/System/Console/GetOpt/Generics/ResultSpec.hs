{-# LANGUAGE ScopedTypeVariables #-}

module System.Console.GetOpt.Generics.ResultSpec where

import           Control.Exception
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           System.Console.GetOpt.Generics.Result

spec :: Spec
spec = do
  describe "Result" $ do
    context ">>" $ do
      it "collects errors" $ do
        (Errors ["foo"] >> Errors ["bar"] :: Result ())
          `shouldBe` Errors ["foo", "bar"]

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

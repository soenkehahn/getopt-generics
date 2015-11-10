{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module WithCli.ResultSpec where

import           Data.Char
import           Data.List
import           Safe
import           System.Exit
import           Test.Hspec
import           Test.QuickCheck hiding (Result(..))

import           WithCli.Result

spec :: Spec
spec = do
  describe "Result" $ do
    context ">>" $ do
      it "collects errors" $ do
        (Errors "foo" >> Errors "bar" :: Result ())
          `shouldBe` Errors "foo\nbar"

    describe "|>" $ do
      it "shortcuts directly without collecting other errors" $ do
        (Errors "foo" |> Errors "bar" :: Result ()) `shouldBe` Errors "foo"
        return () |> return () `shouldBe` return ()

  describe "handleResult" $ do
    context "OutputAndExit" $ do
      it "throws ExitSuccess" $ do
        handleResult (OutputAndExit "foo")
          `shouldThrow` (== ExitSuccess)

    context "Errors" $ do
      it "throws an ExitFailure" $ do
        handleResult (Errors "foo")
          `shouldThrow` (== ExitFailure 1)

  describe "sanitizeMessage" $ do
    it "removes empty lines" $ do
      property $ \ (unlines -> s) -> do
        sanitizeMessage s `shouldNotContain` "\n\n"

    it "adds a newline at the end if missing" $ do
      property $ \ (unlines -> s) ->
        not (null (sanitizeMessage s)) ==>
        lastMay (sanitizeMessage s) `shouldBe` Just '\n'

    it "only strips spaces" $ do
      property $ \ (unlines -> s) -> do
        counterexample s $ do
          let expected = case s of
                "" -> ""
                x | lastMay x == Just '\n' -> x
                x -> x ++ "\n"
          filter (not . isSpace) (sanitizeMessage s) `shouldBe` filter (not . isSpace) expected

    it "removes trailing spaces" $ do
      property $ \ (unlines -> s) -> do
        sanitizeMessage s `shouldSatisfy` (not . (" \n" `isInfixOf`))

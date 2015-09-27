{-# LANGUAGE ScopedTypeVariables #-}

module WithCli.HasArgumentsSpec where

import           Control.Monad
import           Test.Hspec
import           Test.QuickCheck

import           WithCli.HasArguments

spec :: Spec
spec = do
  describe "parseBool" $ do
    forM_ ["true", "True", "tRue", "TRUE", "yes", "yEs", "on", "oN"] $ \ true ->
      it ("parses '" ++ true ++ "' as True") $ do
        parseBool true `shouldBe` Just True

    forM_ ["false", "False", "falSE", "FALSE", "no", "nO", "off", "ofF"] $ \ false ->
      it ("parses '" ++ false ++ "' as False") $ do
        parseBool false `shouldBe` Just False

    it "parses every positive integer as true" $ do
      property $ \ (n :: Int) ->
        n > 0 ==>
        parseBool (show n) `shouldBe` Just True

    it "parses every non-positive integer as false" $ do
      property $ \ (n :: Int) ->
        n <= 0 ==>
        parseBool (show n) `shouldBe` Just False

    it "doesn't parse 'foo'" $ do
      parseBool "foo" `shouldBe` (Nothing :: Maybe Bool)


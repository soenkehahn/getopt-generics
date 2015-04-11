{-# LANGUAGE ScopedTypeVariables #-}

module System.Console.GetOpt.Generics.ResultSpec where

import           Test.Hspec

import           System.Console.GetOpt.Generics.Result

spec :: Spec
spec = do
  describe "Result" $ do
    context ">>" $ do
      it "collects errors" $ do
        (Errors ["foo"] >> Errors ["bar"] :: Result ())
          `shouldBe` Errors ["foo", "bar"]

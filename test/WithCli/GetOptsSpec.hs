{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module WithCli.GetOptsSpec where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec

import           WithCli.FromArguments
import           WithCli.Result
import           Util

spec :: Spec
spec = do
  describe "parseFromArguments" $ do
    it "foo" $ do
      let fa :: FromArguments phase Int
          fa = FromArguments {
            parserDefault = Nothing,
            parserOptions = [],
            parserNonOptions =
              ("type", \ (s : r) -> (, r) <$> Success (const $ Just $ read s)) :
              [],
            parserConvert = \ (Just x) -> return x
          }
      let i = parseFromArguments "program" (unsafeModifiers []) fa ["42"]
      i `shouldBe` Success 42

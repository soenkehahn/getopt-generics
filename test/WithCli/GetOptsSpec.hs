{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module WithCli.GetOptsSpec where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec

import           WithCli.Parser
import           WithCli.Result
import           Util

spec :: Spec
spec = do
  describe "runParser" $ do
    it "foo" $ do
      let fa :: Parser phase Int
          fa = Parser {
            parserDefault = Nothing,
            parserOptions = [],
            parserNonOptions =
              ("type", \ (s : r) -> (, r) <$> Success (const $ Just $ read s)) :
              [],
            parserConvert = \ (Just x) -> return x
          }
      let i = runParser "program" (unsafeModifiers []) fa ["42"]
      i `shouldBe` Success 42

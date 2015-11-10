{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module WithCli.ParserSpec where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec

import           WithCli.Parser
import           WithCli.Result
import           Util

spec :: Spec
spec = do
  describe "runParser" $ do
    it "works" $ do
      let fa :: Parser phase Int
          fa = Parser {
            parserDefault = Nothing,
            parserOptions = [],
            parserNonOptions =
              (NonOptionsParser "type" False (\ (s : r) -> (, r) <$> Success (const $ Just $ read s))) :
              [],
            parserConvert = \ (Just x) -> return x
          }
      let i = runParser "program" (unsafeModifiers []) fa ["42"]
      i `shouldBe` Success 42

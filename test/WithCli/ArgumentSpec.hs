
module WithCli.ArgumentSpec where

import           Data.Proxy
import           Test.Hspec

import           WithCli.Argument

spec :: Spec
spec = do
  describe "Option.Double" $ do
    it "parses doubles" $ do
      parseArgument "1.2" `shouldBe` Just (1.2 :: Double)

    it "renders as NUMBER in help and error output" $ do
      argumentType (Proxy :: Proxy Double) `shouldBe` "NUMBER"

    it "parses doubles that start with a dot" $ do
      parseArgument ".4" `shouldBe` Just (0.4 :: Double)

  describe "Option.Float" $ do
    it "parses floats" $ do
      parseArgument "1.2" `shouldBe` Just (1.2 :: Float)

    it "renders as NUMBER in help and error output" $ do
      argumentType (Proxy :: Proxy Float) `shouldBe` "NUMBER"

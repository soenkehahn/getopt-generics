
module WithCli.PureSpec where

import           Test.Hspec

import           WithCli.Pure

spec :: Spec
spec = do
  describe "withCliPure" $ do
    it "works for no arguments" $ do
      let f :: String
          f = "foo"
      withCliPure "progName" [] [] f `shouldBe` Success "foo"
      withCliPure "progName" [] ["-h"] f `shouldBe` ((OutputAndExit $ unlines $
        "progName [OPTIONS]" :
        "  -h  --help  show help and exit" :
        []) :: Result String)

    it "works for one argument" $ do
      let f :: Int -> String
          f = show
      withCliPure "progName" [] ["42"] f `shouldBe` Success "42"
      withCliPure "progName" [] ["-h"] f `shouldBe` ((OutputAndExit $ unlines $
        "progName [OPTIONS] INTEGER" :
        "  -h  --help  show help and exit" :
        []) :: Result String)

    it "works for two arguments" $ do
      let f :: Int -> String -> (Int, String)
          f n s = (n, s)
      withCliPure "progName" [] ["42", "foo"] f `shouldBe`
        Success (42 :: Int, "foo")

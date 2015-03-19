{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Console.Args.GenericsSpec where

import           Control.Exception
import           Generics.SOP
import qualified GHC.Generics
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           System.Console.Args.Generics

spec :: Spec
spec = do
  describe "withArguments" $ do
    it "parses command line arguments" $ do
      withArgs (words "--bar 4 --baz foo") $ withArguments $ \ options ->
        options `shouldBe` Foo (Just 4) "foo" False

    it "allows optional arguments" $ do
      withArgs (words "--baz foo") $ withArguments $ \ options ->
        options `shouldBe` Foo Nothing "foo" False

    context "invalid arguments" $ do
      it "doesn't execute the action" $ do
        let main = withArgs (words "--invalid") $
              withArguments $ \ (_ :: Foo) ->
                throwIO (ErrorCall "action")
        main `shouldThrow` (== ExitFailure 1)

      it "prints out an error" $ do
        output <- hCapture_ [stderr] $ handle (\ (_ :: SomeException) -> return ()) $
          withArgs (words "--no-such-option") $
            withArguments $ \ (_ :: Foo) -> return ()
        output `shouldContain` "Invalid"
        output `shouldContain` "--no-such-option"


data Foo
  = Foo {
    bar :: Maybe Int,
    baz :: String,
    huhu :: Bool
  }
  deriving (GHC.Generics.Generic, Show, Eq)

instance Generic Foo
instance HasDatatypeInfo Foo

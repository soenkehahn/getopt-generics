{-# LANGUAGE DeriveGeneric #-}

module WithCliSpec where

import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           WithCli

data Foo
  = Foo {
    bar :: Maybe Int,
    baz :: String,
    bool :: Bool
  }
  deriving (Eq, Show, Generic)

instance HasArguments Foo

spec :: Spec
spec = do
  describe "withCli" $ do
    context "no arguments" $ do
      it "executes the operation in case of no command line arguments" $ do
        let main :: IO ()
            main = putStrLn "success"
        (capture_ $ withArgs [] $ withCli main)
          `shouldReturn` "success\n"

      it "produces nice error messages" $ do
        let main :: IO ()
            main = putStrLn "success"
        output <- hCapture_ [stderr] (withArgs ["foo"] (withCli main) `shouldThrow` (== ExitFailure 1))
        output `shouldBe` "unknown argument: foo\n"

    context "1 argument" $ do
      it "parses Ints" $ do
        let main :: Int -> IO ()
            main n = putStrLn ("success: " ++ show n)
        (capture_ $ withArgs ["12"] $ withCli main)
          `shouldReturn` "success: 12\n"

      it "error parsing" $ do
        let main :: Int -> IO ()
            main n = putStrLn ("error: " ++ show n)
        output <- hCapture_ [stderr] (withArgs (words "12 foo") (withCli main)
          `shouldThrow` (== ExitFailure 1))
        output `shouldBe` "unknown argument: foo\n"

      context "record types" $ do
        it "parses command line arguments" $ do
          withArgs (words "--bar 4 --baz foo") $
            withCli $ \ foo -> do
              foo `shouldBe` Foo (Just 4) "foo" False

    context "optional positional arguments with Maybe" $ do
      it "allows optional positional arguments" $ do
        let main :: Maybe Int -> IO ()
            main = print
        (capture_ $ withCli main)
          `shouldReturn` "Nothing\n"
        (capture_ $ withArgs ["23"] $ withCli main)
          `shouldReturn` "Just 23\n"

      it "allows multiple optional positional arguments" $ do
        let main :: Maybe Int -> Maybe String -> IO ()
            main i s = print (i, s)
        (capture_ $ withCli main)
          `shouldReturn` "(Nothing,Nothing)\n"
        (capture_ $ withArgs ["23"] $ withCli main)
          `shouldReturn` "(Just 23,Nothing)\n"
        (capture_ $ withArgs ["23", "foo"] $ withCli main)
          `shouldReturn` "(Just 23,Just \"foo\")\n"

      it "allows optional positional arguments after non-optional arguments" $ do
        let main :: Int -> Maybe String -> IO ()
            main i s = print (i, s)
        (hCapture_ [stderr] $ withCli main `shouldThrow` (== ExitFailure 1))
          `shouldReturn` "missing argument of type INTEGER\n"
        (capture_ $ withArgs ["23"] $ withCli main)
          `shouldReturn` "(23,Nothing)\n"
        (capture_ $ withArgs ["23", "foo"] $ withCli main)
          `shouldReturn` "(23,Just \"foo\")\n"

      it "disallows optional positional arguments before non-optional ones with a proper error message" $ do
        let main :: Maybe Int -> String -> IO ()
            main = error "main"
        hCapture_ [stderr] (withCli main `shouldThrow` (== ExitFailure 1))
          `shouldReturn` "cannot use Maybes for optional arguments before any non-optional arguments\n"

      it "shows optional arguments with nested square brackets in help output" $ do
        let main :: Int -> Maybe String -> Maybe String -> IO ()
            main = error "main"
        output <- capture_ (withArgs ["-h"] (withCli main) `shouldThrow` (== ExitSuccess))
        output `shouldContain` "[OPTIONS] INTEGER [STRING [STRING]]"

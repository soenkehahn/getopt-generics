
module SimpleCLISpec where

import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           SimpleCLI

spec :: Spec
spec = do
  describe "simpleCLI" $ do
    context "no arguments" $ do
      it "executes the operation in case of no command line arguments" $ do
        let main :: IO ()
            main = putStrLn "success"
        (capture_ $ withArgs [] $ simpleCLI main)
          `shouldReturn` "success\n"

      it "produces nice error messages" $ do
        let main :: IO ()
            main = putStrLn "success"
        output <- hCapture_ [stderr] (withArgs ["foo"] (simpleCLI main) `shouldThrow` (== ExitFailure 1))
        output `shouldBe` "unknown argument: foo\n"

    context "1 argument" $ do
      it "parses Ints" $ do
        let main :: Int -> IO ()
            main n = putStrLn ("success: " ++ show n)
        (capture_ $ withArgs ["12"] $ simpleCLI main)
          `shouldReturn` "success: 12\n"

      it "error parsing" $ do
        let main :: Int -> IO ()
            main n = putStrLn ("error: " ++ show n)
        output <- hCapture_ [stderr] (withArgs (words "12 foo") (simpleCLI main) `shouldThrow` (== ExitFailure 1))
        output `shouldBe` "unknown argument: foo\n"

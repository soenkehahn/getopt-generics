
module System.Console.GetOpt.Generics.SimpleSpec where

import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec

import           System.Console.GetOpt.Generics.Simple

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
      it "creates CLIs for unary IO operations" $ do
        let main :: String -> IO ()
            main s = putStrLn ("success: " ++ s)
        (capture_ $ withArgs ["foo"] $ simpleCLI main)
          `shouldReturn` "success: foo\n"

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

      it "offers --help" $ do
        let main :: Int -> IO ()
            main n = putStrLn ("error: " ++ show n)
        output <- capture_ (withArgs ["--help"] (simpleCLI main) `shouldThrow` (== ExitSuccess))
        output `shouldContain` "[OPTIONS] INTEGER"
        output `shouldContain` "-h  --help  show help and exit"

    it "works for IO operations with 2 arguments" $ do
      let main :: Int -> Bool -> IO ()
          main n b = putStrLn ("success: " ++ show (n, b))
      (capture_ $ withArgs (words "42 yes") $ simpleCLI main)
        `shouldReturn` "success: (42,True)\n"

    it "works for IO operations with 3 arguments" $ do
      let main :: String -> Int -> Bool -> IO ()
          main s n b = putStrLn ("success: " ++ show (s, n, b))
      (capture_ $ withArgs (words "foo 42 yes") $ simpleCLI main)
        `shouldReturn` "success: (\"foo\",42,True)\n"

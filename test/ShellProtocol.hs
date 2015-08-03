{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module ShellProtocol (testShellProtocol) where

import           Control.Exception
import           Control.Monad
import           Data.List
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec

testShellProtocol :: IO () -> String -> IO ()
testShellProtocol program shellProtocol = do
  let protocol = parseProtocol shellProtocol
  testProtocol program protocol

data Protocol
  = Protocol {
    _args :: [String],
    _expected :: [String]
  }
  deriving (Show)

parseProtocol :: String -> [Protocol]
parseProtocol = inner . lines
  where
    inner :: [String] -> [Protocol]
    inner [] = []
    inner ((words -> "$" : "program" : args) : rest) =
      let (expected, next) = span (not . ("$ " `isPrefixOf`)) rest in
      Protocol args expected : inner next
    inner lines = error ("parseProtocol: cannot parse: " ++ show lines)

testProtocol :: IO () -> [Protocol] -> IO ()
testProtocol program protocol = do
  forM_ protocol $ \ (Protocol args expected) -> do
    output <- hCapture_ [stdout, stderr] $
      handle (\ (e :: ExitCode) -> printExitCode e) $
      withArgs args $
      withProgName "program" $
      program
    output `shouldBe` unlines expected

printExitCode :: ExitCode -> IO ()
printExitCode e = case e of
  ExitFailure n -> hPutStrLn stderr ("# exit-code " ++ show n)
  ExitSuccess -> return ()

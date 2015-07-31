
module BashProtocol (testBashProtocol) where

import           Control.Exception
import           Control.Monad
import           Data.List
import           System.Environment.Compat
import           System.FilePath
import           System.IO
import           System.IO.Silently
import           System.Process
import           Test.Hspec

testBashProtocol :: FilePath -> String -> IO ()
testBashProtocol program bashProtocol = do
  assert (takeFileName program == "program") (return ())
  let protocol = parseProtocol bashProtocol
  testProtocol (takeDirectory program) protocol

data Protocol
  = Protocol {
    _command :: String,
    _expected :: [String]
  }
  deriving (Show)

parseProtocol :: String -> [Protocol]
parseProtocol = inner . lines
  where
    inner :: [String] -> [Protocol]
    inner [] = []
    inner (('$' : ' ' : command) : rest) =
      let (expected, next) = span (not . ("$ " `isPrefixOf`)) rest in
      Protocol command expected : inner next
    inner lines = error ("parseProtocol: cannot parse: " ++ show lines)

testProtocol :: FilePath -> [Protocol] -> IO ()
testProtocol programDir protocol = do
  path <- getEnv "PATH"
  setEnv "PATH" (programDir ++ ":" ++ path)
  forM_ protocol $ \ (Protocol command expected) -> do
    hPutStrLn stderr ("running: " ++ command)
    output <- hCapture_ [stdout, stderr] $ system command
    output `shouldBe` unlines expected

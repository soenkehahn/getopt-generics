
module ExamplesSpec where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           System.FilePath
import           Test.Hspec

import           ShellProtocol
import qualified CustomOption
import qualified RecordType
import qualified Simple
import qualified SimpleRecord
import qualified Test01
import qualified Test02
import qualified Test03
import qualified Test04

examples :: [(IO (), String)]
examples =
  (Test01.main, "Test01") :
  (Test02.main, "Test02") :
  (Test03.main, "Test03") :
  (Test04.main, "Test04") :

  (Simple.main, "Simple") :
  (SimpleRecord.main, "SimpleRecord") :
  (RecordType.main, "RecordType") :
  (SimpleRecord.main, "SimpleRecord") :
  (CustomOption.main, "CustomOption") :
  []

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "shell protocols" $ do
    forM_ examples $ \ (program, name) ->
      it name $ do
        test program name

test :: IO () -> String -> IO ()
test program name = do
  protocol <- readFile ("docs" </> name <.> "shell-protocol")
  testShellProtocol program protocol

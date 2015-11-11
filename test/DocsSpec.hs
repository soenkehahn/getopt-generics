{-# LANGUAGE CPP #-}

module DocsSpec where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           System.FilePath
import           Test.Hspec

import           ShellProtocol

import qualified Test01
import qualified CustomOption
import qualified Simple
#if MIN_VERSION_base(4,8,0)
import qualified Test02
import qualified Test03
import qualified Test04
import qualified RecordType
import qualified CustomOptionRecord
#endif

examples :: [(IO (), String)]
examples =
  (Test01.main, "Test01") :
  (CustomOption.main, "CustomOption") :
  (Simple.main, "Simple") :
#if MIN_VERSION_base(4,8,0)
  (Test02.main, "Test02") :
  (Test03.main, "Test03") :
  (Test04.main, "Test04") :
  (RecordType.main, "RecordType") :
  (CustomOptionRecord.main, "CustomOptionRecord") :
#endif
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

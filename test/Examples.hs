
import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           Data.List
import           System.Directory
import           System.Exit.Compat
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Test.Hspec

import           BashProtocol

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  files <- runIO $
    map ("docs" </>) <$>
    filter ((".hs" ==) . takeExtension) <$>
    sort <$>
    getDirectoryContents "docs"

  forM_ files $ \ file -> do
    describe "docs directory ghc" $ do
      it ("compile and run " ++ file) $ do
        withSystemTempDirectory "getopt-generics-test" $ \ tempDir -> do
          compile tempDir file >>= execute file

compile :: FilePath -> FilePath -> IO FilePath
compile tempDir file = do
  let program = tempDir </> "program"
  callCommandCompat $ "cabal exec -- ghc " ++ file ++
    " -outputdir " ++ (tempDir </> "build") ++
    " -o " ++ program ++
    " -isrc -O0" ++
    " -Wall -Werror -fno-warn-unused-binds -fno-warn-name-shadowing"
  return program

execute :: FilePath -> FilePath -> IO ()
execute haskellFile program = do
  let bashProtocolFile = dropExtension haskellFile <.> "bash-protocol"
  exists <- doesFileExist bashProtocolFile
  if not exists
    then callCommandCompat $ program ++ " --help"
    else do
      testBashProtocol program =<< readFile bashProtocolFile

callCommandCompat :: String -> IO ()
callCommandCompat command = do
  exitCode <- system command
  case exitCode of
    ExitSuccess -> return ()
    failure@ExitFailure{} -> exitWith failure

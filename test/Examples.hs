
import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           System.Directory
import           System.Exit.Compat
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Test.Hspec

main :: IO ()
main = hspec spec

cache :: FilePath
cache = "test-build-cache"

spec :: Spec
spec = do
  files <- runIO $
    map ("docs" </>) <$>
    filter ((".hs" ==) . takeExtension) <$>
    getDirectoryContents "docs"

  forM_ files $ \ file -> do
    describe "docs directory ghc" $ do
      it ("compile and run " ++ file) $ do
        withSystemTempDirectory "getopt-generics-test" $ \ tempDir -> do
          createDirectoryIfMissing True cache
          compile tempDir file >>= execute

compile :: FilePath -> FilePath -> IO FilePath
compile tempDir file = do
  let program = tempDir </> "program"
  callCommandCompat $ "cabal exec -- ghc " ++ file ++
    " -outputdir " ++ cache ++
    " -o " ++ program ++
    " -isrc -O0" ++
    " -Wall -Werror -fno-warn-unused-binds -fno-warn-name-shadowing"
  return program

execute :: FilePath -> IO ()
execute program = callCommandCompat $ program ++ " --help"

callCommandCompat :: String -> IO ()
callCommandCompat command = do
  (_, _, _, handle) <- createProcess $ shell command
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess -> return ()
    failure@ExitFailure{} -> exitWith failure

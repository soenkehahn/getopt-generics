
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

spec :: Spec
spec = do
  files <- runIO $
    map ("docs" </>) <$>
    filter ((".hs" ==) . takeExtension) <$>
    getDirectoryContents "docs"

  forM_ files $ \ file -> do
    describe "docs directory ghc" $ do
      it file $ do
        withSystemTempDirectory "getopt-generics-test" $ \ tempDir -> do
          let cache = "test-build-cache"
          createDirectoryIfMissing True cache
          callCommandCompat $ "cabal exec -- ghc " ++ file ++
            " -outputdir " ++ cache ++
            " -o " ++ (tempDir </> "program") ++
            " -isrc -O0" ++
            " -Wall -Werror -fno-warn-unused-binds -fno-warn-name-shadowing"
          callCommandCompat $ (tempDir </> "program") ++ " --help"

callCommandCompat :: String -> IO ()
callCommandCompat command = do
  (_, _, _, handle) <- createProcess $ shell command
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess -> return ()
    failure@ExitFailure{} -> exitWith failure

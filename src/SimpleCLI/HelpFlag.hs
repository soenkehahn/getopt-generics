
module SimpleCLI.HelpFlag where

import           System.Console.GetOpt

helpOption :: a -> OptDescr a
helpOption helpFlag = Option ['h'] ["help"] (NoArg helpFlag) "show help and exit"

usage :: String -> [String] -> [OptDescr ()] -> String
usage progName fields options = usageInfo header options
  where
    header :: String
    header = unwords $
      progName :
      "[OPTIONS]" :
       fields ++
      []

versionOption :: a -> OptDescr a
versionOption versionFlag =
  Option ['v'] ["version"] (NoArg versionFlag) "show version and exit"

-- fixme: disentangle
-- fixme: rename module

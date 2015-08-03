{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module System.Console.GetOpt.Generics (
  -- * IO API
  simpleCLI,
  SimpleCLI,
  HasOptions,
  getArguments,
  modifiedGetArguments,
  -- * Pure API
  parseArguments,
  Result(..),
  -- * Customizing the CLI
  Modifier(..),
  deriveShortOptions,
  -- * Available Field Types
  SimpleCLI.Option(argumentType, parseArgument),
  -- * Re-exports from "Generics.SOP"
  Generics.SOP.Generic,
  HasDatatypeInfo,
  Code,
  All2,
  SingI,
  Proxy(..),
 ) where

import           Generics.SOP
import           System.Environment

import           SimpleCLI
import           SimpleCLI.FromArguments
import           SimpleCLI.HasOptions
import           SimpleCLI.Result
import           System.Console.GetOpt.Generics.Modifier

-- | Parses command line arguments (gotten from 'withArgs') and returns the
--   parsed value. This function should be enough for simple use-cases.
--
--   Throws the same exceptions as 'simpleCLI'.
--
-- Here's an example:

-- ### Start "docs/RecordType.hs" Haddock ###

-- |
-- >  {-# LANGUAGE DeriveGeneric #-}
-- >
-- >  module RecordType where
-- >
-- >  import qualified GHC.Generics
-- >  import           System.Console.GetOpt.Generics
-- >
-- >  -- All you have to do is to define a type and derive some instances:
-- >
-- >  data Options
-- >    = Options {
-- >      port :: Int,
-- >      daemonize :: Bool,
-- >      config :: Maybe FilePath
-- >    }
-- >    deriving (Show, GHC.Generics.Generic)
-- >
-- >  instance Generic Options
-- >  instance HasDatatypeInfo Options
-- >
-- >  -- Then you can use `getArguments` to create a command-line argument parser:
-- >
-- >  main :: IO ()
-- >  main = do
-- >    options <- getArguments
-- >    print (options :: Options)

-- ### End ###

-- | And this is how the above program behaves:

-- ### Start "docs/RecordType.shell-protocol" Haddock ###

-- |
-- >  $ program --port 8080 --config some/path
-- >  Options {port = 8080, daemonize = False, config = Just "some/path"}
-- >  $ program  --port 8080 --daemonize
-- >  Options {port = 8080, daemonize = True, config = Nothing}
-- >  $ program --port foo
-- >  cannot parse as NUMBER: foo
-- >  # exit-code 1
-- >  $ program
-- >  missing option: --port=NUMBER
-- >  # exit-code 1
-- >  $ program --help
-- >  program [OPTIONS]
-- >        --port=NUMBER
-- >        --daemonize
-- >        --config=STRING (optional)
-- >    -h  --help                      show help and exit

-- ### End ###

getArguments :: forall a . (Generic a, HasDatatypeInfo a, All2 HasOptions (Code a)) =>
  IO a
getArguments = modifiedGetArguments []

-- | Like 'getArguments` but allows you to pass in 'Modifier's.
modifiedGetArguments :: forall a . (Generic a, HasDatatypeInfo a, All2 HasOptions (Code a)) =>
  [Modifier] -> IO a
modifiedGetArguments modifiers = do
  args <- getArgs
  progName <- getProgName
  handleResult $ parseArguments progName modifiers args

-- | Pure variant of 'modifiedGetArguments'.
--
--   Does not throw any exceptions.
parseArguments :: forall a . (Generic a, HasDatatypeInfo a, All2 HasOptions (Code a)) =>
     String -- ^ Name of the program (e.g. from 'getProgName').
  -> [Modifier] -- ^ List of 'Modifier's to manually tweak the command line interface.
  -> [String] -- ^ List of command line arguments to parse (e.g. from 'getArgs').
  -> Result a
parseArguments progName mods args = do
  modifiers <- mkModifiers mods
  fromArguments <- fromArgumentsGeneric modifiers
  parseFromArguments progName modifiers
    (normalizeFromArguments (applyModifiers modifiers fromArguments)) args

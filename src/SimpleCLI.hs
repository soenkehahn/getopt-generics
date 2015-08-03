{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module SimpleCLI (
  simpleCLI,
  SimpleCLI,
  Option(argumentType, parseArgument),
  HasOptions(fromArguments),
  fromArgumentsOption,

  SOP.Generic,
  SOP.HasDatatypeInfo,
  Typeable,
  Proxy(..),
  ) where

import           Data.Typeable
import qualified Generics.SOP as SOP
import           System.Environment

import           SimpleCLI.FromArguments
import           SimpleCLI.HasOptions
import           SimpleCLI.Option
import           SimpleCLI.Result
import           System.Console.GetOpt.Generics.Modifier

-- | 'simpleCLI' converts an IO operation into a program with a proper CLI.
--   Retrieves command line arguments through 'withArgs'.
--   @main@ (the given IO operation) can have arbitrarily many parameters
--   provided all parameters have an instance for 'Option'.
--
--   May throw the following exceptions:
--
--   - @'ExitFailure' 1@ in case of invalid options. Error messages are written
--     to @stderr@.
--   - @'ExitSuccess'@ in case @--help@ is given. (@'ExitSuccess'@ behaves like
--     a normal exception, except that -- if uncaught -- the process will exit
--     with exit-code @0@.) Help output is written to @stdout@.
--
--   Example:

-- ### Start "docs/Simple.hs" Haddock ###

-- |
-- >  module Simple where
-- >
-- >  import SimpleCLI
-- >
-- >  main :: IO ()
-- >  main = simpleCLI myMain
-- >
-- >  myMain :: String -> Int -> Bool -> IO ()
-- >  myMain s i b = print (s, i, b)

-- ### End ###

-- | Using the above program in a shell:

-- ### Start "docs/Simple.shell-protocol" Haddock ###

-- |
-- >  $ program foo 42 true
-- >  ("foo",42,True)
-- >  $ program --help
-- >  program [OPTIONS] STRING NUMBER BOOL
-- >    -h  --help  show help and exit
-- >  $ program foo 42 bar
-- >  cannot parse as BOOL: bar
-- >  # exit-code 1
-- >  $ program
-- >  missing argument of type STRING
-- >  missing argument of type NUMBER
-- >  missing argument of type BOOL
-- >  # exit-code 1
-- >  $ program foo 42 yes bar
-- >  unknown argument: bar
-- >  # exit-code 1

-- ### End ###

simpleCLI :: forall main . SimpleCLI main => main -> IO ()
simpleCLI = modifiedSimpleCLI []

modifiedSimpleCLI :: forall main . SimpleCLI main => [Modifier] -> main -> IO ()
modifiedSimpleCLI mods main = do
  args <- getArgs
  modifiers <- handleResult (mkModifiers mods)
  run modifiers (return $ emptyFromArguments ()) (\ () -> main) args

class SimpleCLI main where
  -- fixme: hide 'run'
  run :: Modifiers -> Result (FromArguments Unnormalized a) -> (a -> main) -> [String] -> IO ()

instance SimpleCLI (IO ()) where
  run modifiers fromArguments mkMain args = do
    progName <- getProgName
    fa <- handleResult fromArguments
    a <- handleResult $ parseFromArguments progName modifiers
      (normalizeFromArguments (applyModifiers modifiers fa)) args
    mkMain a

instance (HasOptions a, SimpleCLI rest) => SimpleCLI (a -> rest) where
  run modifiers fa mkMain args =
    run modifiers (combine fa (fromArguments modifiers Nothing)) (\ (a, r) -> mkMain a r) args

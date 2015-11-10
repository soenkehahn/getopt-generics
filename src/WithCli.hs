{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module WithCli (
  withCli,
  WithCli(),
  HasArguments(argumentsParser),
  atomicArgumentsParser,
  Argument(argumentType, parseArgument),
  -- * Modifiers
  withCliModified,
  Modifier(..),
  -- * Useful Re-exports
  GHC.Generic,
  Typeable,
  Proxy(..),
  ) where

import           Data.Proxy
import           Data.Typeable
import qualified GHC.Generics as GHC
import           System.Environment

import           WithCli.Argument
import           WithCli.HasArguments
import           WithCli.Modifier
import           WithCli.Parser
import qualified WithCli.Pure.Internal
import           WithCli.Result

-- | 'withCli' converts an IO operation into a program with a proper CLI.
--   Retrieves command line arguments through 'withArgs'.
--   @main@ (the given IO operation) can have arbitrarily many parameters
--   provided all parameters have instances for 'HasArguments'.
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

-- ### Start "docs/Simple.hs" "module Simple where\n\n" Haddock ###

-- |
-- >  import WithCli
-- >
-- >  main :: IO ()
-- >  main = withCli run
-- >
-- >  run :: String -> Int -> Bool -> IO ()
-- >  run s i b = print (s, i, b)

-- ### End ###

-- | Using the above program in a shell:

-- ### Start "docs/Simple.shell-protocol" "" Haddock ###

-- |
-- >  $ program foo 42 true
-- >  ("foo",42,True)
-- >  $ program --help
-- >  program [OPTIONS] STRING INTEGER BOOL
-- >    -h  --help  show help and exit
-- >  $ program foo 42 bar
-- >  cannot parse as BOOL: bar
-- >  # exit-code 1
-- >  $ program
-- >  missing argument of type STRING
-- >  missing argument of type INTEGER
-- >  missing argument of type BOOL
-- >  # exit-code 1
-- >  $ program foo 42 yes bar
-- >  unknown argument: bar
-- >  # exit-code 1

-- ### End ###

withCli :: WithCli main => main -> IO ()
withCli = withCliModified []

-- | This is a variant of 'withCli' that allows to tweak the generated
--   command line interface by providing a list of 'Modifier's.
withCliModified :: WithCli main => [Modifier] -> main -> IO ()
withCliModified mods main = do
  args <- getArgs
  modifiers <- handleResult (mkModifiers mods)
  run modifiers (return $ emptyParser ()) (\ () -> main) args

-- | Everything that can be used as a @main@ function with 'withCli' needs to
--   have an instance of 'WithCli'. You shouldn't need to implement your own
--   instances.
class WithCli main where
  run :: Modifiers -> Result (Parser Unnormalized a) -> (a -> main) -> [String] -> IO ()

instance WithCli (IO ()) where
  run modifiers mkParser mkMain args = do
    progName <- getProgName
    let result = WithCli.Pure.Internal.run
          progName modifiers mkParser mkMain args
    action <- handleResult result
    action

instance (HasArguments a, WithCli rest) => WithCli (a -> rest) where
  run modifiers fa mkMain args =
    run modifiers (combine fa (argumentsParser modifiers Nothing)) (\ (a, r) -> mkMain a r) args

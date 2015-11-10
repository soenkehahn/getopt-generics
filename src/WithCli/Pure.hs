
module WithCli.Pure (
  withCliPure,
  WithCliPure(),
  Result(..),
  handleResult,
  HasArguments(argumentsParser),
  atomicArgumentsParser,
  Argument(argumentType, parseArgument),
  -- * Modifiers
  Modifier(..),
  -- * Useful Re-exports
  GHC.Generic,
  Typeable,
  Proxy(..),
) where

import           Data.Proxy
import           Data.Typeable
import           GHC.Generics as GHC

import           WithCli.Argument
import           WithCli.HasArguments
import           WithCli.Modifier
import           WithCli.Parser
import           WithCli.Pure.Internal
import           WithCli.Result

-- | Pure variant of 'WithCli.withCliModified'.
withCliPure :: WithCliPure function a => String -> [Modifier] -> [String]
  -> function
    -- ^ The @function@ parameter can be a
    -- function with arbitrary many parameters as long as they have an instance
    -- for 'HasArguments'. You can choose the return type of @function@ freely,
    -- 'withCliPure' will return it wrapped in 'Result' to account for parse
    -- errors, etc. (see 'Result').
  -> Result a
withCliPure progName modifiers args function = sanitize $ do
  modifiers <- mkModifiers modifiers
  run progName modifiers (return $ emptyParser ()) (\ () -> function) args

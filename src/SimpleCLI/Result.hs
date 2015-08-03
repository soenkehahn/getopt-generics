{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ViewPatterns #-}

module SimpleCLI.Result (
  Result(..),
  handleResult,
  sanitize,
 ) where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
import           Data.List.Compat
import           System.Exit
import           System.IO

-- fixme: hide non-smart constructors

-- | Type to wrap results from the pure parsing functions.
data Result a
  = Success a
    -- ^ The CLI was used correctly and a value of type @a@ was
    --   successfully constructed.
  | Errors [String]
    -- ^ The CLI was used incorrectly. The 'Result' contains a list of error
    --   messages.
    --
    --   It can also happen that the data type you're trying to use isn't
    --   supported. See the
    --   <https://github.com/zalora/getopt-generics#getopt-generics README> for
    --   details.
  | OutputAndExit String
    -- ^ The CLI was used with @--help@. The 'Result' contains the help message.
  deriving (Show, Eq, Ord, Functor)

instance Applicative Result where
  pure = Success
  OutputAndExit message <*> _ = OutputAndExit message
  _ <*> OutputAndExit message = OutputAndExit message
  Success f <*> Success x = Success (f x)
  Errors a <*> Errors b = Errors (a ++ b)
  Errors errs <*> Success _ = Errors errs
  Success _ <*> Errors errs = Errors errs

instance Monad Result where
  return = pure
  Success a >>= b = b a
  Errors errs >>= _ = Errors errs
  OutputAndExit message >>= _ = OutputAndExit message

  (>>) = (*>)

handleResult :: Result a -> IO a
handleResult result = case result of
  Success a -> return a
  OutputAndExit message -> do
    putStr $ sanitize message
    exitWith ExitSuccess
  Errors errs -> do
    hPutStr stderr $ sanitize $ intercalate "\n" errs
    exitWith $ ExitFailure 1

sanitize :: String -> String
sanitize =
  lines >>>
  map stripTrailingSpaces >>>
  filter (not . null) >>>
  map (++ "\n") >>>
  concat

stripTrailingSpaces :: String -> String
stripTrailingSpaces =
  reverse . inner . dropWhile (`elem` [' ', '\n']) . reverse
  where
    inner s = case s of
      ('\n' : ' ' : r) -> inner ('\n' : r)
      (a : r) -> a : inner r
      [] -> []

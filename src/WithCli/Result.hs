{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module WithCli.Result (
  Result(..),
  (|>),
  handleResult,
  sanitizeMessage,
  sanitize,
 ) where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
import           System.Exit
import           System.IO

-- | Type to wrap results from 'WithCli.Pure.withCliPure'.
data Result a
  = Success a
    -- ^ The CLI was used correctly and a value of type @a@ was
    --   successfully constructed.
  | Errors String
    -- ^ The CLI was used incorrectly. The 'Result' contains error messages.
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
  Errors a <*> Errors b = Errors (a ++ "\n" ++ b)
  Errors err <*> Success _ = Errors err
  Success _ <*> Errors err = Errors err

(|>) :: Result a -> Result b -> Result b
a |> b = a >>= const b

instance Monad Result where
  return = pure
  Success a >>= b = b a
  Errors errs >>= _ = Errors errs
  OutputAndExit message >>= _ = OutputAndExit message

  (>>) = (*>)

-- | Handles an input of type @'Result' a@:
--
-- - On @'Success' a@ it returns the value @a@.
-- - On @'OutputAndExit' message@ it writes the message to 'stdout' and throws
--   'ExitSuccess'.
-- - On @'Errors' errs@ it writes the error messages to 'stderr' and throws
--   @'ExitFailure' 1@.
--
-- This is used by 'WithCli.withCli' to handle parse results.
handleResult :: Result a -> IO a
handleResult result = case sanitize result of
  Success a -> return a
  OutputAndExit message -> do
    putStr message
    exitWith ExitSuccess
  Errors err -> do
    hPutStr stderr err
    exitWith $ ExitFailure 1

sanitize :: Result a -> Result a
sanitize = \ case
  Success a -> Success a
  OutputAndExit message -> OutputAndExit $ sanitizeMessage message
  Errors messages -> Errors $ sanitizeMessage messages

sanitizeMessage :: String -> String
sanitizeMessage =
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

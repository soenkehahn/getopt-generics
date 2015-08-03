{-# LANGUAGE DeriveFunctor #-}

module SimpleCLI.Result (
  Result(..),
  errors,
  outputAndExit,
  handleResult,
 ) where

import           Prelude ()
import           Prelude.Compat

import           Data.List
import           System.Exit
import           System.IO

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

errors :: [String] -> Result a
errors = Errors . map removeTrailingNewline

outputAndExit :: String -> Result a
outputAndExit = OutputAndExit . stripTrailingSpaces

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
    putStr message
    exitWith ExitSuccess
  Errors errs -> do
    mapM_ (hPutStr stderr . addNewlineIfMissing) errs
    exitWith $ ExitFailure 1

addNewlineIfMissing :: String -> String
addNewlineIfMissing s
  | "\n" `isSuffixOf` s = s
  | otherwise = s ++ "\n"

removeTrailingNewline :: String -> String
removeTrailingNewline s
  | "\n" `isSuffixOf` s = init s
  | otherwise = s

stripTrailingSpaces :: String -> String
stripTrailingSpaces = reverse . inner . dropWhile (== ' ') . reverse
  where
    inner s = case s of
      ('\n' : ' ' : r) -> inner ('\n' : r)
      (a : r) -> a : inner r
      [] -> []

{-# LANGUAGE DeriveFunctor #-}

module System.Console.GetOpt.Generics.Result where

import           Control.Applicative

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

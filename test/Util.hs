{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Util where

import           Prelude ()
import           Prelude.Compat

import           WithCli.Modifier
import           WithCli.Pure

parse :: (HasArguments a) => String -> Result a
parse = modsParse []

data Wrapped a
  = Wrap {
    unwrap :: a
  }

modsParse :: forall a . (HasArguments a) => [Modifier] -> String -> Result a
modsParse modifiers args =
  unwrap <$>
  withCliPure "prog-name" modifiers (words args) (Wrap :: a -> Wrapped a)

unsafeModifiers :: [Modifier] -> Modifiers
unsafeModifiers mods = case mkModifiers mods of
  Success x -> x
  Errors errs -> error ("unsafeModifiers: " ++ show errs)
  OutputAndExit msg -> error ("unsafeModifiers: " ++ show msg)

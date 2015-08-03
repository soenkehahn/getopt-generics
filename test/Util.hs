{-# LANGUAGE ConstraintKinds #-}

module Util where

import           System.Console.GetOpt.Generics
import           System.Console.GetOpt.Generics.Modifier

parse :: (Generic a, HasDatatypeInfo a, All2 HasOptions (Code a)) =>
  String -> Result a
parse = modsParse []

modsParse :: (Generic a, HasDatatypeInfo a, All2 HasOptions (Code a)) =>
  [Modifier] -> String -> Result a
modsParse modifiers = parseArguments "prog-name" modifiers . words

unsafeModifiers :: [Modifier] -> Modifiers
unsafeModifiers mods = case mkModifiers mods of
  Success x -> x
  Errors errs -> error ("unsafeModifiers: " ++ show errs)
  OutputAndExit msg -> error ("unsafeModifiers: " ++ show msg)

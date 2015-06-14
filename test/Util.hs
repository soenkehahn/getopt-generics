{-# LANGUAGE ConstraintKinds #-}

module Util where

import           System.Console.GetOpt.Generics

parse :: (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  String -> Result a
parse = modsParse []

modsParse :: (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  [Modifier] -> String -> Result a
modsParse modifiers = parseArguments "prog-name" modifiers . words

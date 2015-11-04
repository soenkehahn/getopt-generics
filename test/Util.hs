{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Util where

import qualified GHC.Generics as GHC
import           Generics.SOP.GGP

import           System.Console.GetOpt.Generics
import           System.Console.GetOpt.Generics.Modifier

parse :: (GHC.Generic a, GTo a, GDatatypeInfo a, All2 HasArguments (GCode a)) =>
  String -> Result a
parse = modsParse []

modsParse :: (GHC.Generic a, GTo a, GDatatypeInfo a, All2 HasArguments (GCode a)) =>
  [Modifier] -> String -> Result a
modsParse modifiers = parseArguments "prog-name" modifiers . words

unsafeModifiers :: [Modifier] -> Modifiers
unsafeModifiers mods = case mkModifiers mods of
  Success x -> x
  Errors errs -> error ("unsafeModifiers: " ++ show errs)
  OutputAndExit msg -> error ("unsafeModifiers: " ++ show msg)

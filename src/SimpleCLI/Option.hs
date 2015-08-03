{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}

module SimpleCLI.Option where

import           Data.Orphans ()
import           Prelude ()
import           Prelude.Compat

import           Data.Proxy
import           Text.Read

import           System.Console.GetOpt.Generics.GetArguments (readFloat)

-- fixme: better names for HasOptions and Options

class Option a where
  argumentType :: Proxy a -> String
  parseArgument :: String -> Maybe a

instance Option String where
  argumentType Proxy = "STRING"
  parseArgument = Just

instance Option Int where
  argumentType _ = "INTEGER"
  parseArgument = readMaybe

instance Option Integer where
  argumentType _ = "INTEGER"
  parseArgument = readMaybe

instance Option Float where
  argumentType _ = "NUMBER"
  parseArgument = readFloat

instance Option Double where
  argumentType _ = "NUMBER"
  parseArgument = readFloat

-- fixme: CustomOptionsExample

-- todo: clean up old modules

-- todo: figure out modifiers

-- todo: clean up old modules 2


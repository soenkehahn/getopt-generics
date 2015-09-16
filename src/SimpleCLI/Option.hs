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

import           Data.List
import           Data.Proxy
import           Text.Read

-- fixme: better names for HasOptions and Options

-- | 'Option' is a typeclass for things that can be parsed as atomic values from
--   single command line arguments, e.g. strings (and filenames) and numbers.
--
--   Occasionally you might want to declare your own instance for additional
--   type safety and for providing a more informative command argument type.
--   Here's an example:

-- ### Start "docs/CustomOption.hs" "module CustomOption where\n\n" Haddock ###

-- |
-- >  {-# LANGUAGE DeriveDataTypeable #-}
-- >
-- >  import SimpleCLI
-- >
-- >  data File = File FilePath
-- >    deriving (Show, Typeable)
-- >
-- >  instance Option File where
-- >    argumentType Proxy = "custom-file-type"
-- >    parseArgument f = Just (File f)
-- >
-- >  instance HasOptions File where
-- >    fromArguments = fromArgumentsOption
-- >
-- >  main :: IO ()
-- >  main = simpleCLI $ \ file -> do
-- >    print (file :: File)

-- ### End ###

-- | And this is how the above program behaves:

-- ### Start "docs/CustomOption.shell-protocol" "" Haddock ###

-- |
-- >  $ program --help
-- >  program [OPTIONS] custom-file-type
-- >    -h  --help  show help and exit
-- >  $ program some/file
-- >  File "some/file"

-- ### End ###

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

readFloat :: (RealFloat n, Read n) => String -> Maybe n
readFloat s = case readMaybe s of
  Just n -> Just n
  Nothing
    | "." `isPrefixOf` s -> readMaybe ("0" ++ s)
    | otherwise -> Nothing

-- fixme: CustomOptionsExample

-- todo: clean up old modules

-- todo: figure out modifiers

-- todo: clean up old modules 2

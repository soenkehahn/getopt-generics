{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}

module WithCli.Argument where

import           Data.Orphans ()
import           Prelude ()
import           Prelude.Compat

import           Data.List
import           Data.Proxy
import           Text.Read

-- | 'Argument' is a typeclass for things that can be parsed as atomic values from
--   single command line arguments, e.g. strings (and filenames) and numbers.
--
--   Occasionally you might want to declare your own instance for additional
--   type safety and for providing a more informative command argument type.
--   Here's an example:

-- ### Start "docs/CustomOption.hs" "module CustomOption where\n\n" Haddock ###

-- |
-- >  {-# LANGUAGE DeriveDataTypeable #-}
-- >
-- >  import WithCli
-- >
-- >  data File = File FilePath
-- >    deriving (Show, Typeable)
-- >
-- >  instance Argument File where
-- >    argumentType Proxy = "custom-file-type"
-- >    parseArgument f = Just (File f)
-- >
-- >  instance HasArguments File where
-- >    argumentsParser = atomicArgumentsParser
-- >
-- >  main :: IO ()
-- >  main = withCli run
-- >
-- >  run :: File -> IO ()
-- >  run = print

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

class Argument a where
  argumentType :: Proxy a -> String
  parseArgument :: String -> Maybe a

instance Argument String where
  argumentType Proxy = "STRING"
  parseArgument = Just

instance Argument Int where
  argumentType _ = "INTEGER"
  parseArgument = readMaybe

instance Argument Integer where
  argumentType _ = "INTEGER"
  parseArgument = readMaybe

instance Argument Float where
  argumentType _ = "NUMBER"
  parseArgument = readFloat

instance Argument Double where
  argumentType _ = "NUMBER"
  parseArgument = readFloat

readFloat :: (RealFloat n, Read n) => String -> Maybe n
readFloat s = case readMaybe s of
  Just n -> Just n
  Nothing
    | "." `isPrefixOf` s -> readMaybe ("0" ++ s)
    | otherwise -> Nothing

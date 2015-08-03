{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module CustomOptionsExample where

import qualified GHC.Generics
import           SimpleCLI

data File = File FilePath
  deriving (Show, Typeable)

instance Option File where
  argumentType Proxy = "custom-file-type"
  parseArgument f = Just (File f)

data Options
  = Options {
    file :: File
  }
  deriving (Show, GHC.Generics.Generic)

-- fixme: less needed?

instance Generic Options
instance HasDatatypeInfo Options
instance HasOptions Options
instance HasOptions File where
  fromArguments = fromArgumentsOption

main :: IO ()
main = simpleCLI $ \ file -> do
  print (file :: Options)

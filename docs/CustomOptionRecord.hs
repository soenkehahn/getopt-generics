{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module CustomOptionRecord where

import qualified GHC.Generics
import           WithCli

data File = File FilePath
  deriving (Show, Typeable)

instance Argument File where
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
instance HasArguments Options
instance HasArguments File where
  argumentsParser = atomicArgumentParser

main :: IO ()
main = withCli run

run :: Options -> IO ()
run = print

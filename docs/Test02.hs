{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Test02 where

import qualified GHC.Generics
import           WithCli

data Options
  = Options {
    port :: Int,
    daemonize :: Bool,
    config :: Maybe FilePath,
    args :: [String]
  }
  deriving (Show, GHC.Generics.Generic)

instance Generic Options
instance HasDatatypeInfo Options
instance HasArguments Options

main :: IO ()
main = withCli $ \ options -> do
  print (options :: Options)

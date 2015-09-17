{-# LANGUAGE DeriveGeneric #-}

module SimpleRecord where

import qualified GHC.Generics
import           System.Console.GetOpt.Generics

data Options
  = Options {
    port :: Int,
    daemonize :: Bool,
    config :: Maybe FilePath
  }
  deriving (Show, GHC.Generics.Generic)

instance Generic Options
instance HasDatatypeInfo Options
instance HasArguments Options

main :: IO ()
main = withCli run

run :: Options -> IO ()
run = print

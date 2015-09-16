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
instance HasOptions Options

main :: IO ()
main = simpleCLI $ \ options -> do
  print (options :: Options)

  -- todo: use myMain functions in docs?

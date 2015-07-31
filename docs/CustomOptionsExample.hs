{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import           Data.Typeable
import qualified GHC.Generics
import           System.Console.GetOpt.Generics

data File = File FilePath
  deriving (Show, Typeable)

instance Option File where
  argumentType Proxy = "file"
  parseArgument f = Just (File f)

data FileOptions
  = FileOptions {
    file :: File
  }
  deriving (Show, GHC.Generics.Generic)

instance System.Console.GetOpt.Generics.Generic FileOptions
instance HasDatatypeInfo FileOptions

main :: IO ()
main = do
  options <- getArguments
  print (options :: FileOptions)

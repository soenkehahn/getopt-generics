
module System.Console.GetOpt.Generics (
  -- * IO API
  simpleCLI,
  SimpleCLI,
  ArgumentTypes,
  getArguments,
  modifiedGetArguments,
  -- * Pure API
  parseArguments,
  Result(..),
  -- * Customizing the CLI
  Modifier(..),
  deriveShortOptions,
  -- * Available Field Types
  Option(argumentType, parseArgument),
  -- * Re-exports from "Generics.SOP"
  Generic,
  HasDatatypeInfo,
  Code,
  All2,
  SingI,
  Proxy(..),
 ) where

import           Generics.SOP

import           System.Console.GetOpt.Generics.GetArguments
import           System.Console.GetOpt.Generics.Modifier
import           System.Console.GetOpt.Generics.Result
import           System.Console.GetOpt.Generics.Simple

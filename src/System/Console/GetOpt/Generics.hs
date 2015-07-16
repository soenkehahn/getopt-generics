
module System.Console.GetOpt.Generics (
  -- * IO API
  simpleCLI,
  getArguments,
  modifiedGetArguments,
  -- * Pure API
  parseArguments,
  Result(..),
  -- * Customizing the CLI
  Modifier(..),
  deriveShortOptions,
  -- * Available Field Types
  -- fixme: hide methods?
  Option(..),
  -- * SimpleCLI class
  SimpleCLI,
  ArgumentTypes,
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

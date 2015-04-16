# getopt-generics

## Status

This library is experimental.

## Usage

`getopt-generics` tries to make it very simple to create executables that parse
command line options. All you have to do is to define a type and derive some
instances:

~~~ {.haskell}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Readme where

import Data.Typeable
import GHC.Generics
import System.Console.GetOpt.Generics
import System.Environment

data Options
  = Options {
    port :: Int,
    daemonize :: Bool,
    config :: Maybe FilePath
  }
  deriving (Show, GHC.Generics.Generic)

instance System.Console.GetOpt.Generics.Generic Options
instance HasDatatypeInfo Options
~~~

Then you can use `getArguments` to create a command-line argument parser:

~~~ {.haskell}
main :: IO ()
main = do
  options <- getArguments
  print (options :: Options)
~~~

This program has

- a non-optional `--port` flag with an integer argument,
- a boolean flag `--daemonize`,
- an optional flag `--config` expecting a file argument and
- a generic `--help` option.

Here's in example of the program above in bash:
``` bash
$ program --port 8080 --config some/path
Options {port = 8080, daemonize = False, config = Just "some/path"}
$ program  --port 8080 --daemonize
Options {port = 8080, daemonize = True, config = Nothing}
$ program --port foo
not an integer: foo
$ program
missing option: --port=int
$ program --help
program
    --port=integer
    --daemonize
    --config=string (optional)
```

## Constraints

There are some constraints that the defined datatype has to fulfill:

  * It has to have only one constructor,
  * that constructor has to have field selectors (i.e. use record syntax) and
  * all fields have to be of a type that has an instance for `Option`.

(Types declared with `newtype` are allowed with the same constraints.)

## Using Custom Field Types

It is possible to use custom field types by providing an instance for `Option`.
Here's an example:

~~~ {.haskell}
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

-- Returns: FileOptions {file = File "some/file"}
getFileOptions :: IO FileOptions
getFileOptions = withArgs (words "--file some/file") getArguments
~~~

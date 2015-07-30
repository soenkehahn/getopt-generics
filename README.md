# getopt-generics

## Status

This library is experimental.

## Usage

`getopt-generics` tries to make it very simple to create command line
interfaces. Here's an example:

<!--- ### Start "docs/SimpleExample.hs" (MarkDown Haskell) ### -->

``` haskell
module SimpleExample where

import           System.Console.GetOpt.Generics

main :: IO ()
main = simpleCLI myMain

myMain :: String -> Int -> Bool -> IO ()
myMain s i b = print (s, i, b)
```

<!--- ### End ### -->

This is how the program behaves in bash:

<!--- ### Start "docs/SimpleExample.bash-protocol" (MarkDown Bash) ### -->

``` bash
$ runhaskell myProgram.hs foo 42 true
("foo",42,True)
$ runhaskell myProgram.hs foo 42 bar
cannot parse as BOOL: bar
$ runhaskell myProgram.hs --help
myProgram.hs [OPTIONS] STRING INTEGER BOOL
  -h  --help  show help and exit
```

<!--- ### End ### -->

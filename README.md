# getopt-generics

## Status

This library is experimental.

## Usage

`getopt-generics` tries to make it very simple to create command line
interfaces. Here's an example:

<!--- ### Start "docs/Simple.hs" (MarkDown Haskell) ### -->

``` haskell
module Simple where

import SimpleCLI

main :: IO ()
main = simpleCLI myMain

myMain :: String -> Int -> Bool -> IO ()
myMain s i b = print (s, i, b)
```

<!--- ### End ### -->

This is how the program behaves in a shell:

<!--- ### Start "docs/Simple.shell-protocol" (MarkDown Shell) ### -->

``` shell
$ program foo 42 true
("foo",42,True)
$ program --help
program [OPTIONS] STRING NUMBER BOOL
  -h  --help  show help and exit
$ program foo 42 bar
cannot parse as BOOL: bar
# exit-code 1
$ program
missing argument of type STRING
missing argument of type NUMBER
missing argument of type BOOL
# exit-code 1
$ program foo 42 yes bar
unknown argument: bar
# exit-code 1
```

<!--- ### End ### -->

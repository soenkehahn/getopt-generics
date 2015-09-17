
module Test01 where

import WithCli

main :: IO ()
main = withCli run

run :: Int -> Bool -> IO ()
run = curry print

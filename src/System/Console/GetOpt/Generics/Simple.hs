{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}

module System.Console.GetOpt.Generics.Simple where

import           Generics.SOP
import           System.Environment

import           System.Console.GetOpt.Generics.GetArguments
import           System.Console.GetOpt.Generics.Modifier
import           System.Console.GetOpt.Generics.Result

class SingI (ArgumentTypes main) => SimpleCLI main where
  {-# MINIMAL _initialFieldStates, _run #-}
  type ArgumentTypes main :: [*]
  _initialFieldStates :: Proxy main -> NP FieldState (ArgumentTypes main)
  _run :: NP I (ArgumentTypes main) -> main -> IO ()

-- | 'simpleCLI' converts an IO operation into a program with a nice CLI. @main@ can have
--   arbitrarily many parameters provided all parameters have an instance for 'Option'.
--
--   Example:
--
--   Given a file @myProgram.hs@:
--
--   > main :: IO ()
--   > main = simpleCLI myMain
--   >
--   > myMain :: String -> Int -> Bool -> IO ()
--   > myMain s i b = print (s, i, b)
--
--   you get:
--
--   > $ runhaskell myProgram.hs foo 42 true
--   > ("foo",42,True)
--   > $ runhaskell myProgram.hs foo 42 bar
--   > cannot parse as BOOL: bar
--   > $ runhaskell myProgram.hs --help
--   > myProgram.hs [OPTIONS] STRING INTEGER BOOL
--   >   -h  --help  show help and exit
simpleCLI :: forall main . (SimpleCLI main, All Option (ArgumentTypes main)) =>
  main -> IO ()
simpleCLI main = do
  args <- getArgs
  progName <- getProgName
  let result = do
        outputInfo progName (mkModifiers []) args
          (hliftA (const $ Comp NoSelector) (_initialFieldStates (Proxy :: Proxy main)))
        filledIn <- fillInPositionalArguments args (_initialFieldStates (Proxy :: Proxy main))
        collectResult filledIn
  f <- handleResult result
  _run f main

instance SimpleCLI (IO ()) where
  type ArgumentTypes (IO ()) = '[]
  _initialFieldStates Proxy = Nil
  _run Nil = id
  _run _ = impossible "_run"

instance (Option a, SimpleCLI rest) =>
  SimpleCLI (a -> rest) where
    type ArgumentTypes (a -> rest) = a ': ArgumentTypes rest
    _initialFieldStates Proxy = PositionalArgument :* _initialFieldStates (Proxy :: Proxy rest)
    _run (I a :* r) main = _run r (main a)
    _run _ _ = impossible "_run"

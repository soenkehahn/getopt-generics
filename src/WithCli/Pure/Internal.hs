{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module WithCli.Pure.Internal where

import           WithCli.HasArguments
import           WithCli.Modifier
import           WithCli.Parser
import           WithCli.Result

class WithCliPure function output where
  run :: String -> Modifiers -> Result (Parser Unnormalized input)
    -> (input -> function) -> [String] -> Result output

instance WithCliPure output output where
  run :: String -> Modifiers -> Result (Parser Unnormalized input) -> (input -> output)
    -> [String] -> Result output
  run progName modifiers mkParser function args = do
    mkParser >>= \ parser -> do
      input <- runParser progName modifiers
        (normalizeParser (applyModifiers modifiers parser))
        args
      return $ function input

instance (HasArguments input, WithCliPure function output) =>
  WithCliPure (input -> function) output where

  run :: String -> Modifiers -> Result (Parser Unnormalized otherInput)
    -> (otherInput -> (input -> function)) -> [String] -> Result output
  run progName modifiers mkParser function args = do
    run progName modifiers
      (combine mkParser (argumentsParser modifiers Nothing))
      (\ (otherInput, input) -> function otherInput input)
      args

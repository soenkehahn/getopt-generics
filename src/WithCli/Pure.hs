{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module WithCli.Pure (
  withCliPure,
  WithCliPure(),
  Result(..),
) where

import           System.Console.GetOpt.Generics.Modifier
import           WithCli.HasArguments
import           WithCli.Parser
import           WithCli.Result

withCliPure :: WithCliPure function a => String -> [Modifier] -> [String]
  -> function -> Result a
withCliPure progName modifiers args function = sanitize $ do
  modifiers <- mkModifiers modifiers
  _run progName modifiers (return $ emptyParser ()) (\ () -> function) args

class WithCliPure function output where
  _run :: String -> Modifiers -> Result (Parser Unnormalized input)
    -> (input -> function) -> [String] -> Result output

instance WithCliPure output output where
  _run :: String -> Modifiers -> Result (Parser Unnormalized input) -> (input -> output)
    -> [String] -> Result output
  _run progName modifiers mkParser function args = do
    mkParser >>= \ parser -> do
      input <- runParser progName modifiers
        (normalizeParser (applyModifiers modifiers parser))
        args
      return $ function input

instance (HasArguments input, WithCliPure function output) =>
  WithCliPure (input -> function) output where

  _run :: String -> Modifiers -> Result (Parser Unnormalized otherInput)
    -> (otherInput -> (input -> function)) -> [String] -> Result output
  _run progName modifiers mkParser function args = do
    _run progName modifiers
      (combine mkParser (argumentsParser modifiers Nothing))
      (\ (otherInput, input) -> function otherInput input)
      args

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module SimpleCLI.FromArguments where

import           Control.Arrow
import           Control.Monad
import           Data.Monoid
import           System.Console.GetOpt as Base

import           SimpleCLI.HelpFlag
import           SimpleCLI.Result
import           System.Console.GetOpt.Generics.FieldString
import           System.Console.GetOpt.Generics.Modifier.Types

data Help a
  = Help
  | Version String
  | NoHelp a
  deriving (Functor)

instance Monoid a => Monoid (Help a) where
  mappend a b = case (a, b) of
    (Help, _) -> Help
    (_, Help) -> Help
    (Version s, _) -> Version s
    (_, Version s) -> Version s
    (NoHelp a, NoHelp b) -> NoHelp (a <> b)
  mempty = NoHelp mempty

foldHelp :: [Help a] -> Help [a]
foldHelp flags = mconcat $ map (fmap pure) flags

parseFromArguments :: String -> Modifiers -> FromArguments Normalized a -> [String] -> Result a
parseFromArguments progName modifiers FromArguments{..} args = do
  let versionOptions = maybe []
        (\ v -> pure $ versionOption $ Version (progName ++ " version " ++ v))
        (getVersion modifiers)
      options = map (fmap NoHelp) parserOptions ++ [helpOption Help] ++ versionOptions
      (flagsHelp, nonOptions, errs) =
        Base.getOpt Base.Permute options args
  case foldHelp flagsHelp of
    Help -> OutputAndExit $
      let fields = case getPositionalArgumentType modifiers of
            Nothing -> map fst parserNonOptions
            Just typ -> ["[" ++ typ ++ "]"]
      in usage progName fields (map void options)
    Version msg -> OutputAndExit msg
    NoHelp flags ->
      reportErrors errs *>
      (fillInOptions flags parserDefault >>=
       fillInNonOptions (map snd parserNonOptions) nonOptions >>=
       parserConvert)
  where
    reportErrors :: [String] -> Result ()
    reportErrors = \ case
      [] -> return ()
      errs -> Errors errs

data FromArguments phase a where
  FromArguments :: {
    parserDefault :: uninitialized,
    parserOptions :: [OptDescr (Result (uninitialized -> uninitialized))],
    -- fixme: better data type
    parserNonOptions :: [(String, [String] -> Result (uninitialized -> uninitialized, [String]))],
    parserConvert :: uninitialized -> Result a
  } -> FromArguments phase a

deriving instance Functor (FromArguments phase)

-- phases:
data Unnormalized
data Normalized

emptyFromArguments :: a -> FromArguments phase a
emptyFromArguments a = FromArguments {
  parserDefault = a,
  parserOptions = [],
  parserNonOptions = [],
  parserConvert = return
}

normalizeFromArguments :: FromArguments Unnormalized a -> FromArguments Normalized a
normalizeFromArguments (FromArguments d options nonOptions convert) =
  FromArguments d (map (mapLongOptions normalize) options) nonOptions convert
  where
    mapLongOptions :: (String -> String) -> OptDescr a -> OptDescr a
    mapLongOptions f descr = case descr of
      -- fixme: no case
      Option shorts longs argDescr help ->
        Option shorts (map f longs) argDescr help

modParserOptions :: (forall x . [OptDescr (Result x)] -> [OptDescr (Result x)])
  -> FromArguments Unnormalized a -> FromArguments Unnormalized a
modParserOptions f (FromArguments def options nonOptions convert) =
  FromArguments def (f options) nonOptions convert

combine :: forall a b phase .
  Result (FromArguments phase a) -> Result (FromArguments phase b)
  -> Result (FromArguments phase (a, b))
combine a b = inner <$> a <*> b
  where
    inner :: FromArguments phase a -> FromArguments phase b -> FromArguments phase (a, b)
    inner (FromArguments defaultA optionsA nonOptionsA convertA) (FromArguments defaultB optionsB nonOptionsB convertB) =
      FromArguments {
        parserDefault = (defaultA, defaultB),
        parserOptions =
          map (fmap (fmap first)) optionsA ++ map (fmap (fmap second)) optionsB,
        parserNonOptions =
          map (fmap (fmap (fmap (first first)))) nonOptionsA ++
          map (fmap (fmap (fmap (first second)))) nonOptionsB,
        parserConvert =
          \ (u, v) -> (,) <$> (convertA u) <*> (convertB v)
      }

fillInOptions :: [Result (u -> u)] -> u -> Result u
fillInOptions [] u = return u
fillInOptions (option : options) u = do
  f <- option
  fillInOptions options (f u)

fillInNonOptions :: [[String] -> Result (u -> u, [String])] -> [String] -> u
  -> Result u
fillInNonOptions (parser : parsers) nonOptions@(_ : _) u = do
  (p, rest) <- parser nonOptions
  fillInNonOptions parsers rest (p u)
fillInNonOptions [] [] u =
  return u
fillInNonOptions [] nonOptions _ =
  Errors (map ("unknown argument: " ++) nonOptions)
fillInNonOptions _ [] u = return u

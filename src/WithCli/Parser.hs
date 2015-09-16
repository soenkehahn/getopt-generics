{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module WithCli.Parser where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
import           Control.Monad
import           Data.Monoid
import           System.Console.GetOpt as Base

import           System.Console.GetOpt.Generics.Modifier.Types
import           WithCli.HelpFlag
import           WithCli.Normalize
import           WithCli.Result

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

runParser :: String -> Modifiers -> Parser Normalized a -> [String] -> Result a
runParser progName modifiers Parser{..} args = do
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

data Parser phase a where
  Parser :: {
    parserDefault :: uninitialized,
    parserOptions :: [OptDescr (Result (uninitialized -> uninitialized))],
    -- fixme: better data type
    parserNonOptions :: [(String, [String] -> Result (uninitialized -> uninitialized, [String]))],
    parserConvert :: uninitialized -> Result a
  } -> Parser phase a

instance Functor (Parser phase) where
  fmap f (Parser def options nonOptions convert) =
    Parser def options nonOptions (fmap f . convert)

-- phases:
data Unnormalized
data Normalized

emptyParser :: a -> Parser phase a
emptyParser a = Parser {
  parserDefault = a,
  parserOptions = [],
  parserNonOptions = [],
  parserConvert = return
}

normalizeParser :: Parser Unnormalized a -> Parser Normalized a
normalizeParser (Parser d options nonOptions convert) =
  Parser d (map (mapLongOptions normalize) options) nonOptions convert
  where
    mapLongOptions :: (String -> String) -> OptDescr a -> OptDescr a
    mapLongOptions f descr = case descr of
      -- fixme: no case
      Option shorts longs argDescr help ->
        Option shorts (map f longs) argDescr help

modParserOptions :: (forall x . [OptDescr (Result x)] -> [OptDescr (Result x)])
  -> Parser Unnormalized a -> Parser Unnormalized a
modParserOptions f (Parser def options nonOptions convert) =
  Parser def (f options) nonOptions convert

combine :: forall a b phase .
  Result (Parser phase a) -> Result (Parser phase b)
  -> Result (Parser phase (a, b))
combine a b = inner <$> a <*> b
  where
    inner :: Parser phase a -> Parser phase b -> Parser phase (a, b)
    inner (Parser defaultA optionsA nonOptionsA convertA) (Parser defaultB optionsB nonOptionsB convertB) =
      Parser {
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

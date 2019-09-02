{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}

module WithCli.HasArguments where

import           Data.Orphans ()
import           Prelude ()
import           Prelude.Compat

import           Data.Char
import           Data.List.Compat
import           Data.Proxy
import           Data.Traversable
import qualified GHC.Generics as GHC
import           Generics.SOP as SOP
import           Generics.SOP.GGP as SOP
import           System.Console.GetOpt
import           Text.Read

import           WithCli.Argument
import           WithCli.Modifier
import           WithCli.Normalize
import           WithCli.Parser
import           WithCli.Result

parseArgumentResult :: forall a . Argument a => Maybe String -> String -> Result a
parseArgumentResult mMsg s = case parseArgument s of
  Just x -> return x
  Nothing -> parseError (argumentType (Proxy :: Proxy a)) mMsg s

parseError :: String -> Maybe String -> String -> Result a
parseError typ mMsg s = Errors $
  "cannot parse as " ++ typ ++
  maybe "" (\ msg -> " (" ++ msg ++ ")") mMsg ++
  ": " ++ s

-- | Everything that can be used as an argument to your @main@ function
--   (see 'withCli') needs to have a 'HasArguments' instance.
--
--   'HasArguments' also allows to conjure up instances for record types
--   to create more complex command line interfaces. Here's an example:

-- ### Start "docs/RecordType.hs" "module RecordType where\n\n" Haddock ###

-- |
-- >  {-# LANGUAGE DeriveAnyClass #-}
-- >  {-# LANGUAGE DeriveGeneric #-}
-- >
-- >  import WithCli
-- >
-- >  data Options
-- >    = Options {
-- >      port :: Int,
-- >      daemonize :: Bool,
-- >      config :: Maybe FilePath
-- >    }
-- >    deriving (Show, Generic, HasArguments)
-- >
-- >  main :: IO ()
-- >  main = withCli run
-- >
-- >  run :: Options -> IO ()
-- >  run = print

-- ### End ###

-- | In a shell this program behaves like this:

-- ### Start "docs/RecordType.shell-protocol" "" Haddock ###

-- |
-- >  $ program --port 8080 --config some/path
-- >  Options {port = 8080, daemonize = False, config = Just "some/path"}
-- >  $ program  --port 8080 --daemonize
-- >  Options {port = 8080, daemonize = True, config = Nothing}
-- >  $ program --port foo
-- >  cannot parse as INTEGER: foo
-- >  # exit-code 1
-- >  $ program
-- >  missing option: --port=INTEGER
-- >  # exit-code 1
-- >  $ program --help
-- >  program [OPTIONS]
-- >        --port=INTEGER
-- >        --daemonize
-- >        --config=STRING (optional)
-- >    -h  --help                      show help and exit

-- ### End ###

class HasArguments a where
  argumentsParser :: Modifiers -> Maybe String -> Result (Parser Unnormalized a)
  default argumentsParser ::
    (GHC.Generic a, GTo a, SOP.GDatatypeInfo a, All2 HasArguments (GCode a)) =>
    Modifiers ->
    Maybe String -> Result (Parser Unnormalized a)
  argumentsParser = const . genericParser

-- * atomic HasArguments

instance HasArguments Int where
  argumentsParser = atomicArgumentsParser

instance HasArguments Bool where
  argumentsParser = wrapForPositionalArguments "Bool" (const boolParser)

instance HasArguments String where
  argumentsParser = atomicArgumentsParser

instance HasArguments Float where
  argumentsParser = atomicArgumentsParser

instance HasArguments Double where
  argumentsParser = atomicArgumentsParser

instance (HasArguments a, HasArguments b) => HasArguments (a, b)

instance (HasArguments a, HasArguments b, HasArguments c) => HasArguments (a, b, c)

wrapForPositionalArguments :: String -> (Modifiers -> Maybe String -> Result a) -> (Modifiers -> Maybe String -> Result a)
wrapForPositionalArguments typ wrapped modifiers (Just field) =
  if isPositionalArgumentsField modifiers field
    then Errors ("UseForPositionalArguments can only be used for fields of type [String] not " ++ typ)
    else wrapped modifiers (Just field)
wrapForPositionalArguments _ wrapped modifiers Nothing = wrapped modifiers Nothing

instance Argument a => HasArguments (Maybe a) where
  argumentsParser _ = maybeParser

instance Argument a => HasArguments [a] where
  argumentsParser modifiers (Just field) =
    return $ if isPositionalArgumentsField modifiers field
      then positionalArgumentsParser
      else listParser (Just field)
  argumentsParser _ Nothing =
    return $ listParser Nothing

-- | Useful for implementing your own instances of 'HasArguments' on top
--   of a custom 'Argument' instance.
atomicArgumentsParser :: forall a . Argument a =>
  Modifiers ->
  Maybe String -> Result (Parser Unnormalized a)
atomicArgumentsParser =
  wrapForPositionalArguments typ inner
  where
    typ = argumentType (Proxy :: Proxy a)

    inner modifiers mLong = return $ case mLong of
      Nothing -> withoutLongOption
      Just long -> withLongOption modifiers long

    withoutLongOption = Parser {
      parserDefault = Nothing,
      parserOptions = [],
      parserNonOptions =
        [NonOptionsParser typ False (\ (s : r) -> fmap ((, r) . const . Just) $ parseArgumentResult Nothing s)],
      parserConvert = \ case
        Just a -> return a
        Nothing -> Errors $
          "missing argument of type " ++ typ
    }

    withLongOption modifiers long = Parser {
      parserDefault = Left (),
      parserOptions = pure $
        Option [] [long]
          (fmap (fmap (const . Right)) $
            ReqArg (parseArgumentResult Nothing) typ)
          "",
      parserNonOptions = [],
      parserConvert = \ case
        Right a -> return a
        Left () -> Errors $
          "missing option: --" ++ normalize (applyModifiersLong modifiers long) ++ "=" ++ typ
    }

listParser :: forall a . Argument a =>
  Maybe String -> Parser Unnormalized [a]
listParser mLong = case mLong of
  Nothing -> positionalArgumentsParser
  Just long -> Parser {
    parserDefault = [],
    parserOptions = pure $
      Option [] [long]
        (ReqArg
          (\ s -> fmap (\ a -> (++ [a])) (parseArgumentResult (Just "multiple possible") s))
          (argumentType (Proxy :: Proxy a) ++ " (multiple possible)"))
        "",
    parserNonOptions = [],
    parserConvert = return
  }

positionalArgumentsParser :: forall a . Argument a =>
  Parser Unnormalized [a]
positionalArgumentsParser = Parser {
  parserDefault = [],
  parserOptions = [],
  parserNonOptions = [NonOptionsParser (argumentType (Proxy :: Proxy a)) True parse],
  parserConvert = return
}
  where
    parse :: [String] -> Result ([a] -> [a], [String])
    parse args = do
      mods <- forM args $ \ arg ->
        case parseArgument arg of
          Just a -> return (a :)
          Nothing -> parseError (argumentType (Proxy :: Proxy a)) Nothing arg
      return (foldl' (.) id mods, [])

maybeParser :: forall a . Argument a =>
  Maybe String -> Result (Parser Unnormalized (Maybe a))
maybeParser mLong = case mLong of
  Nothing -> return $ Parser {
    parserDefault = Nothing,
    parserOptions = [],
    parserNonOptions =
      let parse :: [String] -> Result (Maybe a -> Maybe a, [String])
          parse (a : r) = do
            v <- parseArgumentResult (Just "optional") a
            return (const (Just v), r)
          parse [] = return (id, [])
      in [NonOptionsParser (argumentType (Proxy :: Proxy a)) True parse],
    parserConvert = return
  }
  Just long -> return $ Parser {
    parserDefault = Nothing,
    parserOptions = pure $
      Option [] [long]
        (ReqArg
          (\ s -> fmap (\ a -> (const (Just a))) (parseArgumentResult (Just "optional") s))
          (argumentType (Proxy :: Proxy a) ++ " (optional)"))
        "",
    parserNonOptions = [],
    parserConvert = return
  }

boolParser :: Maybe String -> Result (Parser Unnormalized Bool)
boolParser mLong = return $ case mLong of
  Nothing -> Parser {
    parserDefault = Nothing,
    parserOptions = [],
    parserNonOptions = pure $
      (NonOptionsParser "BOOL" False (\ (s : r) -> (, r) <$> maybe (parseError "BOOL" Nothing s) (return . const . Just) (parseBool s))),
    parserConvert = \ case
      Just x -> return x
      Nothing -> Errors $
        "missing argument of type BOOL"
  }
  Just long -> Parser {
    parserDefault = False,
    parserOptions = pure $
      Option [] [long]
        (NoArg (return (const True)))
        "",
    parserNonOptions = [],
    parserConvert = return
  }

parseBool :: String -> Maybe Bool
parseBool s
  | map toLower s `elem` ["true", "yes", "on"] = Just True
  | map toLower s `elem` ["false", "no", "off"] = Just False
  | otherwise = case readMaybe s of
    Just (n :: Integer) -> Just (n > 0)
    Nothing -> Nothing

-- * generic HasArguments

genericParser :: forall a .
  (GHC.Generic a, GTo a, GDatatypeInfo a, All2 HasArguments (GCode a)) =>
  Modifiers ->
  Result (Parser Unnormalized a)
genericParser modifiers = fmap (fmap gto) $
  let datatypeInfo = gdatatypeInfo (Proxy :: Proxy a)
      err :: forall a . String -> Result a
      err message = Errors $
        "getopt-generics doesn't support " ++ message ++
        " (" ++ datatypeName datatypeInfo ++ ")."
  in case constructorInfo datatypeInfo of
    firstConstructor :* Nil ->
      case firstConstructor of
        Record _ fields ->
          fmap (fmap (SOP . Z)) (fieldsParser modifiers fields)
        Constructor{} ->
          fmap (fmap (SOP . Z)) (noSelectorsParser modifiers shape)
        Infix{} -> err "infix constructors"
    Nil -> err "empty data types"
    _ :* _ :* _ -> err "sum types"

fieldsParser :: All HasArguments xs =>
  Modifiers -> NP FieldInfo xs -> Result (Parser Unnormalized (NP I xs))
fieldsParser modifiers = \ case
  Nil -> return $ emptyParser Nil
  FieldInfo fieldName :* rest ->
    fmap (fmap (\ (a, r) -> a :* r)) $
      combine (fmap (fmap I) $ (argumentsParser modifiers (Just fieldName))) (fieldsParser modifiers rest)

noSelectorsParser :: All HasArguments xs =>
  Modifiers -> Shape xs -> Result (Parser Unnormalized (NP I xs))
noSelectorsParser modifiers = \ case
  ShapeNil -> return $ emptyParser Nil
  ShapeCons rest ->
    fmap (fmap (\ (a, r) -> a :* r)) $
      combine (fmap (fmap I) $ (argumentsParser modifiers Nothing)) (noSelectorsParser modifiers rest)

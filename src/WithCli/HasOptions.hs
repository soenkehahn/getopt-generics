{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

module WithCli.HasOptions where

import           Data.Orphans ()
import           Prelude ()
import           Prelude.Compat

import           Data.Char
import           Data.List.Compat
import           Data.Proxy
import           Data.Traversable
import           Generics.SOP as SOP
import           System.Console.GetOpt
import           Text.Read

import           System.Console.GetOpt.Generics.Modifier
import           WithCli.FromArguments
import           WithCli.Normalize
import           WithCli.Option
import           WithCli.Result

parseArgumentResult :: forall a . Option a => Maybe String -> String -> Result a
parseArgumentResult mMsg s = case parseArgument s of
  Just x -> return x
  Nothing -> parseError (argumentType (Proxy :: Proxy a)) mMsg s

parseError :: String -> Maybe String -> String -> Result a
parseError typ mMsg s = Errors $ pure $
  "cannot parse as " ++ typ ++
  maybe "" (\ msg -> " (" ++ msg ++ ")") mMsg ++
  ": " ++ s

-- | Everything that can be used as a parameter to your @main@ function
--   (see 'withCli') needs to have a 'HasOptions' instance.
--
--   'HasOptions' also allows to conjure up instances for record types
--   to create more complex command line interfaces. Here's an example:

-- ### Start "docs/SimpleRecord.hs" "module SimpleRecord where\n\n" Haddock ###

-- |
-- >  {-# LANGUAGE DeriveGeneric #-}
-- >
-- >  import qualified GHC.Generics
-- >  import           System.Console.GetOpt.Generics
-- >
-- >  data Options
-- >    = Options {
-- >      port :: Int,
-- >      daemonize :: Bool,
-- >      config :: Maybe FilePath
-- >    }
-- >    deriving (Show, GHC.Generics.Generic)
-- >
-- >  instance Generic Options
-- >  instance HasDatatypeInfo Options
-- >  instance HasOptions Options
-- >
-- >  main :: IO ()
-- >  main = withCli $ \ options -> do
-- >    print (options :: Options)
-- >
-- >    -- todo: use myMain functions in docs?

-- ### End ###

-- | In a shell this program behaves like this:

-- ### Start "docs/SimpleRecord.shell-protocol" "" Haddock ###

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

class HasOptions a where
  fromArguments :: Modifiers -> Maybe String -> Result (FromArguments Unnormalized a)
  default fromArguments ::
    (SOP.Generic a, SOP.HasDatatypeInfo a, All2 HasOptions (Code a)) =>
    Modifiers ->
    Maybe String -> Result (FromArguments Unnormalized a)
  fromArguments = const . fromArgumentsGeneric

-- * atomic HasOptions

-- todo: better instance derivation for HasOptions
-- todo: HasOptions for Float and Double
-- todo: better names for HasOptions and Option

instance HasOptions Int where
  fromArguments = fromArgumentsOption

instance HasOptions Bool where
  fromArguments = wrapForPositionalArguments "Bool" (const fromArgumentsBool)

instance HasOptions String where
  fromArguments = fromArgumentsOption

instance HasOptions Float where
  fromArguments = fromArgumentsOption

instance HasOptions Double where
  fromArguments = fromArgumentsOption

wrapForPositionalArguments :: String -> (Modifiers -> Maybe String -> Result a) -> (Modifiers -> Maybe String -> Result a)
wrapForPositionalArguments typ wrapped modifiers (Just field) =
  if isPositionalArgumentsField modifiers field
    then Errors ["UseForPositionalArguments can only be used for fields of type [String] not " ++ typ]
    else wrapped modifiers (Just field)
wrapForPositionalArguments _ wrapped modifiers Nothing = wrapped modifiers Nothing

-- | todo
instance Option a => HasOptions (Maybe a) where
  fromArguments _ = fromArgumentsMaybe

instance Option a => HasOptions [a] where
  fromArguments modifiers (Just field) =
    if isPositionalArgumentsField modifiers field
      then return $ fromArgumentsPositionalArguments field
      else fromArgumentsList (Just field)
  fromArguments _ Nothing =
    fromArgumentsList Nothing

-- fixme: warnings

-- | Useful for implementing your own instances of 'HasOptions' on top
--   of a custom 'Option' instance.
fromArgumentsOption :: forall a . Option a =>
  Modifiers ->
  Maybe String -> Result (FromArguments Unnormalized a)
fromArgumentsOption =
  -- fixme: code layout
  wrapForPositionalArguments (argumentType (Proxy :: Proxy a)) $
  \ modifiers mLong ->
  return $ case mLong of
  Nothing -> FromArguments {
    parserDefault = Nothing,
    parserOptions = [],
    parserNonOptions =
      [(typ, \ (s : r) -> fmap ((, r) . const . Just) $ parseArgumentResult Nothing s)],
    parserConvert = \ case
      Just a -> return a
      Nothing -> Errors $ pure $
        "missing argument of type " ++ typ
  }
  Just long -> FromArguments {
    parserDefault = Left (),
    parserOptions = pure $
      Option [] [long]
        (fmap (fmap (const . Right)) $
          ReqArg (parseArgumentResult Nothing) typ)
        "",
    parserNonOptions = [],
    parserConvert = \ case
      Right a -> return a
      Left () -> Errors $ pure $
        "missing option: --" ++ normalize (applyModifiersLong modifiers long) ++ "=" ++ typ
  }
  where
    typ = argumentType (Proxy :: Proxy a)

fromArgumentsList :: forall a . Option a =>
  Maybe String -> Result (FromArguments Unnormalized [a])
fromArgumentsList mLong = return $ case mLong of
  Nothing -> fromArgumentsPositionalArguments "fixme"
  Just long -> FromArguments {
    parserDefault = [],
    parserOptions = pure $
      Option [] [long]
        (ReqArg
          (\ s -> fmap (\ a -> (++ [a])) (parseArgumentResult (Just "multiple possible") s))
          (argumentType (Proxy :: Proxy a) ++ (error "fixme") "<multiple> fixme"))
        "",
    parserNonOptions = [],
    parserConvert = return
  }

fromArgumentsPositionalArguments :: forall a . Option a =>
  String -> FromArguments Unnormalized [a]
fromArgumentsPositionalArguments selector = FromArguments {
  parserDefault = [],
  parserOptions = [],
  parserNonOptions = [(selector, parse)],
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

fromArgumentsMaybe :: forall a . Option a =>
  Maybe String -> Result (FromArguments Unnormalized (Maybe a))
fromArgumentsMaybe mLong = return $ case mLong of
  Nothing -> (error "fixme")
  Just long -> FromArguments {
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

fromArgumentsBool :: Maybe String -> Result (FromArguments Unnormalized Bool)
fromArgumentsBool mLong = return $ case mLong of
  Nothing -> FromArguments {
    parserDefault = Nothing,
    parserOptions = [],
    parserNonOptions = pure $
      ("BOOL", \ (s : r) -> (, r) <$> maybe (parseError "BOOL" Nothing s) (return . const . Just) (parseBool s)),
    parserConvert = \ case
      Just x -> return x
      Nothing -> Errors $ pure $
        "missing argument of type BOOL"
  }
  Just long -> FromArguments {
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

-- * generic HasOptions

fromArgumentsGeneric :: forall a .
  (Generic a, HasDatatypeInfo a, All2 HasOptions (Code a)) =>
  Modifiers ->
  Result (FromArguments Unnormalized a)
fromArgumentsGeneric modifiers = fmap (fmap to) $ case datatypeInfo (Proxy :: Proxy a) of
  ADT _ typeName (constructorInfo :* Nil) ->
    case constructorInfo of
      (Record _ fields) ->
        fmap (fmap (SOP . Z)) (fromArgumentsFields modifiers fields)
      Constructor{} ->
        fmap (fmap (SOP . Z)) (fromArgumentsNoSelectors modifiers shape)
      Infix{} ->
        err typeName "infix constructors"
  ADT _ typeName Nil ->
    err typeName "empty data types"
  ADT _ typeName (_ :* _ :* _) ->
    err typeName "sum types"
  Newtype _ _ (Record _ _fields) ->
    (error "fixme") -- mapSingleNS $ h fields
  Newtype _ typeName (Constructor _) ->
    err typeName "constructors without field labels"
  where
    err typeName message = Errors $ pure $
      "getopt-generics doesn't support " ++ message ++
      " (" ++ typeName ++ ")."

-- fixme: combinators?
-- fixme: consistent syntax
fromArgumentsFields :: All HasOptions xs => Modifiers -> NP FieldInfo xs -> Result (FromArguments Unnormalized (NP I xs))
fromArgumentsFields _ Nil = return $ emptyFromArguments Nil
fromArgumentsFields modifiers (FieldInfo fieldName :* rest) =
  fmap (fmap (\ (a, r) -> a :* r)) $
    combine (fmap (fmap I) $ (fromArguments modifiers (Just fieldName))) (fromArgumentsFields modifiers rest)

fromArgumentsNoSelectors :: All HasOptions xs =>
  Modifiers -> Shape xs -> Result (FromArguments Unnormalized (NP I xs))
fromArgumentsNoSelectors modifiers = \ case
  ShapeNil -> return $ emptyFromArguments Nil
  ShapeCons rest -> 
    fmap (fmap (\ (a, r) -> a :* r)) $
      combine (fmap (fmap I) $ (fromArguments modifiers Nothing)) (fromArgumentsNoSelectors modifiers rest)

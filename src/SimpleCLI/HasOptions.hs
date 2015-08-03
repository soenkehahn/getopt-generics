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

module SimpleCLI.HasOptions where

import           Data.Orphans ()
import           Prelude ()
import           Prelude.Compat

import           Data.Char
import           Data.List
import           Data.Proxy
import           Data.Traversable
import           Generics.SOP as SOP
import           System.Console.GetOpt
import           Text.Read

import           SimpleCLI.FromArguments
import           SimpleCLI.Option
import           SimpleCLI.Result
import           System.Console.GetOpt.Generics.FieldString
import           System.Console.GetOpt.Generics.Modifier

parseArgumentResult :: forall a . Option a => Maybe String -> String -> Result a
parseArgumentResult mMsg s = case parseArgument s of
  Just x -> return x
  Nothing -> parseError (argumentType (Proxy :: Proxy a)) mMsg s

parseError :: String -> Maybe String -> String -> Result a
parseError typ mMsg s = Errors $ pure $
  "cannot parse as " ++ typ ++
  maybe "" (\ msg -> " (" ++ msg ++ ")") mMsg ++
  ": " ++ s

class HasOptions a where
  fromArguments :: Modifiers -> Maybe String -> Result (FromArguments Unnormalized a)
  default fromArguments ::
    (SOP.Generic a, SOP.HasDatatypeInfo a, All2 HasOptions (Code a)) =>
    Modifiers ->
    Maybe String -> Result (FromArguments Unnormalized a)
  fromArguments modifiers _ = fromArgumentsGeneric modifiers

-- * atomic HasOptions

-- todo: better instance derivation for HasOptions
-- fixme: DRY up

instance HasOptions Int where
  fromArguments modifiers (Just field) =
    if isPositionalArgumentsField modifiers field
      then Errors ["UseForPositionalArguments can only be used for fields of type [String] not Int"]
      else fromArgumentsOption modifiers (Just field)
  fromArguments modifiers Nothing = fromArgumentsOption modifiers Nothing

instance HasOptions Bool where
  fromArguments modifiers (Just field) =
    if isPositionalArgumentsField modifiers field
      then Errors ["UseForPositionalArguments can only be used for fields of type [String] not Bool"]
      else fromArgumentsBool (Just field)
  fromArguments _ Nothing = fromArgumentsBool Nothing

instance HasOptions String where
  fromArguments = fromArgumentsOption

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

fromArgumentsOption :: forall a . Option a =>
  Modifiers ->
  Maybe String -> Result (FromArguments Unnormalized a)
fromArgumentsOption modifiers mLong = return $ case mLong of
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

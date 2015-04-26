{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module System.Console.GetOpt.Generics.Internal where

import           Prelude                               ()
import           Prelude.Compat

import           Data.Char
import           Generics.SOP

import           System.Console.GetOpt.Generics.Result

normalizedDatatypeInfo :: (HasDatatypeInfo a, Code a ~ xss, SingI xss) =>
  Proxy a -> Result (DatatypeInfo xss)
normalizedDatatypeInfo p =
  mapFieldInfoM
    (\ (FieldInfo s) -> FieldInfo <$> normalizeFieldName s)
    (datatypeInfo p)

mapFieldInfoM :: (SingI xss, Applicative m) =>
     (forall a . FieldInfo a -> m (FieldInfo a))
  -> DatatypeInfo xss -> m (DatatypeInfo xss)
mapFieldInfoM f info = case info of
    (ADT mod name constructors) ->
      ADT mod name <$> (mapConsInfo (mapSingleCons f) constructors)
    (Newtype mod name constructor) ->
      Newtype mod name <$> (mapSingleCons f constructor)
  where
    mapSingleCons :: forall m xs . (Applicative m) =>
         (forall a . FieldInfo a -> m (FieldInfo a))
      -> ConstructorInfo xs -> m (ConstructorInfo xs)
    mapSingleCons f c = case c of
      (Record name fields) -> Record name <$> (mapFieldInfo f fields)
      cons@Infix{} -> pure cons
      cons@Constructor{} -> pure cons

    mapConsInfo :: forall m xs . Applicative m =>
         (forall a . ConstructorInfo a -> m (ConstructorInfo a))
      -> NP ConstructorInfo xs -> m (NP ConstructorInfo xs)
    mapConsInfo _ Nil = pure Nil
    mapConsInfo f (a :* r) = (:*) <$> f a <*> mapConsInfo f r

    mapFieldInfo :: forall m xs . Applicative m =>
         (forall a . FieldInfo a -> m (FieldInfo a))
      -> NP FieldInfo xs -> m (NP FieldInfo xs)
    mapFieldInfo _ Nil = pure Nil
    mapFieldInfo f (a :* r) = (:*) <$> f a <*> mapFieldInfo f r

normalizeFieldName :: String -> Result String
normalizeFieldName s =
    let normalized = dropWhile (== '-') $
          filter (\ c -> (isAscii c && isAlpha c) || (c == '-')) s
    in case normalized of
      "" -> Errors ["unsupported field name: " ++ s]
      x -> Success $ slugify x
  where
    slugify (a : r)
      | isUpper a = slugify (toLower a : r)
    slugify (a : b : r)
      | isUpper b = a : '-' : slugify (toLower b : r)
      | otherwise = a : slugify (b : r)
    slugify x = x

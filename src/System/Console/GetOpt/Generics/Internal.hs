{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}

module System.Console.GetOpt.Generics.Internal where

import           Data.Char
import           Generics.SOP

normalizedDatatypeInfo :: (HasDatatypeInfo a, Code a ~ xss, SingI xss) =>
  Proxy a -> DatatypeInfo xss
normalizedDatatypeInfo p = mapFieldInfo (\ (FieldInfo s) -> FieldInfo (normalizeFieldName s)) (datatypeInfo p)

mapFieldInfo :: (SingI xss) =>
  (forall b . FieldInfo b -> FieldInfo b) -> DatatypeInfo xss -> DatatypeInfo xss
mapFieldInfo f info = case info of
    (ADT mod name constructors) -> ADT mod name (hliftA (mapSingleCons f) constructors)
    (Newtype mod name constructor) -> Newtype mod name (mapSingleCons f constructor)
  where
    mapSingleCons :: (forall b . FieldInfo b -> FieldInfo b) -> ConstructorInfo xs -> ConstructorInfo xs
    mapSingleCons f c = case c of
      (Record name fields) -> Record name (hliftA f fields)
      cons@Infix{} -> cons
      cons@Constructor{} -> cons

normalizeFieldName :: String -> String
normalizeFieldName =
    slugify . filter (\ c -> (isAscii c && isAlpha c) || (c == '-'))
  where
    slugify (a : r)
      | isUpper a = slugify (toLower a : r)
    slugify (a : b : r)
      | isUpper b = a : '-' : slugify (toLower b : r)
      | otherwise = a : slugify (b : r)
    slugify x = x

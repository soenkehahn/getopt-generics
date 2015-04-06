{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module System.Console.GetOpt.Generics.Internal where

import           Data.Char
import           Generics.SOP

normalizedDatatypeInfo :: forall a xss . (HasDatatypeInfo a, Code a ~ xss, SingI xss) =>
  Proxy a -> DatatypeInfo xss
normalizedDatatypeInfo p = mapFieldInfo (\ (FieldInfo s) -> FieldInfo (slugify s)) (datatypeInfo p)

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

slugify :: String -> String
slugify [] = []
slugify (x : xs)
  | isUpper x = '-' : toLower x : slugify xs
  | otherwise = x : slugify xs

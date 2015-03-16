{-# LANGUAGE ConstraintKinds, DeriveGeneric, GADTs, ScopedTypeVariables #-}

module Opt where

import           Data.List
import           Generics.SOP

foo :: forall a . (HasDatatypeInfo a, Generic a, All2 Show (Code a))
  => a -> String
foo a = case (datatypeInfo (Proxy :: Proxy a), (from a)) of
  ((ADT _module _typeName meta), (SOP value)) -> baz meta value
  _ -> error "bar"

baz :: (All2 Show xss) =>
  NP ConstructorInfo xss -> (NS (NP I) xss) -> String
baz (_ :* r) (S rv) = baz r rv
baz (Constructor n :* _) (Z fields) = n ++ " " ++ intercalate ", " (showFields fields)
baz (Record n recordMeta :* _) (Z record) =
  n ++ " {" ++ intercalate ", " (showRecord recordMeta record) ++ "}"

showRecord :: (All Show xs) =>
  NP FieldInfo xs -> NP I xs -> [String]
showRecord (FieldInfo fieldName :* rMeta) (I v :* rv) =
  (fieldName ++ " = " ++ show v) : showRecord rMeta rv
showRecord Nil Nil = []
showRecord _ _ = error "shouldn't happen"

showFields :: (All Show xs) =>
  NP I xs -> [String]
showFields (I a :* r) = show a : showFields r
showFields Nil = []

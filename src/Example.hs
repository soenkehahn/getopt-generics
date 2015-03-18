{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Generics.SOP
import Safe
import qualified GHC.Generics as GHC
import Options.Applicative

main :: IO ()
main = withOption $ \option ->
  print (option :: Option)

withOption :: (Generic a, HasDatatypeInfo a, All2 ToOption (Code a), All Show (Map ConstructorInfo (Code a))) => (a -> IO ()) -> IO ()
withOption action = execParser opts >>= action
  where
    opts = info parser fullDesc

parser :: forall a. (Generic a, HasDatatypeInfo a, All2 ToOption (Code a), All Show (Map ConstructorInfo (Code a)))
       => Parser a
parser = case datatypeInfo (Proxy :: Proxy a) of
           ADT _ _ cs -> to <$> SOP <$> parser' cs
           -- Newtype _ _ c -> parser' (c :* Nil)
  where
    parser' ::
      NP ConstructorInfo (Code a) -> Parser (NS (NP I) (Code a))
    parser' (Record _ fields :* Nil) = Z <$> parser'' fields
    parser' _cs = error $ ""

    parser'' :: (All ToOption xs) => NP FieldInfo xs -> Parser (NP I xs)
    parser'' Nil = pure Nil
    parser'' (field :* r) =
      (:*) <$> (I <$> goField field) <*> (parser'' r)
      --to . SOP . Z <$> hsequence (hcpure (Proxy :: Proxy ToOption) (toOpt "abc"))

goField :: (ToOption a) => FieldInfo a -> Parser a
goField (FieldInfo field) = toOpt field

data Option = Option
  { port :: String
  , debug :: Bool
  , int :: Int
  } deriving (GHC.Generic, Show)

instance Generic Option
instance HasDatatypeInfo Option

class ToOption a where
  toOpt :: String -> Parser a

instance ToOption String where
  toOpt name = strOption (long name)

instance ToOption Bool where
  toOpt name = switch (long name)

instance ToOption Int where
  toOpt name =
    readMay <$> strOption (long name)

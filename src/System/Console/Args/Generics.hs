{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module System.Console.Args.Generics (withArguments) where

import           Generics.SOP
import           Options.Applicative

withArguments :: (Generic a, HasDatatypeInfo a, All2 HasOptParser (Code a)) =>
  (a -> IO ()) -> IO ()
withArguments action =
    execParser opts >>= action
  where
    opts = info parser fullDesc

parser :: forall a . (Generic a, HasDatatypeInfo a, All2 HasOptParser (Code a)) =>
       Parser a
parser = case datatypeInfo (Proxy :: Proxy a) of
    ADT _ _ cs -> parseRecord cs
    Newtype _ _ c -> parseRecord (c :* Nil)

parseRecord :: (Generic a, HasDatatypeInfo a, All2 HasOptParser (Code a)) =>
  NP ConstructorInfo (Code a) -> Parser a
parseRecord (Record _ fields :* Nil) = to <$> SOP <$> Z <$> parseFields fields
parseRecord _ = undefined

parseFields :: (All HasOptParser xs) => NP FieldInfo xs -> Parser (NP I xs)
parseFields Nil = pure Nil
parseFields (field :* r) =
  (:*) <$> (I <$> parseField field) <*> (parseFields r)

parseField :: (HasOptParser a) => FieldInfo a -> Parser a
parseField (FieldInfo field) = getOptParser field

class HasOptParser a where
  getOptParser :: String -> Parser a

instance HasOptParser String where
  getOptParser name = strOption (long name)

instance HasOptParser Bool where
  getOptParser name = switch (long name)

instance HasOptParser Int where
  getOptParser name = option auto (long name)

instance HasOptParser a => HasOptParser (Maybe a) where
  getOptParser name = optional (getOptParser name)

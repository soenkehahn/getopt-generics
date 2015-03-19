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
import           System.Exit
import           System.IO

withArguments :: (Generic a, HasDatatypeInfo a, All2 HasOptParser (Code a)) =>
  (a -> IO ()) -> IO ()
withArguments action = do
  case parser of
    Left errorMessage -> do
      hPutStrLn stderr errorMessage
      exitWith $ ExitFailure 1
    Right p -> execParser (info p fullDesc) >>= action

parser :: forall a . (Generic a, HasDatatypeInfo a, All2 HasOptParser (Code a)) =>
       Either String (Parser a)
parser = case datatypeInfo (Proxy :: Proxy a) of
    ADT _ typeName cs -> parseRecord typeName cs
    Newtype _ typeName c -> parseRecord typeName (c :* Nil)

parseRecord :: (Generic a, HasDatatypeInfo a, All2 HasOptParser (Code a)) =>
  DatatypeName -> NP ConstructorInfo (Code a) -> Either String (Parser a)
parseRecord typeName meta = case meta of
    (Record _ fields :* Nil) ->
      Right (to <$> SOP <$> Z <$> parseFields fields)
    (_ :* _ :* _) -> err "sum-types"
    Nil -> err "empty data types"
    (Infix{} :* Nil) -> err "infix constructors"
    (Constructor{} :* Nil) -> err "constructors without field labels"
  where
    err :: String -> Either String (Parser a)
    err message =
      Left ("args-generics doesn't support " ++ message ++ " (" ++ typeName ++ ").")

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

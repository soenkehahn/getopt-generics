{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import Generics.SOP
import qualified GHC.Generics as GHC
import Options.Applicative

main :: IO ()
main = withOption $ \option ->
  print $ port option

withOption :: (Generic a, HasDatatypeInfo a, All2 ToOption (Code a), All Show (Map ConstructorInfo (Code a))) => (a -> IO ()) -> IO ()
withOption action = execParser opts >>= action
  where
    opts = info parser fullDesc

parser :: forall a. (Generic a, HasDatatypeInfo a, All2 ToOption (Code a), All Show (Map ConstructorInfo (Code a)))
       => Parser a
parser = case datatypeInfo (Proxy :: Proxy a) of
           ADT _ _ cs -> parser' cs
           Newtype _ _ c -> parser' (c :* Nil)
  where
    parser' :: NP ConstructorInfo (Code a) -> Parser a
    parser' (Record _ fields :* Nil) = parser'' fields
    parser' cs = error $ ""

    parser'' :: NP FieldInfo xs -> Parser a
    parser'' fields = error $ show ""
      --to . SOP . Z <$> hsequence (hcpure (Proxy :: Proxy ToOption) (toOpt "abc"))

goField :: (ToOption a) => FieldInfo a -> K (Parser String) a
goField (FieldInfo field) = K $ toOpt field

data Option = Option
  { port :: String
  , debug :: Bool
  } deriving (GHC.Generic)

instance Generic Option
instance HasDatatypeInfo Option

class ToOption a where
  toOpt :: String -> Parser a

instance ToOption String where
  toOpt name = strOption (long name)

instance ToOption Bool where
  toOpt name = switch (long name)

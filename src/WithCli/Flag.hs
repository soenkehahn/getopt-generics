{-# LANGUAGE DeriveFunctor #-}

module WithCli.Flag where

import           Prelude ()
import           Prelude.Compat

import           Data.Monoid
import           System.Console.GetOpt

data Flag a
  = Help
  | Version String
  | NoHelp a
  deriving (Functor)

instance Monoid a => Monoid (Flag a) where
  mappend a b = case (a, b) of
    (Help, _) -> Help
    (_, Help) -> Help
    (Version s, _) -> Version s
    (_, Version s) -> Version s
    (NoHelp a, NoHelp b) -> NoHelp (a <> b)
  mempty = NoHelp mempty

foldFlags :: [Flag a] -> Flag [a]
foldFlags flags = mconcat $ map (fmap pure) flags

helpOption :: OptDescr (Flag a)
helpOption =
  Option ['h'] ["help"] (NoArg Help) "show help and exit"

versionOption :: String -> OptDescr (Flag a)
versionOption version =
  Option ['v'] ["version"] (NoArg (Version version)) "show version and exit"

usage :: String -> [String] -> [OptDescr ()] -> String
usage progName fields options = usageInfo header options
  where
    header :: String
    header = unwords $
      progName :
      "[OPTIONS]" :
       fields ++
      []

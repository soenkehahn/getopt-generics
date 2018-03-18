{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module WithCli.Flag where

import           Prelude ()
import           Prelude.Compat

import           Data.List
import           Data.Maybe
import           System.Console.GetOpt

data Flag a
  = Help
  | Version String
  | NoHelp a
  deriving (Functor)

flagConcat :: Monoid a => [Flag a] -> Flag a
flagConcat = foldl' flagAppend (NoHelp mempty)
  where
    flagAppend :: Monoid a => Flag a -> Flag a -> Flag a
    flagAppend a b = case (a, b) of
      (Help, _) -> Help
      (_, Help) -> Help
      (Version s, _) -> Version s
      (_, Version s) -> Version s
      (NoHelp a, NoHelp b) -> NoHelp (mappend a b)

foldFlags :: [Flag a] -> Flag [a]
foldFlags flags = flagConcat $ map (fmap pure) flags

helpOption :: OptDescr (Flag a)
helpOption =
  Option ['h'] ["help"] (NoArg Help) "show help and exit"

versionOption :: String -> OptDescr (Flag a)
versionOption version =
  Option ['v'] ["version"] (NoArg (Version version)) "show version and exit"

usage :: String -> [(Bool, String)] -> [OptDescr ()] -> String
usage progName fields options = usageInfo header options
  where
    header :: String
    header = unwords $
      progName :
      "[OPTIONS]" :
      fromMaybe [] (formatFields fields) ++
      []

    formatFields :: [(Bool, String)] -> Maybe [String]
    formatFields [] = Nothing
    formatFields fields = Just $
      let (map snd -> nonOptional, map snd -> optional) =
            span (not . fst) fields
      in nonOptional ++ [formatOptional optional]

    formatOptional :: [String] -> String
    formatOptional [] = ""
    formatOptional [a] = "[" ++ a ++ "]"
    formatOptional (a : r) = "[" ++ a ++ " " ++ formatOptional r ++ "]"

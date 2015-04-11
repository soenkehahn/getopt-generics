
module System.Console.GetOpt.Generics.InternalSpec where

import           Data.Char
import           Test.Hspec
import           Test.QuickCheck hiding (Success)

import           System.Console.GetOpt.Generics.Internal
import           System.Console.GetOpt.Generics.Result

spec :: Spec
spec = do
  describe "normalizeFieldName" $ do
    it "is idempotent" $ do
      property $ \ x ->
        (normalizeFieldName =<< normalizeFieldName x)
          `shouldBe` normalizeFieldName x

    it "contains only valid characters" $ do
      property $ \ x ->
        case normalizeFieldName x of
          Success s -> s `shouldSatisfy`
            (\ s -> all (\ c -> c `elem` ['a' .. 'z'] || c == '-') s)
          _ -> return ()

    it "complains on field names without alphabetic characters" $ do
      property $
        forAll (listOf (elements "-_$/|\\#~_[]")) $ \ s ->
        normalizeFieldName s `shouldBe`
          Errors [("unsupported field name: " ++ s)]

    it "replaces camelCase with dashes" $
      property $
        \ prefix suffix ->
        (fmap (not . null) (normalizeFieldName prefix) == Success True) ==>
        forAll upperCaseChar $ \ upper ->
        counterexample (prefix ++ [upper] ++ suffix) $
        normalizeFieldName (prefix ++ [upper] ++ suffix)
          `shouldBe` do
            normalizedPrefix <- normalizeFieldName prefix
            normalizedRest <- normalizeFieldName (toLower upper : suffix)
            return $ normalizedPrefix ++ "-" ++ normalizedRest

upperCaseChar :: Gen Char
upperCaseChar = elements ['A' .. 'Z']

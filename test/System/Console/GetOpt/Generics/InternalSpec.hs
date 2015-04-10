
module System.Console.GetOpt.Generics.InternalSpec where

import           Data.Char
import           Test.Hspec
import           Test.QuickCheck

import           System.Console.GetOpt.Generics.Internal

spec :: Spec
spec = do
  describe "normalizeFieldName" $ do
    it "is idempotent" $ do
      property $ \ x ->
        normalizeFieldName (normalizeFieldName x)
          `shouldBe` normalizeFieldName x

    it "contains only valid characters" $ do
      property $ \ x ->
        normalizeFieldName x `shouldSatisfy`
          all (\ c -> c `elem` ['a' .. 'z'] || c == '-')

    it "replaces camelCase with dashes" $
      property $
        \ prefix suffix ->
        not (null (normalizeFieldName prefix)) ==>
        forAll upperCaseChar $ \ upper ->
        counterexample (prefix ++ [upper] ++ suffix) $
        normalizeFieldName (prefix ++ [upper] ++ suffix)
          `shouldBe`
          (normalizeFieldName prefix ++
           '-' : normalizeFieldName (toLower upper : suffix))

upperCaseChar :: Gen Char
upperCaseChar = elements ['A' .. 'Z']

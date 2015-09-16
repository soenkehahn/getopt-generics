{-# LANGUAGE ViewPatterns #-}

module WithCli.NormalizeSpec where

import           Data.Char
import           Test.Hspec
import           Test.QuickCheck hiding (Success)

import           WithCli.Normalize

isValidInputChar :: Char -> Bool
isValidInputChar c = c `elem` ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"

isAllowedOutputChar :: Char -> Bool
isAllowedOutputChar c = c `elem` ['a' .. 'z'] ++ ['0' .. '9'] ++ "-"

upperCaseChar :: Gen Char
upperCaseChar = elements ['A' .. 'Z']

spec :: Spec
spec = do
  describe "normalize" $ do
    it "is idempotent" $ do
      property $ \ x -> do
        let once = normalize x
            twice = normalize once
        once `shouldBe` twice

    it "replaces underscores with dashes" $ do
      normalize "foo_bar" `shouldBe` "foo-bar"

    it "doesn't modify digits" $ do
      normalize "foo2bar" `shouldBe` "foo2bar"

    it "when there's one valid character it returns only dashes and lower case characters" $ do
      property $ \ x ->
        any isValidInputChar x ==>
        normalize x `shouldSatisfy`
          (\ s -> all isAllowedOutputChar s)

    it "when there are no valid characters it returns its input" $ do
      property $ forAll (listOf (arbitrary `suchThat` (not . isValidInputChar))) $ \ x ->
        normalize x `shouldBe` x

    it "replaces camelCase with dashes" $ do
      let isValidPrefixChar c = c `elem` ['A' .. 'Z'] ++ ['a' .. 'z']
      property $
        \ prefix suffix ->
        (any isValidPrefixChar prefix) ==>
        forAll upperCaseChar $ \ upper ->
        counterexample (prefix ++ [upper] ++ suffix) $
        normalize (prefix ++ [upper] ++ suffix)
          `shouldBe`
            normalize prefix ++ "-" ++ normalize (toLower upper : suffix)

  describe "matches" $ do
    it "matches normalized strings" $ do
      property $ \ s ->
        normalize s `matches` s

    it "matches unnormalized strings" $ do
      property $ \ s ->
        s `matches` s

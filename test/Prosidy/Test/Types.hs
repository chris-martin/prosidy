{-# LANGUAGE OverloadedStrings #-}
module Prosidy.Test.Types (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Prosidy.Types.Key as Key
import qualified Prosidy.Types.Series as Series
import qualified Data.Text as Text
import qualified Data.Sequence as Seq

tests :: TestTree
tests = testGroup "types"
    [ testKey
    , testSeriesNE
    ]

testKey :: TestTree
testKey = testGroup "key" 
    [ testProperty "valid" $ forAll validGen $ \raw ->
        fmap Key.rawKey (Key.makeKey raw) === Right raw
    , testProperty "invalid-head" $ forAll invalidHeadGen $ \raw ->
        case Key.makeKey raw of
            Right _ -> label "key creation should fail" False
            Left Key.EmptyKeyError -> label "key should be nonempty" $ property False
            Left (Key.InvalidCharacterError (Key.InvalidCharacter raw' nth ch)) ->
                conjoin
                    [ label "error string matches input" $ raw === raw'
                    , label "invalid at index 0" $ nth === 0
                    , label "char is invalid" $ not (Key.isValidKeyHead ch)
                    ]
    , testProperty "invalid-tail" $ forAll invalidTailGen $ \raw ->
        case Key.makeKey raw of
            Right _ -> label "key creation should fail" False
            Left Key.EmptyKeyError -> label "key should be nonempty" $ property False
            Left (Key.InvalidCharacterError (Key.InvalidCharacter raw' nth ch)) ->
                conjoin
                    [ label "error string matches input" $ raw === raw'
                    , label "invalid at index > 0" $ nth =/= 0
                    , label "char is invalid" $ not (Key.isValidKeyTail ch)
                    ]
    , testProperty "empty" $
        case Key.makeKey "" of
            Right _ -> label "key creation should fail" False
            Left Key.EmptyKeyError -> property ()
            Left (Key.InvalidCharacterError _) -> label "input should be empty" False
    ]
  where
    arbHead = arbitraryUnicodeChar `suchThat` Key.isValidKeyHead
    arbTail = listOf $ arbitraryUnicodeChar `suchThat` Key.isValidKeyTail

    validGen = do
        keyHead <- arbHead
        keyTail <- arbTail
        pure . Text.pack $ keyHead : keyTail

    invalidHeadGen = do
        keyHead <- arbitraryUnicodeChar `suchThat` (not . Key.isValidKeyHead)
        keyTail <- arbTail
        pure . Text.pack $ keyHead : keyTail

    invalidTailGen = do
        keyHead  <- arbHead
        keyTail0 <- arbTail
        keyTail1 <- elements "\n\t\v\\#{}[]:=, "
        keyTail2 <- arbTail
        pure . Text.pack $ keyHead : keyTail0 ++ keyTail1 : keyTail2

testSeriesNE :: TestTree
testSeriesNE = testGroup "SeriesNE"
    [ testProperty "nonempty" $
        forAll genSeq1 $ \xs ->
            fmap Series.toSeqNE (Series.fromSeqNE xs) === Just xs
    , testProperty "empty" $
        Series.fromSeqNE (mempty :: Seq.Seq Word) === Nothing
    ]
  where
    genSeq1 = fmap Seq.fromList . listOf1 $ (arbitrary :: Gen Word)
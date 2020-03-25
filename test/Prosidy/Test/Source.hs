{-# LANGUAGE OverloadedStrings #-}
module Prosidy.Test.Source (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Data.Foldable                  ( toList
                                                , for_
                                                )
import           Data.IORef
import           System.IO.Unsafe
import           Control.Exception              ( evaluate )

import qualified Prosidy.Source                as PS
import qualified Data.Text                     as T

tests :: TestTree
tests = testGroup
    "source"
    [ testEmptyLineMap
    , testSimpleLineMap
    , testLineDelimiters
    , testLocation
    , testLocationLazy
    , propLineOffset
    ]

testEmptyLineMap :: TestTree
testEmptyLineMap = testCase "empty" $ do
    let source = PS.makeSource "<test>" ""
    assertBool "An empty source generates an empty line map."
        $ null (PS.lineOffsets $ PS.sourceLineMap source)

testSimpleLineMap :: TestTree
testSimpleLineMap = testCase "simple" $ do
    let sourceLines =
            [ "This source file contains a few lines."
            , "Some, line the next, are empty."
            , ""
            , "Multiple consecutive empty lines are fine as well."
            , ""
            , ""
            , "終わり"
            ]
        source = PS.makeSource "<test>" $ T.unlines sourceLines
    toList (PS.lineOffsets $ PS.sourceLineMap source)
        @?= fmap PS.Offset [39, 71, 72, 123, 124, 125, 129]
    for_ (zip [PS.Line 0 ..] sourceLines) $ \(lineNumber, line) ->
        PS.getSourceLine lineNumber source @?= Just (line <> "\n")

testLineDelimiters :: TestTree
testLineDelimiters = testCase "endings" $ do
    let source = PS.makeSource "<test>" "abc\ndef\rghi\r\njkl"
        lines  = PS.sourceLineMap source
    PS.getSourceLine (PS.Line 0) source @?= Just "abc\n"
    PS.lineToOffset (PS.Line 0) lines @?= Just (PS.Offset 0)
    PS.getSourceLine (PS.Line 1) source @?= Just "def\r"
    PS.lineToOffset (PS.Line 1) lines @?= Just (PS.Offset 4)
    PS.getSourceLine (PS.Line 2) source @?= Just "ghi\r\n"
    PS.lineToOffset (PS.Line 2) lines @?= Just (PS.Offset 8)
    PS.getSourceLine (PS.Line 3) source @?= Just "jkl"
    PS.lineToOffset (PS.Line 3) lines @?= Just (PS.Offset 13)
    PS.getSourceLine (PS.Line 4) source @?= Nothing

testLocation :: TestTree
testLocation = testCase "location" $ do
    let source = PS.makeSource "<test>" "ab\ncd\nef"
        checkLocation offset line column = do
            let loc = PS.getLocation (PS.Offset offset) source
            fmap PS.locationOffset loc @?= Just (PS.Offset offset)
            fmap PS.locationLine loc @?= Just (PS.Line line)
            fmap PS.locationColumn loc @?= Just (PS.Column column)
    checkLocation 0 0 0
    checkLocation 1 0 1
    checkLocation 2 0 2
    checkLocation 3 1 0
    checkLocation 4 1 1
    checkLocation 5 1 2
    checkLocation 6 2 0
    checkLocation 7 2 1
    PS.getLocation (PS.Offset 8) source @?= Nothing

testLocationLazy :: TestTree
testLocationLazy = testCase "lazy" $ do
    let source   = PS.makeSource "<test>" "abc\ndef"
        Just loc = PS.getLocation (PS.Offset 5) source
    (line, checkLine) <- checkEvaluated (PS.locationLine loc)
    (col , checkCol ) <- checkEvaluated (PS.locationColumn loc)
    assertBool "line is unevaluated" . not =<< checkLine
    assertBool "col is unevaluated" . not =<< checkCol
    _ <- evaluate line
    assertBool "line is evaluated" =<< checkLine
    assertBool "col is unevaluated" . not =<< checkCol
    _ <- evaluate col
    assertBool "line is evaluated" =<< checkLine
    assertBool "col is evaluated" =<< checkCol

propLineOffset :: TestTree
propLineOffset =
    testProperty "line-and-offset" $ forAll gen $ \(source, initialOffset) ->
        let lineMap     = PS.sourceLineMap source
            line        = PS.offsetToLine initialOffset lineMap
            Just offset = PS.lineToOffset line lineMap
            line'       = PS.offsetToLine offset lineMap
        in  (initialOffset >= offset) .&&. (line === line')
  where
    gen = do
        text          <- T.pack <$> genChar
        initialOffset <-
            elements
                [PS.Offset 0 .. toEnum
                    (if T.null text then 0 else T.length text - 1)]
        pure (PS.makeSource "<text>" text, initialOffset)
    genChar =
        listOf $ frequency
            [(10, elements ['a' .. 'z']), (4, pure ' '), (1, pure '\n')]

{-# NOINLINE checkEvaluated #-}
checkEvaluated :: a -> IO (a, IO Bool)
checkEvaluated val = do
    ref <- newIORef False
    pure (unsafePerformIO (writeIORef ref True) `seq` val, readIORef ref)

import Data.Maybe
import System.Environment
import System.Process

main =
  do
    ghc <- readGHC <$> getEnv "ghc"
    callProcess "cabal" $ "build" : "all" : constraints ghc
    callProcess "cabal" $ "test" : "all" : "--enable-tests" : constraints ghc

x .= Just y  = Just ("--constraint=" ++ x ++ "==" ++ y)
x .= Nothing = Nothing

data GHC = GHC_8_6 | GHC_8_8 | GHC_8_10

readGHC s = case s of
    "8.6"  -> GHC_8_6
    "8.8"  -> GHC_8_8
    "8.10" -> GHC_8_10

constraints ghc = catMaybes
    [ "base" .= case ghc of
          GHC_8_6  -> Just "4.12.*"
          GHC_8_8  -> Just "4.13.*"
          GHC_8_10 -> Just "4.14.*"
    , "aeson" .= case ghc of
          GHC_8_10 -> Just "1.5.6.0"
          _        -> Nothing
    , "megaparsec" .= case ghc of
          GHC_8_10 -> Just "9.0.1"
          _        -> Nothing
    , "prettyprinter" .= case ghc of
          GHC_8_10 -> Just "1.7.0"
          _        -> Nothing
    , "profunctors" .= case ghc of
          GHC_8_10 -> Just "5.6.2"
          _        -> Nothing
    ]

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

data GHC = GHC_8_6 | GHC_8_8 | GHC_8_10 | GHC_9_0

readGHC s = case s of
    "8.6"  -> GHC_8_6
    "8.8"  -> GHC_8_8
    "8.10" -> GHC_8_10
    "9.0"  -> GHC_9_0

constraints ghc = catMaybes
    [ "base" .= case ghc of
          GHC_8_6  -> Just "4.12.*"
          GHC_8_8  -> Just "4.13.*"
          GHC_8_10 -> Just "4.14.*"
          GHC_9_0  -> Just "4.15.*"
    , "aeson" .= case ghc of
          GHC_8_6  -> Just "1.4.*"
          GHC_8_8  -> Just "1.5.*"
          GHC_8_10 -> Just "2.0.*"
          _        -> Nothing
    , "base-compat-batteries" .= case ghc of
          GHC_8_8  -> Just "0.11.*"
          GHC_9_0  -> Just "0.12.*"
          _        -> Nothing
    , "hashable" .= case ghc of
          GHC_8_8  -> Just "1.3.*"
          GHC_8_10 -> Just "1.4.*"
          _        -> Nothing
    , "megaparsec" .= case ghc of
          GHC_8_6  -> Just "7.0.*"
          GHC_8_10 -> Just "9.0.1"
          GHC_9_0  -> Just "9.2.*"
          _        -> Nothing
    , "prettyprinter" .= case ghc of
          GHC_8_6  -> Just "1.6.*"
          GHC_8_10 -> Just "1.7.*"
          _        -> Nothing
    , "profunctors" .= case ghc of
          GHC_8_6  -> Just "5.3.*"
          GHC_8_10 -> Just "5.6.2"
          _        -> Nothing
    , "tasty" .= case ghc of
          GHC_8_6  -> Just "1.2.*"
          GHC_8_10 -> Just "1.3.*"
          GHC_9_0  -> Just "1.4.*"
          _        -> Nothing
    ]

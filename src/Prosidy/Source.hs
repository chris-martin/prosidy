{-|
Module      : Prosidy.Source
Description : Utilities for tracking source locaitons.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Safe #-}
module Prosidy.Source
    ( Source(..)
    , Location
    , SparseLocation(..)
    , LineMap
    , Offset(..)
    , Line(..)
    , Column(..)
    , locationSource
    , locationColumn
    , locationLine
    , locationOffset
    , makeSource
    , getSourceLine
    , getLocation
    , enrichLocation
    , stripLocation
    , lineOffsets
    , lineToOffset
    , offsetToLine
    )
where

import           Data.Text                      ( Text )
import           Control.Monad                  ( guard )

import qualified Data.Text                     as T
import qualified Data.Text.Prettyprint.Doc     as PP

import           Prosidy.Internal.Classes
import           Prosidy.Source.LineMap
import           Prosidy.Source.Units

-- | Information about Prosidy source file.
--
-- The 'Show' instance for ths class does not include the 'LineMap' or 'Text'
-- fields, as those are rather noisy.
data Source = Source
  { sourceName    :: String
    -- ^ The reported file-name of the 'Source'.
    --
    -- When read from file handles, a non-filepath description such as
    -- @"\<stdin\>"@ is typically chosen.
    -- This field doesn't have semantic meaning, and should only be used to
    -- enrich the output displayed to users.
  , sourceText    :: Text
    -- ^ The full source, as 'Text'.
  , sourceLineMap :: LineMap
    -- ^ A mapping of the start position of each line in the 'Source'.
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable, NFData, Binary)

instance Show Source where
    show (Source fp _ _) = "Source " <> show fp

instance Pretty Source where
    pretty = pretty . sourceName

-- | Create a 'Source' from a descriptive name and a body. The source name is
-- typically a 'FilePath', but this is not guarenteed. For instance, when read 
-- from standard-input, Prosidy chooses to name the source @\<stdin\>@.
makeSource :: String -> Text -> Source
makeSource name body = Source name body lineMap
  where
    lineMap = case T.foldl' lineMapFold (1, '\0', []) $ body of
        (_, _, acc) -> fromOffsets acc
    lineMapFold (ix, prev, acc) ch
        | ch == '\n' && prev == '\r' = (succ ix, ch, Offset ix : drop 1 acc)
        | ch == '\n' || ch == '\r'   = (succ ix, ch, Offset ix : acc)
        | otherwise                  = (succ ix, ch, acc)

-- | Convert an 'Offset' into a 'Location'.
getLocation :: Offset -> Source -> Maybe Location
getLocation offset src = do
    guard $ T.length (sourceText src) > fromEnum offset
    Just . enrichLocation $ SparseLocation src offset

-- | Fetch a single line from a source.
getSourceLine :: Line -> Source -> Maybe Text
getSourceLine line source = do
    start <- fromEnum <$> lineToOffset line lineMap
    let end = fromEnum <$> lineToOffset (succ line) lineMap
    Just . maybe id (T.take . (flip (-) start)) end . T.drop start $ sourceText
        source
    where lineMap = sourceLineMap source

-- | A location in a 'Source'. The line and column numbers of this type are not
-- attached to this type; convert to a 'Location' to access those values.
data SparseLocation = SparseLocation
    { sparseLocationSource :: Source
      -- ^ The 'Source' this location references.
    , sparseLocationOffset :: Offset
      -- ^ The position in the 'Source', counted by Unicode codepoints.
    }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (NFData, Binary, Hashable)

-- | A location in a 'Source', with the line and column number computed lazily.
data Location = Location
    { locationSource :: Source
      -- ^ The 'Source' this location references.
    , locationOffset :: Offset
      -- ^ The position in the 'Source', counted by Unicode codepoints.
    , locationLine   :: ~Line
      -- ^ The line number in the 'Source'.
    , locationColumn :: ~Column
      -- ^ The column number in the 'Source'.
    }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (NFData, Binary, Hashable)

instance Pretty Location where
    pretty loc = pretty (locationSource loc) PP.<+> "@" PP.<+> mconcat
        [pretty (locationLine loc), "×", pretty (locationColumn loc)]

-- | Add lazily computed line and column number information to a 
-- 'SparseLocation'.
enrichLocation :: SparseLocation -> Location
enrichLocation sl = Location { locationSource = source
                             , locationOffset = offset
                             , locationLine   = line
                             , locationColumn = column
                             }
  where
    source                     = sparseLocationSource sl
    lineMap                    = sourceLineMap source
    offset@(~(Offset offsetN)) = sparseLocationOffset sl
    line                       = offsetToLine offset lineMap
    column                     = case lineToOffset line lineMap of
        Just (Offset n) -> Column (offsetN - n)
        Nothing         -> Column 0

-- | Remove line and column number information from a 'Location'.
stripLocation :: Location -> SparseLocation
stripLocation l = SparseLocation { sparseLocationSource = locationSource l
                                 , sparseLocationOffset = locationOffset l
                                 }

{-|
Module      : Prosidy.Source
Description : Utilities for tracking source locaitons.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
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

import           Data.Hashable                  ( Hashable(..) )
import           Data.Vector.Unboxed            ( Vector
                                                , MVector
                                                , Unbox
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )
import           Data.Binary                    ( Binary(..) )
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                )
import           Control.Monad                  ( guard )

import qualified Data.Text                     as T
import qualified Data.Vector.Unboxed           as V
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM

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

-- | Create a 'Source' from a descriptive name and a body. The source name is
-- typically a 'FilePath', but this is not guarenteed. For instance, when read 
-- from standard-input, Prosidy chooses to name the source @\<stdin\>@.
makeSource :: String -> Text -> Source
makeSource name body = Source name body lineMap
  where
    lineMap = case T.foldl' lineMapFold (1, '\0', []) $ body of
        (_, _, acc) -> LineMap . V.fromList . reverse $ acc
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

-- | A dense vector containing offsets poiting to the start of each line. That
-- is, the starting position of the third line of a file can be found at
-- position 2.
newtype LineMap = LineMap (Vector Offset)
  deriving stock (Eq, Generic)
  deriving newtype (Show, NFData)

instance Binary LineMap where
    get = fmap (LineMap . V.fromList) get
    put (LineMap v) = put (V.toList v)

instance Hashable LineMap where
    hashWithSalt salt (LineMap v) = V.foldl' hashWithSalt salt v

-- | Convert a 'LineMap' into a list of 'Offset's, corresponding to the first
-- character of a line. Note that the initial offset is omitted-- the offset at
-- index 0 will be the offset of the /second/ line.
lineOffsets :: LineMap -> [Offset]
lineOffsets (LineMap v) = V.toList v

-- | Fetch the 'Offset' for the given 'Line'. Evaluates to 'Nothing' if the
-- given 'Line' does not appear in the LineMap
lineToOffset :: Line -> LineMap -> Maybe Offset
lineToOffset (Line 0  ) _            = Just $ Offset 0
lineToOffset (Line nth) (LineMap xs) = xs V.!? fromIntegral (pred nth)

-- | Fetch the 'Line' number for a given 'Offset'. Newlines will be attributed
-- the line that they terminate, rather than the line started immediately 
-- afterwards.
offsetToLine :: Offset -> LineMap -> Line
offsetToLine offset (LineMap xs) = Line . fromIntegral $ go Nothing
                                                            0
                                                            (V.length xs)
  where
    go result min max
        | min >= max
        = maybe 0 succ result
        | otherwise
        = let nthIndex  = ((max - min) `div` 2) + min
              nthOffset = xs V.! nthIndex
          in  case nthOffset `compare` offset of
                  EQ -> succ nthIndex
                  LT -> go (Just nthIndex) (nthIndex + 1) max
                  GT -> go result min nthIndex

-- | A line number.
--
-- The 'Show' instance for 'Line' counts from one, while the internal
-- implementation counts from zero.
newtype Line = Line Word
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (ToJSON, FromJSON, Enum)
  deriving anyclass (Hashable, NFData, Binary)

-- | A column number.
newtype Column = Column Word
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (ToJSON, FromJSON, Enum)
  deriving anyclass (Hashable, NFData, Binary)

-- | An offset into a 'Source', counted by UTF-8 codepoint.
newtype Offset = Offset Word
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Enum)
  deriving anyclass (Hashable, NFData, Binary)

newtype instance MVector s Offset = MV_Offset (MVector s Word)

instance VGM.MVector MVector Offset where
    basicLength (MV_Offset m) = VGM.basicLength m
    {-# INLINE basicLength #-}

    basicUnsafeSlice ix len (MV_Offset m) =
        MV_Offset $ VGM.basicUnsafeSlice ix len m
    {-# INLINE basicUnsafeSlice #-}

    basicOverlaps (MV_Offset x) (MV_Offset y) = VGM.basicOverlaps x y
    {-# INLINE basicOverlaps #-}

    basicUnsafeNew len = MV_Offset <$> VGM.basicUnsafeNew len
    {-# INLINE basicUnsafeNew #-}

    basicInitialize (MV_Offset v) = VGM.basicInitialize v
    {-# INLINE basicInitialize #-}

    basicUnsafeRead (MV_Offset v) = fmap Offset <$> VGM.basicUnsafeRead v
    {-# INLINE basicUnsafeRead #-}

    basicUnsafeWrite (MV_Offset v) ix (Offset w) = VGM.basicUnsafeWrite v ix w
    {-# INLINE basicUnsafeWrite #-}

newtype instance Vector Offset = V_Offset (Vector Word)

instance VG.Vector Vector Offset where
    basicUnsafeFreeze (MV_Offset v) = V_Offset <$> VG.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}

    basicUnsafeThaw (V_Offset v) = MV_Offset <$> VG.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}

    basicLength (V_Offset v) = VG.basicLength v
    {-# INLINE basicLength #-}

    basicUnsafeSlice ix len (V_Offset v) =
        V_Offset $ VG.basicUnsafeSlice ix len v
    {-# INLINE basicUnsafeSlice #-}

    basicUnsafeIndexM (V_Offset v) ix = Offset <$> VG.basicUnsafeIndexM v ix
    {-# INLINE basicUnsafeIndexM #-}

instance Unbox Offset where

{- |
Module      : Prosidy.Source.LineMap
Description : Binary-search tree for finding the position of new lines.
Copyright   : (c) James Alexander Feldman-Crough, 2019
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
module Prosidy.Source.LineMap
    ( LineMap
    , lineOffsets
    , lineToOffset
    , offsetToLine
    , fromOffsets
    )
where

import qualified Data.Vector.Unboxed           as V
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import           Data.Vector.Unboxed            ( Vector
                                                , MVector
                                                , Unbox
                                                )

import           Data.Foldable
import           Data.List                      ( sort )

import           Prosidy.Internal.Classes
import           Prosidy.Source.Units

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

fromOffsets :: Foldable f => f Offset -> LineMap
fromOffsets = LineMap . V.fromList . sort . toList

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

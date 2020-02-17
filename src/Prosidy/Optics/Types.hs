{-|
Module      : Prosidy.Optics.Types
Description : Optics for definitions in 'Prosidy.Types'.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
module Prosidy.Optics.Types 
    ( -- * Classy optics
      -- ** Items with 'Metadata' 
      HasMetadata(..)
    , properties
    , settings
    , hasProperty
    , atSetting
      -- ** Items wrapping content
    , HasContent(..)
      -- * Prisms on 'Block' contexts
    , _BlockTag
    , _BlockLiteral
    , _BlockParagraph
      -- * Prisms on 'Inline' contexts
    , _InlineTag
    , _Text
    , _Break
      -- * Optics on common types 
    , key 
    , _Assoc
    , _NonEmpty
    , _Series
    , _SeriesNE
    , _Set
    ) where

import Prosidy.Types
import Prosidy.Types.Assoc (toHashMap, fromHashMap)
import Prosidy.Types.Series (toSeq, fromSeq, toSeqNE, fromSeqNE)
import Prosidy.Types.Set (toHashSet, fromHashSet)
import Prosidy.Optics.Internal

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-------------------------------------------------------------------------------
class HasMetadata t where
    metadata :: Lens' t Metadata

instance HasMetadata Document where
    metadata = lens documentMetadata (\d m -> d{documentMetadata = m})
    {-# INLINE metadata #-}

instance HasMetadata (Tag a) where
    metadata = lens tagMetadata (\d m -> d{tagMetadata = m})
    {-# INLINE metadata #-}

instance HasMetadata (Region a) where
    metadata = lens regionMetadata (\d m -> d{regionMetadata = m})
    {-# INLINE metadata #-}

instance HasMetadata Metadata where
    metadata = id
    {-# INLINE metadata #-}

properties :: HasMetadata m => Lens' m (Set Key)
properties = metadata . lens metadataProperties (\m p -> m{metadataProperties = p})
{-# INLINEABLE properties #-}
{-# SPECIALIZE INLINE properties :: Lens' Metadata   (Set Key) #-}
{-# SPECIALIZE INLINE properties :: Lens' Document   (Set Key) #-}
{-# SPECIALIZE INLINE properties :: Lens' (Tag a)    (Set Key) #-}
{-# SPECIALIZE INLINE properties :: Lens' (Region a) (Set Key) #-}

settings :: HasMetadata m => Lens' m (Assoc Key Text)
settings = metadata . lens metadataSettings (\m s -> m{metadataSettings = s})
{-# INLINABLE settings #-}
{-# SPECIALIZE INLINE settings :: Lens' Metadata   (Assoc Key Text) #-}
{-# SPECIALIZE INLINE settings :: Lens' Document   (Assoc Key Text) #-}
{-# SPECIALIZE INLINE settings :: Lens' (Tag a)    (Assoc Key Text) #-}
{-# SPECIALIZE INLINE settings :: Lens' (Region a) (Assoc Key Text) #-}

hasProperty :: HasMetadata m => Key -> Lens' m Bool
hasProperty k = properties . _Set . lens (HS.member k) 
    (\hs b -> (if b then HS.insert else HS.delete) k hs)
{-# INLINE hasProperty #-}

atSetting :: HasMetadata m => Key -> Lens' m (Maybe Text)
atSetting k = settings . _Assoc . lens (HM.lookup k)
    (\hm x -> maybe (HM.delete k) (HM.insert k) x hm)
{-# INLINE atSetting #-}

-------------------------------------------------------------------------------
class HasContent t where
    type Content t
    content :: Lens' t (Content t)

instance HasContent Document where
    type Content Document = Series Block
    content = lens documentContent (\d c -> d{documentContent = c})
    {-# INLINE content #-}

instance HasContent (Tag a) where
    type Content (Tag a) = a
    content = lens tagContent (\t c -> t{tagContent = c})
    {-# INLINE content #-}

instance HasContent (Region a) where
    type Content (Region a) = a
    content = lens regionContent (\t c -> t{regionContent = c})
    {-# INLINE content #-}

instance HasContent Paragraph where
    type Content Paragraph = SeriesNE Inline
    content = lens paragraphContent (\t c -> t{paragraphContent = c})
    {-# INLINE content #-}

-------------------------------------------------------------------------------
_BlockTag :: Prism' Block BlockTag
_BlockTag = prism' BlockTag $ \case
    BlockTag t -> Just t
    _          -> Nothing

_BlockParagraph :: Prism' Block Paragraph
_BlockParagraph = prism' BlockParagraph $ \case
    BlockParagraph p -> Just p 
    _                -> Nothing

_BlockLiteral :: Prism' Block LiteralTag
_BlockLiteral = prism' BlockLiteral $ \case
    BlockLiteral t -> Just t
    _              -> Nothing

_InlineTag :: Prism' Inline InlineTag
_InlineTag = prism' InlineTag $ \case
    InlineTag t -> Just t
    _           -> Nothing

_Text :: Prism' Inline Text
_Text = prism' InlineText $ \case
    InlineText t -> Just t
    _            -> Nothing

_Break :: Prism' Inline ()
_Break = prism' (const Break) $ \case
    Break -> Just ()
    _     -> Nothing

-------------------------------------------------------------------------------
-- | A Prism from 'Text' into a valid 'Key'. 
key :: Prism' Text Key
key = prism' rawKey (either (const Nothing) Just . makeKey)
{-# INLINE key #-}

-- | An isomorphism between Prosidy's 'Assoc' wrapper and 'HashMap'.
_Assoc :: Iso (Assoc k v) (Assoc k' v') (HashMap k v) (HashMap k' v')
_Assoc = iso toHashMap fromHashMap
{-# INLINE _Assoc #-}

-- | A prism between possibly-empty and non-empty containers.
_NonEmpty :: Prism' (Series a) (SeriesNE a)
_NonEmpty = prism' (fromSeq . toSeqNE) (fromSeqNE . toSeq)
{-# INLINE _NonEmpty #-}

-- | An isomorpism between Prosidy's 'Series' wrapper and 'Seq'.
_Series :: Iso (Series a) (Series b) (Seq a) (Seq b)
_Series = iso toSeq fromSeq
{-# INLINE _Series #-}

-- | A prism from a non-empty 'Seq' into a 'SeriesNE'.
_SeriesNE :: Prism' (Seq a) (SeriesNE a)
_SeriesNE = prism' toSeqNE fromSeqNE
{-# INLINE _SeriesNE #-}

-- | An isomorphism between Prosidy's 'Set' wrapper and 'HashSet'.
_Set :: Iso (Set a) (Set b) (HashSet a) (HashSet b)
_Set = iso toHashSet fromHashSet
{-# INLINE _Set #-}
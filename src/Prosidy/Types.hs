{-|
Module      : Prosidy.Types
Description : A convenience module which reëxports type definitions & helpers. 
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Prosidy.Types 
    ( -- * Documents
      Document(..)
    , documentToRegion
    , regionToDocument
      -- * Tags
    , Tag(..)
    , BlockTag
    , InlineTag
    , LiteralTag
    , tagToRegion
    , regionToTag
      -- * Contextual enumerations
    , Block(..)
    , Inline(..)
      -- * Paragraphs
    , Paragraph(..)
      -- * Common structures
    , Metadata(..)
    , Region(..)
      -- * Utility wrappers
    , module X 
    ) where

import Prosidy.Types.Assoc as X (Assoc(..))
import Prosidy.Types.Key as X (Key, KeyError(..), InvalidCharacter, makeKey, rawKey)
import Prosidy.Types.Series as X (Series(..), SeriesNE)
import Prosidy.Types.Set as X (Set(..))
import Prosidy.Source (Location)

import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.:), (.=), object, pairs)

import qualified Data.Aeson as Aeson

-------------------------------------------------------------------------------
-- | A sum type enumerating allowed types inside of a block context.
data Block =
    BlockLiteral LiteralTag
  | BlockParagraph Paragraph
  | BlockTag BlockTag
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance FromJSON Block where
    parseJSON = withObject "block" $ \o -> do
        ty <- o .: "type"
        case ty :: Text of
            "tag" -> do
                subtype <- o .: "subtype"
                case subtype :: Text of
                    "block"   -> BlockTag <$> o .: "value"
                    "literal" -> BlockLiteral <$> o .: "value"
                    _         -> fail $ "unknown tag subtype: " <> show subtype
            "paragraph" -> BlockParagraph <$> o .: "value"
            _ -> fail $ "unknown block type: " <> show ty

instance ToJSON Block where
    toEncoding b = pairs . mconcat $ case b of
        BlockLiteral t -> 
            [ "type" .= ("tag" :: Text)
            , "subtype" .= ("literal" :: Text) 
            , "value" .= t 
            ]
        BlockParagraph p ->
            [ "type" .= ("paragraph" :: Text)
            , "value" .= p
            ]
        BlockTag t -> 
            [ "type" .= ("tag" :: Text)
            , "subtype" .= ("block" :: Text) 
            , "value" .= t 
            ]

    toJSON b = object $ case b of
        BlockLiteral t -> 
            [ "type" .= ("tag" :: Text)
            , "subtype" .= ("literal" :: Text) 
            , "value" .= t 
            ]
        BlockParagraph p ->
            [ "type" .= ("paragraph" :: Text)
            , "value" .= p
            ]
        BlockTag t -> 
            [ "type" .= ("tag" :: Text)
            , "subtype" .= ("block" :: Text) 
            , "value" .= t 
            ]

-------------------------------------------------------------------------------
-- | A full Prosidy document.
data Document = Document
    { documentMetadata :: Metadata
    , documentContent  :: Series Block
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData, Binary)

instance FromJSON Document where
    parseJSON = withObject "Document" $ \o -> Document
        <$> o .: "metadata"
        <*> o .: "content"

instance ToJSON Document where
    toEncoding (Document md ct) = pairs $ mconcat
        [ "metadata" .= md
        , "content"  .= ct
        ]

    toJSON (Document md ct) = object
        [ "metadata" .= md
        , "content"  .= ct
        ]

-- | Convert a 'Document' to a 'Region'. The resulting 'Region' will never have
-- a 'Location' attached. 
documentToRegion :: Document -> Region (Series Block)
documentToRegion (Document md ct) = Region md ct Nothing

-- | Convert a 'Region' to a 'Document'. Any 'Location' on the 'Region' will be
-- discarded.
regionToDocument :: Region (Series Block) -> Document
regionToDocument (Region md ct _) = Document md ct

-------------------------------------------------------------------------------
-- | A sum type enumerating allowed types inside of an inline context.
data Inline =
    Break
    -- ^ Spacing recorded between lines or on either side of an 'Inline' 'Tag'.
    -- Although we could represent this as 'Text', Prosidy defines a special
    -- node for this case so that authors in CJK languages (or other languages
    -- without explicit spaces between words) may simply ignore these spaces
    -- in their output.
  | InlineTag  InlineTag
    -- ^ A 'Tag' which contains only 'Inline' items. These tags begin with the
    -- @#@ sigil in source.
  | InlineText Text
    -- ^ A fragment of plain text.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance FromJSON Inline where
    parseJSON = withObject "Inline" $ \o -> do
        ty <- o .: "type"
        case ty :: Text of
            "break" -> pure Break
            "tag"   -> InlineTag  <$> o .: "value"
            "text"  -> InlineText <$> o .: "value"
            _       -> fail $ "unknown inline type: " <> show ty

instance ToJSON Inline where
    toEncoding i = pairs . mconcat $ case i of
        Break        -> 
            [ "type" .= ("break" :: Text)
            , "value" .= Aeson.Null
            ]
        InlineTag  t -> 
            [ "type"    .= ("tag" :: Text)
            , "subtype" .= ("inline" :: Text)
            , "value"   .= t
            ]
        InlineText t -> 
            [ "type"  .= ("text" :: Text) 
            , "value" .= t
            ]

    toJSON i = object $ case i of
        Break        -> 
            [ "type"  .= ("break" :: Text)
            ]
        InlineTag  t -> 
            [ "type"    .= ("tag" :: Text)
            , "subtype" .= ("inline" :: Text)
            , "value"   .= t
            ]
        InlineText t -> 
            [ "type"  .= ("text" :: Text) 
            , "value" .= t
            ]

-------------------------------------------------------------------------------
-- | A set of properties and settings, associated with a 'Region'. 
--
-- The namespaces of properties and settings are distinct; a property can share
-- a name with a setting without conflict.
data Metadata = Metadata
    { metadataProperties :: Set Key 
      -- ^ Properties are a set of 'Key's with no associated value.
    , metadataSettings   :: Assoc Key Text
      -- ^ Settings are 'Key's with an attached value.
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Binary, Hashable)

instance Monoid Metadata where
    mempty = Metadata mempty mempty

instance Semigroup Metadata where
    Metadata p1 s1 <> Metadata p2 s2 =
        Metadata (p1 <> p2) (s1 <> s2)

instance FromJSON Metadata where
    parseJSON = withObject "Metadata" $ \o -> Metadata
        <$> o .: "properties"
        <*> o .: "settings"

instance ToJSON Metadata where
    toEncoding (Metadata ps ss) = pairs $ mconcat
        [ "properties" .= ps 
        , "settings"   .= ss
        ]

    toJSON (Metadata ps ss) = object
        [ "properties" .= ps 
        , "settings"   .= ss
        ]

-------------------------------------------------------------------------------
-- | A non-empty collection of 'Inline' items. A 'Paragraph' represents the
-- border between block and inline contexts. All ancestors of a paragraph are
-- block items or a document, and all children are inline items.
data Paragraph = Paragraph 
    { paragraphContent  :: SeriesNE Inline
    , paragraphLocation :: Maybe Location
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData, Binary)

instance FromJSON Paragraph where
    parseJSON = fmap (flip Paragraph Nothing) . parseJSON

instance ToJSON Paragraph where
    toEncoding (Paragraph s _) = toEncoding s
    toJSON     (Paragraph s _) = toJSON s

-------------------------------------------------------------------------------
-- | An untagged structural grouping of items with type @a@. Regions do not
-- occur in parsing.
data Region a = Region
    { regionMetadata :: Metadata
    , regionContent  :: a
    , regionLocation :: Maybe Location
    }
  deriving stock (Eq, Foldable, Functor, Show, Traversable, Generic)
  deriving anyclass (Hashable, NFData, Binary)

instance ToJSON a => ToJSON (Region a) where
    toJSON (Region md ct _) = Aeson.object
        [ "metadata" .= md
        , "content"  .= ct
        ]

-------------------------------------------------------------------------------
-- | A 'Region', annotated with a tag name.
data Tag a = Tag
    { tagName     :: Key
    , tagMetadata :: Metadata
    , tagContent  :: a
    , tagLocation :: Maybe Location
    }
  deriving stock (Eq, Foldable, Functor, Show, Traversable, Generic)
  deriving anyclass (Hashable, NFData, Binary)

instance FromJSON a => FromJSON (Tag a) where
    parseJSON = withObject "Tag" $ \o -> Tag 
        <$> o .: "name"
        <*> o .: "metadata"
        <*> o .: "content"
        <*> pure Nothing

instance ToJSON a => ToJSON (Tag a) where
    toEncoding (Tag nm md ct _) = pairs $ mconcat
        [ "name"     .= nm
        , "metadata" .= md
        , "content"  .= ct
        ]

    toJSON (Tag nm md ct _) = object
        [ "name"     .= nm
        , "metadata" .= md
        , "content"  .= ct
        ]

-- | A 'Tag' containing zero or more 'Block' items. 
-- Specified in Prosidy source with the @#-@ sigil.
type BlockTag   = Tag (Series Block)

-- | A 'Tag' containing zero or more 'Inline' items.
-- Specified in Prosidy source with the @#@ sigil.
type InlineTag  = Tag (Series Inline)

-- | A 'Tag' containing a single plain-text item.
-- Specified in Prosidy source with the @#=@ sigil.
type LiteralTag = Tag Text

-- | Convert a 'Tag' to a 'Region' by discarding the tag's name.
tagToRegion :: Tag a -> Region a
tagToRegion (Tag _ md ct loc) = Region md ct loc

-- | Convert a 'Region' to a 'Tag' by providing a tag name.
regionToTag :: Key -> Region a -> Tag a
regionToTag name (Region md ct loc) = Tag name md ct loc
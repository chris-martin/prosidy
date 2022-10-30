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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
      -- * Regions
    , Region(..)
    , BlockRegion
    , InlineRegion
    , LiteralRegion
      -- * Contextual enumerations
    , Block(..)
    , Inline(..)
      -- * Paragraphs
    , Paragraph(..)
      -- * Common structures
    , Metadata(..)
      -- * Textual fragments
    , Fragment(..)
      -- * Utility wrappers
    , module X
    )
where

import           Prosidy.Internal.Classes

import           Prosidy.Types.Assoc           as X
                                                ( Assoc(..) )
import           Prosidy.Types.Key             as X
                                                ( Key
                                                , KeyError(..)
                                                , InvalidCharacter
                                                , makeKey
                                                , rawKey
                                                )
import           Prosidy.Types.Series          as X
                                                ( Series(..)
                                                , SeriesNE
                                                )
import           Prosidy.Types.Set             as X
                                                ( Set(..) )
import           Prosidy.Source                 ( Location )

import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc      ( (<+>) )
import           Data.Foldable                  ( toList )
import           Prosidy.Types.Assoc            ( toEntries )
import qualified Data.Text.Prettyprint.Doc     as PP

-------------------------------------------------------------------------------
-- | A sum type enumerating allowed types inside of a block context.
data Block =
    BlockLiteral LiteralTag
  | BlockParagraph Paragraph
  | BlockTag BlockTag
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance Pretty Block where
    pretty (BlockLiteral   lit) = pretty lit
    pretty (BlockParagraph pp ) = pretty pp
    pretty (BlockTag       tag) = pretty tag

-------------------------------------------------------------------------------
-- | A full Prosidy document.
data Document = Document
    { documentMetadata :: Metadata
    , documentContent  :: Series Block
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData, Binary)

instance Pretty Document where
    pretty (Document md ct) = PP.nest 4 . PP.vsep $ [pretty md, pretty ct]

-- | Convert a 'Document' to a 'Region'. The resulting 'Region' will never have
-- a 'Location' attached.
documentToRegion :: Document -> Region (Series Block)
documentToRegion (Document md ct) = Region md ct Nothing

-- | Convert a 'Region' to a 'Document'. Any 'Location' on the 'Region' will be
-- discarded.
regionToDocument :: Region (Series Block) -> Document
regionToDocument (Region md ct _) = Document md ct

-------------------------------------------------------------------------------
-- | Plain text, possibly annotated with a 'Location'.
data Fragment = Fragment
  { fragmentText     :: Text
    -- ^ Access the underlying 'Text'.
  , fragmentLocation :: Maybe Location
    -- ^ The location of the 'Text' in the source code.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance Pretty Fragment where
    pretty = pretty . fragmentText

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
  | InlineText Fragment
    -- ^ A fragment of plain text.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance Pretty Inline where
    pretty Break            = "\9248"
    pretty (InlineTag  tag) = pretty tag
    pretty (InlineText f  ) = pretty f

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

instance Pretty Metadata where
    pretty (Metadata props sets)
        | null props && null sets
        = "∅"
        | otherwise
        = let props' = fmap pretty . toList $ props
              sets' =
                      fmap (\(k, v) -> pretty k <+> PP.equals <+> pretty v)
                          . toEntries
                          $ sets
          in  PP.list $ props' ++ sets'

instance Semigroup Metadata where
    Metadata p1 s1 <> Metadata p2 s2 = Metadata (p1 <> p2) (s1 <> s2)

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

instance Pretty Paragraph where
    pretty pg = "¶" PP.<+> pretty (paragraphContent pg)

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

instance Pretty a => Pretty (Region a) where
    pretty (Region md ct _) = PP.nest 4 $ PP.vsep ["§", pretty md, pretty ct]

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

instance Pretty a => Pretty (Tag a) where
    pretty (Tag name md ct _) =
        PP.nest 4 $ PP.vsep [pretty name, pretty md, pretty ct]

-- | A 'Tag' containing zero or more 'Block' items.
-- Specified in Prosidy source with the @#-@ sigil.
type BlockTag = Tag (Series Block)

-- | A 'Region' containing a zero or more 'Block' items. Like 'BlockTag',
-- without a tag name.
type BlockRegion = Region (Series Block)

-- | A 'Tag' containing zero or more 'Inline' items.
-- Specified in Prosidy source with the @#@ sigil.
type InlineTag = Tag (Series Inline)

-- | A 'Region' containing a zero or more 'Inline' items. Like 'InlineTag',
-- without a tag name.
type InlineRegion = Region (Series Inline)

-- | A 'Tag' containing a single plain-text item.
-- Specified in Prosidy source with the @#=@ sigil.
type LiteralTag = Tag Text

-- | A 'Region' containing a single plain-text item. Like 'LiteralTag', without
-- a tag name.
type LiteralRegion = Region Text

-- | Convert a 'Tag' to a 'Region' by discarding the tag's name.
tagToRegion :: Tag a -> Region a
tagToRegion (Tag _ md ct loc) = Region md ct loc

-- | Convert a 'Region' to a 'Tag' by providing a tag name.
regionToTag :: Key -> Region a -> Tag a
regionToTag name (Region md ct loc) = Tag name md ct loc

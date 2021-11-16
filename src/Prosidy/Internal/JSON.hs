{- |
Module      : Prosidy.Internal.JSON
Description : Orphan JSON instances to let as many modules be -XSafe as possible.
Copyright   : (c) James Alexander Feldman-Crough, 2019
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module Prosidy.Internal.JSON () where

import           Prosidy.Internal.Classes
import           Prosidy.Types
import Data.Aeson
    ( pairs,
      (.:),
      withObject,
      withText,
      object,
      FromJSONKeyFunction(FromJSONKeyTextParser),
      Value(Null),
      KeyValue((.=)) )
import qualified Data.HashMap.Strict           as HM
import           Data.Text                      ( Text )
import           Control.Exception              ( displayException )

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
            _           -> fail $ "unknown block type: " <> show ty

instance ToJSON Block where
    toEncoding b = pairs . mconcat $ case b of
        BlockLiteral t ->
            [ "type" .= ("tag" :: Text)
            , "subtype" .= ("literal" :: Text)
            , "value" .= t
            ]
        BlockParagraph p -> ["type" .= ("paragraph" :: Text), "value" .= p]
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
        BlockParagraph p -> ["type" .= ("paragraph" :: Text), "value" .= p]
        BlockTag t ->
            [ "type" .= ("tag" :: Text)
            , "subtype" .= ("block" :: Text)
            , "value" .= t
            ]

instance FromJSON Document where
    parseJSON = withObject "Document"
        $ \o -> Document <$> o .: "metadata" <*> o .: "content"

instance ToJSON Document where
    toEncoding (Document md ct) =
        pairs $ mconcat ["metadata" .= md, "content" .= ct]

    toJSON (Document md ct) = object ["metadata" .= md, "content" .= ct]

instance FromJSON Fragment where
    parseJSON = withText "Fragment" $ pure . flip Fragment Nothing

instance ToJSON Fragment where
    toEncoding = toEncoding . fragmentText
    toJSON     = toJSON . fragmentText

instance FromJSON Inline where
    parseJSON = withObject "Inline" $ \o -> do
        ty <- o .: "type"
        case ty :: Text of
            "break" -> pure Break
            "tag"   -> InlineTag <$> o .: "value"
            "text"  -> InlineText <$> o .: "value"
            _       -> fail $ "unknown inline type: " <> show ty

instance ToJSON Inline where
    toEncoding i = pairs . mconcat $ case i of
        Break -> ["type" .= ("break" :: Text), "value" .= Null]
        InlineTag t ->
            [ "type" .= ("tag" :: Text)
            , "subtype" .= ("inline" :: Text)
            , "value" .= t
            ]
        InlineText t -> ["type" .= ("text" :: Text), "value" .= t]

    toJSON i = object $ case i of
        Break -> ["type" .= ("break" :: Text)]
        InlineTag t ->
            [ "type" .= ("tag" :: Text)
            , "subtype" .= ("inline" :: Text)
            , "value" .= t
            ]
        InlineText t -> ["type" .= ("text" :: Text), "value" .= t]

instance FromJSON Metadata where
    parseJSON = withObject "Metadata"
        $ \o -> Metadata <$> o .: "properties" <*> o .: "settings"

instance ToJSON Metadata where
    toEncoding (Metadata ps ss) =
        pairs $ mconcat ["properties" .= ps, "settings" .= ss]

    toJSON (Metadata ps ss) = object ["properties" .= ps, "settings" .= ss]

instance FromJSON Paragraph where
    parseJSON = fmap (flip Paragraph Nothing) . parseJSON

instance ToJSON Paragraph where
    toEncoding (Paragraph s _) = toEncoding s
    toJSON (Paragraph s _) = toJSON s

instance ToJSON a => ToJSON (Region a) where
    toJSON (Region md ct _) = object ["metadata" .= md, "content" .= ct]

instance FromJSON a => FromJSON (Tag a) where
    parseJSON = withObject "Tag" $ \o ->
        Tag
            <$> o
            .:  "name"
            <*> o
            .:  "metadata"
            <*> o
            .:  "content"
            <*> pure Nothing

instance ToJSON a => ToJSON (Tag a) where
    toEncoding (Tag nm md ct _) =
        pairs $ mconcat ["name" .= nm, "metadata" .= md, "content" .= ct]

    toJSON (Tag nm md ct _) =
        object ["name" .= nm, "metadata" .= md, "content" .= ct]

instance FromJSONKey Key where
    fromJSONKey =
        FromJSONKeyTextParser $ either (fail . displayException) pure . makeKey

instance (Hashable a, Eq a, ToJSONKey a) => ToJSON (Set a) where
    toJSON (Set hs) = toJSON $ foldMap (flip HM.singleton True) hs
    toEncoding (Set hs) = toEncoding $ foldMap (flip HM.singleton True) hs

instance (Hashable a, Eq a, FromJSONKey a) => FromJSON (Set a) where
    parseJSON json = do
        m <- parseJSON json
        pure . Set . HM.keysSet $ HM.filter id m

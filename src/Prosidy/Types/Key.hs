{-|
Module      : Prosidy.Types.Key
Description : Definitions and helpers for 'Key'.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Prosidy.Types.Key
    ( -- * The 'Key' type.
      Key
      -- * Creating 'Key's and unwrapping them
    , makeKey
    , rawKey
    , unsafeMakeKey
      -- * Checking validity of raw text.
    , isValidKeyHead
    , isValidKeyTail
      -- * Errors
    , KeyError(..)
    , InvalidCharacter(..)
    )
where

import           Prosidy.Internal.Classes

import           Data.Text                      ( Text )
import           Data.String                    ( IsString(..) )
import           Data.Foldable                  ( for_ )
import           Control.Monad                  ( unless )
import           Control.Exception              ( Exception(..)
                                                , throw
                                                )

import qualified Data.Char                     as Char
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text

-- | A 'Key' is an identifier used in tags, properties, and setting names.
newtype Key = Key Text
  deriving stock (Show, Generic)
  deriving (Binary, Eq, Hashable, NFData, Ord, ToJSON, ToJSONKey) via Text

-- | 'Key' exposes an 'IsString' instance, but beware! Invalid strings will
-- throw a pure exception.
instance IsString Key where
    fromString = either throw id . makeKey . Text.pack

instance FromJSON Key where
    parseJSON json = do
        text <- parseJSON json
        either (fail . displayException) pure $ makeKey text

instance Pretty Key where
    pretty = pretty . rawKey

-- | Create a new 'Key', checking its validity.
makeKey :: Text -> Either KeyError Key
makeKey rawText = case Text.unpack rawText of
    [] -> Left EmptyKeyError
    keyHead : keyTail
        | isValidKeyHead keyHead -> do
            for_ (zip [1 ..] keyTail) $ \(ix, ch) ->
                unless (isValidKeyTail ch)
                    $ Left
                    . InvalidCharacterError
                    $ InvalidCharacter rawText ix ch
            Right $ Key rawText
        | otherwise -> Left . InvalidCharacterError $ InvalidCharacter
            rawText
            0
            keyHead

-- | Create a new 'Key' /without/ performing any checks.
unsafeMakeKey :: Text -> Key
unsafeMakeKey = Key
{-# INLINE unsafeMakeKey #-}

-- | Convert a 'Key' into its 'Text' representation.
rawKey :: Key -> Text
rawKey (Key key) = key

-- | Check if a character is suitable for use as the first character in a
-- 'Key'.
isValidKeyHead :: Char -> Bool
isValidKeyHead = (||) <$> Char.isAlphaNum <*> (== '_')

-- | Check if a character is suitable for use as any character except the
-- first character in a 'Key'.
isValidKeyTail :: Char -> Bool
isValidKeyTail = not . invalid
  where
    invalid  = (||) <$> Char.isSpace <*> (`Set.member` reserved)
    reserved = Set.fromList "\\#{}[]:=,"

-- | Errors returned when creating invalid keys.
data KeyError =
    InvalidCharacterError InvalidCharacter
    -- ^ A character provided as a 'Key'\'s name was invalid.
  | EmptyKeyError
    -- ^ A string of length 0 was provided as a 'Key'\'s name.
  deriving (Show, Eq)

-- | Details for errors thrown when creating 'Key's with one or more invalid
-- characters.
data InvalidCharacter = InvalidCharacter
    { -- | The full string provided as the 'Key's name.
      invalidCharacterText :: Text
      -- | The position of the invalid character.
    , invalidCharacterPosition :: Word
      -- | The exact character that was invalid.
    , invalidCharacterCharacter :: Char
    }
  deriving (Show, Eq)

instance Exception KeyError where
    displayException EmptyKeyError =
        "Cannot create a Key with a length of zero."
    displayException (InvalidCharacterError (InvalidCharacter text nth ch)) =
        unwords
            [ "Cannot create a Key named " <> show text <> ":"
            , "the character"
            , show ch
            , "at index"
            , show nth
            , "is not allowed."
            ]

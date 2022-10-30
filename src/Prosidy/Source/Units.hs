{- |
Module      : Prosidy.Source.Units
Description : Positional units for marking source-code locations.
Copyright   : (c) James Alexander Feldman-Crough, 2019
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Prosidy.Source.Units (Line(..), Column(..), Offset(..)) where

import           Prosidy.Internal.Classes

-- | A line number.
--
-- The 'Show' instance for 'Line' counts from one, while the internal
-- implementation counts from zero.
newtype Line = Line Word
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable, NFData, Binary)
  deriving (ToJSON, FromJSON, Enum) via Word

instance Pretty Line where
    pretty (Line n) = pretty $ succ n

-- | A column number.
newtype Column = Column Word
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable, NFData, Binary)
  deriving (ToJSON, FromJSON, Enum) via Word

instance Pretty Column where
    pretty (Column n) = pretty $ succ n

-- | An offset into a 'Source', counted by UTF-8 codepoint.
newtype Offset = Offset Word
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, NFData, Binary)
  deriving (ToJSON, FromJSON, Enum) via Word

instance Pretty Offset where
    pretty (Offset n) = "+" <> pretty n

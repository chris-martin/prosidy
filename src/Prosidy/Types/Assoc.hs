{-|
Module      : Prosidy.Types.Assoc
Description : An associative mapping of keys to values.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Prosidy.Types.Assoc
    ( Assoc(..)
    , asHashMap
    , fromHashMap
    , toHashMap
    , toEntries
    )
where

import           Prosidy.Internal.Classes

import           Data.HashMap.Strict            ( HashMap )

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text.Prettyprint.Doc     as PP

-- | An associative mapping of keys to values.
--
-- Currently implemented as a 'HashMap', this newtype wrapper allows us to:
--
-- 1) Add non-orphan instances to the underlying structure.
-- 2) Change the underlying type if needed.
newtype Assoc k v = Assoc (HashMap k v)
  deriving (Generic)
  deriving (Eq, Show, ToJSON, FromJSON, NFData, Semigroup, Monoid, Hashable) via HashMap k v
  deriving (Foldable, Functor) via HashMap k

instance (Eq k, Hashable k, Binary k, Binary v) => Binary (Assoc k v) where
    get = Assoc . HM.fromList <$> get

    put (Assoc hm) = put $ HM.toList hm

instance (Pretty k, Pretty v) => Pretty (Assoc k v) where
    pretty (Assoc hm) =
        PP.list
            . map (\(k, v) -> pretty k PP.<+> PP.equals PP.<+> pretty v)
            $ HM.toList hm

-- | Given a function which operates on a 'HashMap', return a function which
-- performs an equivalent transfromation on an 'Assoc'.
asHashMap
    :: Functor f
    => (HashMap k v -> f (HashMap k' v'))
    -> Assoc k v
    -> f (Assoc k' v')
asHashMap f (Assoc a) = Assoc <$> f a

-- | Convert a 'HashMap' to an 'Assoc'.
fromHashMap :: HashMap k v -> Assoc k v
fromHashMap = Assoc

-- | Convert an 'Assoc' to a 'HashMap'.
toHashMap :: Assoc k v -> HashMap k v
toHashMap (Assoc hm) = hm

-- | Convert an 'Assoc' into a list of key/value pairs.
toEntries :: Assoc k v -> [(k, v)]
toEntries (Assoc hm) = HM.toList hm

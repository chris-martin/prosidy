{-|
Module      : Prosidy.Types.Set
Description : An unordered collection of unique items.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Prosidy.Types.Set (Set(..), asHashSet, fromHashSet, toHashSet) where

import           Prosidy.Internal.Classes

import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as HS

-- | A newtype wrapper around an unordered collection of unique elements.
--
-- Currently, this is implemented as a wrapper around a 'HashSet'.
newtype Set a = Set (HashSet a)
  deriving stock (Generic)
  deriving (Eq, Show, NFData, Semigroup, Monoid, Hashable) via HashSet a
  deriving Foldable via HashSet

instance (Eq a, Hashable a, Binary a) => Binary (Set a) where
    get = Set . HS.fromList <$> get

    put (Set s) = put $ HS.toList s

instance Pretty a => Pretty (Set a) where
    pretty = pretty . HS.toList . toHashSet

-- | Given a function which operates on 'HashSet's, return a function which
-- performs the same operation on a 'Set'.
asHashSet :: Functor f => (HashSet a -> f (HashSet b)) -> Set a -> f (Set b)
asHashSet f (Set s) = Set <$> f s

-- | Convert a 'Set' to a 'HashSet'.
toHashSet :: Set a -> HashSet a
toHashSet (Set s) = s

-- | Convert a 'HashSet' to a 'Set'.
fromHashSet :: HashSet a -> Set a
fromHashSet = Set

{-|
Module      : Prosidy.Types.Assoc
Description : An associative mapping of keys to values.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Prosidy.Types.Assoc 
    ( Assoc(..)
    , asHashMap
    , fromHashMap
    , toHashMap
    ) where

import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Control.DeepSeq (NFData)
import Data.Binary (Binary(..))
import Data.Hashable (Hashable(..))

import qualified Data.HashMap.Strict as HM

-- | An associative mapping of keys to values.
--
-- Currently implemented as a 'HashMap', this newtype wrapper allows us to:
--
-- 1) Add non-orphan instances to the underlying structure.
-- 2) Change the underlying type if needed.
newtype Assoc k v = Assoc (HashMap k v)
  deriving stock (Generic)
  deriving newtype (Eq, Foldable, Functor, Show, ToJSON, FromJSON, NFData, Semigroup, Monoid, Hashable)

instance (Eq k, Hashable k, Binary k, Binary v) => Binary (Assoc k v) where
    get = 
        Assoc . HM.fromList <$> get

    put (Assoc hm) = 
        put $ HM.toList hm

-- | Given a function which operates on a 'HashMap', return a function which
-- performs an equivalent transfromation on an 'Assoc'.
asHashMap :: Functor f => (HashMap k v -> f (HashMap k' v')) -> Assoc k v -> f (Assoc k' v')
asHashMap f (Assoc a) = Assoc <$> f a

-- | Convert a 'HashMap' to an 'Assoc'.
fromHashMap :: HashMap k v -> Assoc k v
fromHashMap = Assoc

-- | Convert an 'Assoc' to a 'HashMap'.
toHashMap :: Assoc k v -> HashMap k v
toHashMap (Assoc hm) = hm
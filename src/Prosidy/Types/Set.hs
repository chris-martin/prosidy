{-|
Module      : Prosidy.Types.Set
Description : An unordered collection of unique items.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Prosidy.Types.Set 
    ( Set(..)
    , asHashSet
    , fromHashSet
    , toHashSet
    ) where

import Data.HashSet (HashSet)
import GHC.Generics (Generic)
import Data.Aeson (FromJSONKey, ToJSONKey, ToJSON(..), FromJSON(..))
import Control.DeepSeq (NFData)
import Data.Binary (Binary(..))
import Data.Hashable (Hashable(..))
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

newtype Set a = Set (HashSet a)
  deriving stock (Generic)
  deriving newtype (Eq, Foldable, Show, NFData, Semigroup, Monoid, Hashable)

instance (Hashable a, Eq a, ToJSONKey a) => ToJSON (Set a) where
    toJSON (Set hs) = toJSON $ foldMap (flip HM.singleton True) hs
    toEncoding (Set hs) = toEncoding $ foldMap (flip HM.singleton True) hs

instance (Hashable a, Eq a, FromJSONKey a) => FromJSON (Set a) where
    parseJSON json = do
        m <- parseJSON json
        pure . Set . HM.keysSet $ HM.filter id m

instance (Eq a, Hashable a, Binary a) => Binary (Set a) where
    get =
        Set . HS.fromList <$> get
    
    put (Set s) =
        put $ HS.toList s

asHashSet :: (HashSet a -> HashSet b) -> Set a -> Set b
asHashSet f (Set s) = Set (f s)

toHashSet :: Set a -> HashSet a
toHashSet (Set s) = s

fromHashSet :: HashSet a -> Set a
fromHashSet = Set
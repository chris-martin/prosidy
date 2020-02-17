{-|
Module      : Prosidy.Types.Series
Description : A type of items occuring in a sequence.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Prosidy.Types.Series 
    ( -- * Possibly empty collections
      Series(..)
    , asSeq
    , fromSeq
    , toSeq
      -- * Known non-empty collections
    , SeriesNE
    , fromSeqNE
    , toSeqNE
    ) where

import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Control.DeepSeq (NFData)
import Data.Binary (Binary(..))
import Data.Hashable (Hashable(..))
import Data.Foldable (toList, foldl')
import Control.Monad (guard)

import qualified Data.Sequence as Seq

newtype Series a = Series (Seq a)
  deriving stock (Generic, Show)
  deriving newtype (Eq, Foldable, Functor, Applicative, ToJSON, FromJSON, NFData, Semigroup, Monoid)

instance Binary a => Binary (Series a) where
    get = Series . Seq.fromList <$> get
    {-# INLINE get #-}

    put (Series xs) = put $ toList xs
    {-# INLINE put #-}

instance Hashable a => Hashable (Series a) where
    hashWithSalt salt (Series xs) =
        foldl' hashWithSalt salt xs

instance Traversable Series where
    traverse f (Series xs) = Series <$> traverse f xs

newtype SeriesNE a = SeriesNE (Seq a)
  deriving stock (Generic, Show)
  deriving newtype (Eq, Foldable, Functor, Applicative, ToJSON, NFData, Semigroup, Monoid)

instance Binary a => Binary (SeriesNE a) where
    get = maybe (error "SeriesNE must be non-empty") id . fromSeqNE . Seq.fromList <$> get
    {-# INLINE get #-}

    put (SeriesNE xs) = put $ toList xs
    {-# INLINE put #-}

instance FromJSON a => FromJSON (SeriesNE a) where
    parseJSON value = do
        inner <- parseJSON value
        guard (not $ null inner)
        pure $ SeriesNE inner

instance Hashable a => Hashable (SeriesNE a) where
    hashWithSalt salt (SeriesNE xs) =
        foldl' hashWithSalt salt xs

instance Traversable SeriesNE where
    traverse f (SeriesNE xs) = SeriesNE <$> traverse f xs

asSeq :: (Seq a -> Seq b) -> Series a -> Series b
asSeq f (Series s) = Series (f s)

fromSeq :: Seq a -> Series a
fromSeq = Series

toSeq :: Series a -> Seq a
toSeq (Series s) = s

fromSeqNE :: Seq a -> Maybe (SeriesNE a)
fromSeqNE s | null s    = Nothing
fromSeqNE s | otherwise = Just (SeriesNE s)

toSeqNE :: SeriesNE a -> Seq a
toSeqNE (SeriesNE a) = a
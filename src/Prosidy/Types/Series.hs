{-|
Module      : Prosidy.Types.Series
Description : A type of items occuring in a sequence.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
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
      -- * Pattern synonyms for easy manipulation of series
    , pattern NonEmpty
    , pattern Empty
    , pattern (:>>:)
    , pattern (:<<:)
    , pattern (:>:)
    , pattern (:<:)
    )
where

import           Data.Sequence                  ( Seq )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                )
import           Control.DeepSeq                ( NFData )
import           Data.Binary                    ( Binary(..) )
import           Data.Hashable                  ( Hashable(..) )
import           Data.Foldable                  ( toList
                                                , foldl'
                                                )
import           Control.Monad                  ( guard )

import qualified Data.Sequence                 as Seq

-- | A newtype wrapper around a sequential collection.
--
-- Currently, 'Series' is implemented as a 'Seq', but this is not guarenteed to
-- be true.
newtype Series a = Series (Seq a)
  deriving stock (Generic, Show)
  deriving newtype (Eq, Foldable, Functor, Applicative, ToJSON, FromJSON, NFData, Semigroup, Monoid)

instance Binary a => Binary (Series a) where
    get = Series . Seq.fromList <$> get
    {-# INLINE get #-}

    put (Series xs) = put $ toList xs
    {-# INLINE put #-}

instance Hashable a => Hashable (Series a) where
    hashWithSalt salt (Series xs) = foldl' hashWithSalt salt xs

instance Traversable Series where
    traverse f (Series xs) = Series <$> traverse f xs

-- | A non-empty 'Series'.
newtype SeriesNE a = SeriesNE (Seq a)
  deriving stock (Generic, Show)
  deriving newtype (Eq, Foldable, Functor, Applicative, ToJSON, NFData, Semigroup)

instance Binary a => Binary (SeriesNE a) where
    get =
        maybe (error "SeriesNE must be non-empty") id
            .   fromSeqNE
            .   Seq.fromList
            <$> get
    {-# INLINE get #-}

    put (SeriesNE xs) = put $ toList xs
    {-# INLINE put #-}

instance FromJSON a => FromJSON (SeriesNE a) where
    parseJSON value = do
        inner <- parseJSON value
        guard (not $ null inner)
        pure $ SeriesNE inner

instance Hashable a => Hashable (SeriesNE a) where
    hashWithSalt salt (SeriesNE xs) = foldl' hashWithSalt salt xs

instance Traversable SeriesNE where
    traverse f (SeriesNE xs) = SeriesNE <$> traverse f xs

pattern FromSeries :: Series a -> Seq a
pattern FromSeries a <- (fromSeq -> a)

-- | Matches against an empty 'Series'.
pattern Empty :: Series a
pattern Empty = Series Seq.Empty

-- | Matches a non-empty 'SeriesNE' as if it were just a 'Series'.
pattern NonEmpty :: SeriesNE a -> Series a
pattern NonEmpty a <- (seriesNE -> Just a)
  where NonEmpty (SeriesNE a) = Series a

-- | Match against the first element of a 'Series'.
infixr 5 :<:
pattern (:<:) :: a -> Series a -> Series a
pattern a :<: b <- Series (a Seq.:<| FromSeries b)
  where a :<: Series b = Series (a Seq.:<| b)

-- | Match against the last element of a 'Series'.
infixl 5 :>:
pattern (:>:) :: Series a -> a -> Series a
pattern a :>: b <- Series (FromSeries a Seq.:|> b)
  where Series a :>: b = Series (a Seq.:|> b)

-- | Match against a non-empty 'SeriesNE' and a leading element.
infixr 3 :<<:
pattern (:<<:) :: a -> Series a -> SeriesNE a
pattern a :<<: b <- SeriesNE (a Seq.:<| FromSeries b)
  where a :<<: Series b = SeriesNE (a Seq.:<| b)

-- | Match against a non-empty 'SeriesNE' and a trailing element.
infixl 3 :>>:
pattern (:>>:) :: Series a -> a -> SeriesNE a
pattern a :>>: b <- SeriesNE (FromSeries a Seq.:|> b)
  where Series a :>>: b = SeriesNE (a Seq.:|> b)

seriesNE :: Series a -> Maybe (SeriesNE a)
seriesNE = fromSeqNE . toSeq

{-# COMPLETE (:<<:) #-}
{-# COMPLETE (:>>:) #-}
{-# COMPLETE (:>:), Empty #-}
{-# COMPLETE (:<:), Empty #-}

-- | Given a function which operates on a 'Seq', return a function which
-- operates on a 'Series'.
asSeq :: Functor f => (Seq a -> f (Seq b)) -> Series a -> f (Series b)
asSeq f (Series s) = Series <$> f s

-- | Convert a 'Seq' into a 'Series'.
fromSeq :: Seq a -> Series a
fromSeq = Series

-- | Convert a 'Series' into a 'Seq'.
toSeq :: Series a -> Seq a
toSeq (Series s) = s

-- | Convert a non-empty 'Seq' into a 'SeriesNE'.
fromSeqNE :: Seq a -> Maybe (SeriesNE a)
fromSeqNE s | null s    = Nothing
fromSeqNE s | otherwise = Just (SeriesNE s)

-- | Convert a 'SeriesNE' into a 'Seq'. The returned 'Seq' is guarenteed to
-- always contain at least one element.
toSeqNE :: SeriesNE a -> Seq a
toSeqNE (SeriesNE a) = a

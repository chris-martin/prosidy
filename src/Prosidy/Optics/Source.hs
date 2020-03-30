{-|
Module      : Prosidy.Optics.Source
Description : Profunctor optics over Prosidy types.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE Safe #-}
module Prosidy.Optics.Source
    ( -- * Classy optics; implementable on all types with a location
      HasLocation(..)
    , offset
      -- ** Read-only optics.
    , column
    , line
    , source

      -- * Conversion utilities
    , sparse
    )
where

import           Prosidy.Types
import           Prosidy.Source
import           Prosidy.Optics.Internal

-- | A classy optic for selecting the 'Location' from a value. Note that
-- 'location' is affine: a 'Location' can't be attached to a value which does 
-- not -- already have one, and not all values with an instance of 
-- 'HasLocation' have a location.
class HasLocation t where
    location :: Affine' t Location

instance HasLocation Location where
    location = id
    {-# INLINE location #-}

instance HasLocation (Tag a) where
    location = affine' tagLocation (\d l -> d { tagLocation = Just l })
    {-# INLINE location #-}

instance HasLocation (Region a) where
    location = affine' regionLocation (\d l -> d { regionLocation = Just l })
    {-# INLINE location #-}

instance HasLocation Fragment where
    location =
        affine' fragmentLocation (\d l -> d { fragmentLocation = Just l })
    {-# INLINE location #-}

instance HasLocation Paragraph where
    location =
        affine' paragraphLocation (\d l -> d { paragraphLocation = Just l })
    {-# INLINE location #-}

instance HasLocation Block where
    location = affine' get set
      where
        get (BlockLiteral   lit) = tagLocation lit
        get (BlockParagraph p  ) = paragraphLocation p
        get (BlockTag       tag) = tagLocation tag
        set (BlockLiteral lit) l = BlockLiteral lit { tagLocation = Just l }
        set (BlockParagraph p) l =
            BlockParagraph p { paragraphLocation = Just l }
        set (BlockTag tag) l = BlockTag tag { tagLocation = Just l }
    {-# INLINE location #-}

instance HasLocation Inline where
    location = affine' get set
      where
        get Break            = Nothing
        get (InlineTag  tag) = tagLocation tag
        get (InlineText f  ) = fragmentLocation f
        set Break            _ = Break
        set (InlineTag  tag) l = InlineTag tag { tagLocation = Just l }
        set (InlineText f  ) l = InlineText f { fragmentLocation = Just l }
    {-# INLINE location #-}

-- | Focus on the 'Offset' from a value parsed from a source file. If the 
-- 'Offset' is modified, note that the resulting 'column' and 'line' will /also/ be
-- modified as they are denormalizations of this value.
offset :: HasLocation l => Affine' l Offset
offset = location . sparse . lens
    sparseLocationOffset
    (\sl x -> sl { sparseLocationOffset = x })
{-# INLINE offset #-}

-- | Fetch the 'Column' from a value parsed from a source file. Modifications
-- are not allowed as the 'offset' and 'line' may become inconsistent.
column
    :: (HasLocation l, Contravariant f, Applicative f) => Optic' (->) f l Column
column = location . to locationColumn
{-# INLINE column #-}

-- | Fetch the 'Line' from a value parsed from a source file. Modifications
-- are not allowed as the 'offset' and 'column' may become inconsistent.
line :: (HasLocation l, Contravariant f, Applicative f) => Optic' (->) f l Line
line = location . to locationLine
{-# INLINE line #-}

-- | Fetch the 'Source' a value was parsed from. Modifications are not allowed 
-- as the 'line', 'offset', and 'column' may become inconsistent.
source
    :: (HasLocation l, Contravariant f, Applicative f) => Optic' (->) f l Source
source = location . to locationSource
{-# INLINE source #-}

-- | An isomorphism between 'Location' and 'SparseLocation'. This is allowed
-- because although a 'Location' has strictly more data than a 'SparseLocation',
-- those values are denormalizations of values within 'SparseLocation'.
sparse :: Iso' Location SparseLocation
sparse = iso stripLocation enrichLocation
{-# INLINE sparse #-}

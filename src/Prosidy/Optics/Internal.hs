{-|
Module      : Prosidy.Optics.Internal
Description : Internal implementations of common Optics functions, removing a dependency on lens.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Safe #-}
module Prosidy.Optics.Internal
    ( module Prosidy.Optics.Internal
    , Profunctor(..)
    , Choice(..)
    , Strong(..)
    , Contravariant(..)
    )
where

import           Data.Profunctor                ( Profunctor(..)
                                                , Choice(..)
                                                , Strong(..)
                                                )
import           Data.Functor.Const             ( Const(..) )
import           Data.Monoid                    ( First(..)
                                                , Endo(..)
                                                )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Tagged                    ( Tagged(..) )
import           Data.Functor.Contravariant.Compat
                                                ( Contravariant(..) )

type Optic p f s t a b = p a (f b) -> p s (f t)
type Iso s t a b = forall p f . (Profunctor p, Functor f) => Optic p f s t a b
type Lens s t a b = forall p f . (Strong p, Functor f) => Optic p f s t a b
type Prism s t a b
    = forall p f . (Choice p, Applicative f) => Optic p f s t a b
type Affine s t a b
    = forall p f . (Choice p, Strong p, Applicative f) => Optic p f s t a b
type Traversal s t a b = forall f . (Applicative f) => Optic (->) f s t a b

type Optic' p f s a = Optic p f s s a a
type Iso' s a = Iso s s a a
type Lens' s a = Lens s s a a
type Prism' s a = Prism s s a a
type Affine' s a = Affine s s a a
type Traversal' s a = Traversal s s a a

type Getter s a = forall f . (Functor f, Contravariant f) => Optic' (->) f s a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso get set = dimap get (fmap set)
{-# INLINE iso #-}

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = dimap into outof . second'
  where
    into x = (x, get x)
    outof (x, f) = fmap (set x) f
{-# INLINE lens #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism set get = dimap get rhs . right' where rhs = either pure (fmap set)
{-# INLINE prism #-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' set get = dimap lhs rhs . right'
  where
    lhs x = maybe (Left x) Right (get x)
    rhs = either pure (fmap set)
{-# INLINE prism' #-}

affine :: (s -> Either t a) -> (s -> b -> t) -> Affine s t a b
affine get set = dimap lhs rhs . right' . second'
  where
    lhs x = fmap (x, ) $ get x
    rhs = either pure (\(x, f) -> set x <$> f)
{-# INLINE affine #-}

affine' :: (s -> Maybe a) -> (s -> b -> s) -> Affine s s a b
affine' get set = dimap lhs rhs . right' . second'
  where
    lhs x = maybe (Left x) (Right . (x, )) $ get x
    rhs (Left  x     ) = pure x
    rhs (Right (x, f)) = set x <$> f
{-# INLINE affine' #-}

nullAffine :: Affine s s a b
nullAffine = affine' (const Nothing) const
{-# INLINE nullAffine #-}

to :: (s -> a) -> Getter s a
to k = dimap k (contramap k)
{-# INLINE to #-}

view :: Lens s t a b -> s -> a
view f = getConst . f Const
{-# INLINE view #-}

views :: Traversal s t a b -> s -> [a]
views f = flip appEndo [] . getConst . f (Const . Endo . (:))
{-# INLINE views #-}

preview :: Optic (->) (Const (First a)) s t a b -> s -> Maybe a
preview f = getFirst . getConst . f (Const . First . Just)
{-# INLINE preview #-}

over :: Optic (->) Identity s t a b -> (a -> b) -> s -> t
over t f = runIdentity . t (Identity . f)
{-# INLINE over #-}

assign :: Optic' (->) Identity s a -> a -> s -> s
assign t = over t . const
{-# INLINE assign #-}

review :: Prism' s a -> a -> s
review p = runIdentity . unTagged . p . Tagged . Identity
{-# INLINE review #-}

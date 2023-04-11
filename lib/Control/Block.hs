-- |
-- Higher-order functions with their function arguments at the end,
-- for channeling the full power of BlockArguments and LambdaCase .
module Control.Block
  ( -- * Functor
    (<$>)
  , (<&>)
  , fmap
  , imap
  , change
  , ichange

    -- * Applicative
  , (<**>)
  , apply
  , through

    -- * Monad
  , bind
  , ibind

    -- * Foldable

    -- ** With monoids
  , Fold.foldMap
  , foldMap1
  , foldMapA
  , foldMapA1
  , ifoldMap
  , ifoldMapA
  , reduce
  , reduce1
  , reduceA
  , reduceA1
  , ireduce
  , ireduceA

    -- ** Without monoids
  , reduceL
  , reduceL1
  , reduceR
  , reduceR1

    -- * Traversable
  , traverse
  , itraverse
  , itraverse_
  , for
  , ifor
  , ifor_

    -- * Maybe and List
  , mabye
  , emptn

    -- * Filterable
  , (<$?>)
  , (<&?>)
  , filter
  , ifilter
  , sift
  , isift
  , Witherable.mapMaybe
  , imapMaybe
  , changeMaybe
  , ichangeMaybe

    -- * Witherable
  , filterA
  , ifilterA
  , siftA
  , isiftA
  , wither
  , iwither
  , forMaybe
  , iforMaybe
  )
where

import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable)
import Data.Foldable qualified as Fold
import Data.Foldable.WithIndex
import Data.Foldable1 (Foldable1, foldMap1, foldl1', toNonEmpty)
import Data.Function
import Data.Functor
import Data.Functor.WithIndex
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Data.Monoid (Monoid (mempty))
import Data.Semigroup
import Data.Traversable
import Data.Traversable.WithIndex
import Witherable
import Prelude (Bool)

-- | Non-infix version of '(<&>)'.
change :: (Functor f) => f x -> (x -> y) -> f y
change = (<&>)

-- | Flipped version of 'imap'.
ichange :: (FunctorWithIndex i f) => f x -> (i -> x -> y) -> f y
ichange = flip imap

-- | Non-infix version of '(<*>)'.
apply :: (Applicative f) => f (x -> y) -> f x -> f y
apply = (<*>)

-- | Flipped version of 'apply'. Non-infix version of '(<**>)'.
through :: (Applicative f) => f x -> f (x -> y) -> f y
through = (<**>)

-- | 'foldMap' through an 'Applicative' functor.
foldMapA :: (Foldable t, Applicative f, Monoid m) => (x -> f m) -> t x -> f m
foldMapA f = Fold.foldl' (\ !fm x -> liftA2 (<>) fm (f x)) (pure mempty)

-- | 'ifoldMap' through an 'Applicative' functor.
ifoldMapA ::
  (FoldableWithIndex i t, Applicative f, Monoid m) =>
  ((i -> x -> f m) -> t x -> f m)
ifoldMapA f = ifoldl' (\i !fm x -> liftA2 (<>) fm (f i x)) (pure mempty)

-- | 'foldMap1' through an 'Applicative' functor.
foldMapA1 ::
  (Foldable1 t, Applicative f, Semigroup s) => (x -> f s) -> t x -> f s
foldMapA1 f tx = case toNonEmpty tx of
  x :| [] -> f x
  x :| (y : ys) -> liftA2 (<>) (f x) (foldMapA1 f (y :| ys))

-- | Flipped version of 'foldMap1'.
reduce1 :: (Foldable1 t, Semigroup s) => t x -> (x -> s) -> s
reduce1 = flip foldMap1

-- | Flipped version of 'foldMap'.
reduce :: (Foldable t, Monoid m) => t x -> (x -> m) -> m
reduce = flip Fold.foldMap

-- | Flipped version of 'ifoldMap'.
ireduce :: (FoldableWithIndex i t, Monoid m) => t x -> (i -> x -> m) -> m
ireduce = flip ifoldMap

-- | A version of 'foldl'' taking the accumulator first, then the @Foldable@.
reduceL :: (Foldable t) => y -> t x -> (y -> x -> y) -> y
reduceL = flip . flip Fold.foldl'

-- | A version of 'foldl1'' taking the accumulator first, then the @Foldable1@.
reduceL1 :: (Foldable1 t) => t x -> (x -> x -> x) -> x
reduceL1 = flip foldl1'

-- | A version of 'foldr' taking the accumulator first, then the @Foldable@.
reduceR :: (Foldable t) => y -> t x -> (x -> y -> y) -> y
reduceR = flip . flip Fold.foldr

-- | A version of 'foldr1' taking the accumulator first, then the @Foldable@.
reduceR1 :: (Foldable1 t) => t x -> (x -> x -> x) -> x
reduceR1 = flip Fold.foldr1

-- | Flipped version of 'foldMapA'.
reduceA :: (Foldable t, Applicative f, Monoid m) => t x -> (x -> f m) -> f m
reduceA = flip foldMapA

-- | Flipped version of 'foldMapA'.
ireduceA ::
  (FoldableWithIndex i t, Applicative f, Monoid m) =>
  t x ->
  (i -> x -> f m) ->
  f m
ireduceA = flip ifoldMapA

-- | Flipped version of 'foldMapA1'.
reduceA1 ::
  (Foldable1 t, Applicative f, Semigroup s) => t x -> (x -> f s) -> f s
reduceA1 = flip foldMapA1

-- | Non-infix version of '(>>=)'.
bind :: (Monad f) => f x -> (x -> f y) -> f y
bind = (>>=)

-- | Indexed version of 'bind'.
ibind :: (FunctorWithIndex i f, Monad f) => f x -> (i -> x -> f y) -> f y
ibind = (join .) . ichange

-- | A version of 'maybe' with the 'Maybe' argument first.
mabye :: Maybe x -> y -> (x -> y) -> y
mabye mx nothing just = maybe nothing just mx

-- | Act on the empty or 'NonEmpty' cases of a regular list.
emptn :: [x] -> y -> (NonEmpty x -> y) -> y
emptn lx y xy = case lx of [] -> y; (x : xs) -> xy (x :| xs)

-- | Flipped version of 'filter'.
sift :: (Filterable t) => t x -> (x -> Bool) -> t x
sift = flip filter

-- | Flipped version of 'ifilter'.
isift :: (FilterableWithIndex i t) => t x -> (i -> x -> Bool) -> t x
isift = flip ifilter

-- | Flipped version of 'mapMaybe'.
changeMaybe :: (Filterable t) => t x -> (x -> Maybe y) -> t y
changeMaybe = flip Witherable.mapMaybe

-- | Flipped version of 'imapMaybe'.
ichangeMaybe :: (FilterableWithIndex i t) => t x -> (i -> x -> Maybe y) -> t y
ichangeMaybe = flip imapMaybe

-- | Flipped version of 'filterA'.
siftA :: (Applicative f, Witherable t) => t x -> (x -> f Bool) -> f (t x)
siftA = flip filterA

-- | Flipped version of 'ifilterA'.
isiftA ::
  (Applicative f, WitherableWithIndex i t) =>
  (t x -> (i -> x -> f Bool) -> f (t x))
isiftA = flip ifilterA

-- | Flipped version of 'iwither'.
iforMaybe ::
  (Applicative f, WitherableWithIndex i t) =>
  (t x -> (i -> x -> f (Maybe y)) -> f (t y))
iforMaybe = flip iwither

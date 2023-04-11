module Control.Block (
  -- * Functor
  (<$>),
  (<&>),
  fmap,
  imap,
  change,
  ichange,

  -- * Foldable
  foldMap,
  ifoldMap,
  reduce,
  reduceL,
  reduceR,
  ireduce,
  ifor_,
  itraverse_,

  -- * Traversable
  traverse,
  itraverse,
  for,
  ifor,

  -- * Monad
  bind,
  ibind,
) where

import Control.Monad (join)
import Data.Foldable (foldl')
import Data.Foldable.WithIndex (FoldableWithIndex (ifoldMap), ifor_, itraverse_)
import Data.Functor ((<&>))
import Data.Functor.WithIndex (FunctorWithIndex (imap))
import Data.Traversable (for)
import Data.Traversable.WithIndex (TraversableWithIndex (itraverse), ifor)

change :: (Functor f) => f x -> (x -> y) -> f y
change = (<&>)

ichange :: (FunctorWithIndex i f) => f x -> (i -> x -> y) -> f y
ichange = flip imap

-- reduce1 :: (Foldable1 t, Semigroup s) => t x -> (x -> s) -> s
-- reduce1 = flip foldMap1

reduce :: (Foldable t, Monoid m) => t x -> (x -> m) -> m
reduce = flip foldMap

ireduce :: (FoldableWithIndex i t, Monoid m) => t x -> (i -> x -> m) -> m
ireduce = flip ifoldMap

reduceL :: (Foldable t) => (y -> x -> y) -> y -> t x -> y
reduceL = foldl'

reduceR :: (Foldable t) => (x -> y -> y) -> y -> t x -> y
reduceR = foldr

bind :: (Monad f) => f x -> (x -> f y) -> f y
bind = (>>=)

ibind :: (FunctorWithIndex i f, Monad f) => f x -> (i -> x -> f y) -> f y
ibind = (join .) . ichange

module Data.Treap.Sized where

import Prelude

import Data.Monoid.Additive (Additive(..))

class Monoid b <= Sized a b | a -> b where
    sizeof :: a -> b

instance intNodeSized :: Sized Int (Additive Int) where
    sizeof i = Additive i
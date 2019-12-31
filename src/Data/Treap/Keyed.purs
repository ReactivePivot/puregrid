module Data.Treap.Keyed where

import Prelude

class Ord b <= Keyed a b | a -> b where
    key :: a -> b

instance stringKeyed :: Keyed String String where
    key k = k
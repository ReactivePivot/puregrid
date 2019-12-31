module Data.Treap.Countable where

import Data.Monoid.Additive (Additive(..))

class Countable a where
    count :: a -> Additive Int

instance intCountable :: Countable (Additive Int) where
    count a = a

aitoi :: Additive Int -> Int
aitoi (Additive i) = i
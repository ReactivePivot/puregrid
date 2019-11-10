module Data.Treap.Random where

import Prelude (class Monoid, class Ord, class Functor, map)
import Data.Int (toNumber)
import Data.Treap (class Keyed, class Sized, Treap, key, size, treapAdd)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Random

data RTreap g s k = RT g (Treap s k)

instance rTreapFunctor :: Functor (RTreap g s) where
    map f (RT g t) = RT g (map f t)

rTreapAddValue :: forall g s k v
    . RandomGen Number g 
    => Ord k 
    => Monoid s 
    => Sized v s
    => Keyed v k
    => v -> RTreap g s k
    -> RTreap g s k
rTreapAddValue v t = rTreapAdd (size v) (key v) t

rTreapAdd :: forall g s k
    .  RandomGen Number g 
    => Ord k 
    => Monoid s 
    => s -> k -> RTreap g s k
    -> RTreap g s k
rTreapAdd s k (RT g t) = 
    let p = next g
    in RT (snd p) (treapAdd t s k (fst p))
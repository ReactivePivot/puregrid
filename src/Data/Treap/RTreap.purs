module Data.Treap.Random(
    RTreap(..),
    rTreapAdd,
    rItemAt,
    rItemAtIndex,
    rRange,
    rIndexRange
) where

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid)
import Data.Random (class RNG, nextNum)
import Data.Treap (Treap, indexRange, itemAt, itemAtIndex, treapAdd, treapRange)
import Data.Treap.Countable (class Countable, count)
import Data.Treap.Keyed (class Keyed)
import Data.Treap.Sized (class Sized, sizeof)
import Data.Tuple (fst, snd)
import Prelude (class Functor, class Ord, class Show, map)

data RTreap g s v = RTreap g (Treap s v)

derive instance genericRTreap :: Generic (RTreap g s v) _

instance showRTreap :: (Show g, Show (Treap s v)) => Show (RTreap g s v) where
    show x = genericShow x 

instance rTreapFunctor :: Functor (RTreap g s) where
    map f (RTreap g t) = RTreap g (map f t)

rTreapAdd :: forall g v s k. Ord k => RNG g => Sized v s => Keyed v k => 
    v -> RTreap g s v -> RTreap g s v
rTreapAdd v (RTreap g t) =
    let p = nextNum g
    in RTreap (snd p) (treapAdd v (fst p) (t :: Treap s v))

rRange :: forall g s k o. Sized k s => Ord o => Monoid o =>
    (s -> o) -> o -> o -> RTreap g s k -> List k
rRange prop from to (RTreap g t) = treapRange prop from to t

rItemAt :: forall g s k o. Sized k s => Ord o => Monoid o => 
    (s -> o) -> o -> RTreap g s k -> Maybe k
rItemAt prop o (RTreap g t) = itemAt prop o t

rItemAtIndex :: forall g s k. Countable s => Sized k s => Int -> (RTreap g s k) -> Maybe k
rItemAtIndex i (RTreap g t) = itemAtIndex i t

rIndexRange :: forall g s k. Countable s => Sized k s => Int -> Int -> (RTreap g s k) -> List k
rIndexRange i j (RTreap g t) = indexRange i j t


instance sizedRTreap :: Sized v s => Sized (RTreap g s v) s where
    sizeof (RTreap g t) = sizeof t

instance foldable :: Foldable (RTreap g s) where
    foldr f b (RTreap g t) = foldr f b t
    foldl f b (RTreap g t) = foldl f b t
    foldMap f (RTreap g t) = foldMap f t

instance countableTreap :: Countable s => Countable (RTreap g s k) where
    count (RTreap g t) = count t

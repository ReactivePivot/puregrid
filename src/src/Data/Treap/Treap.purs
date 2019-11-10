module Data.Treap where

import Prelude (class Monoid, class Ord, class Show, Ordering(..), map, compare, (<>), (<$>), (>), mempty, class Functor)
import Data.Generic.Rep as G
import Data.Generic.Rep.Show as GShow

data Treap s k 
    = TreapNode { left :: Treap s k, key :: k, size :: s, sum :: s, priority :: Number, right :: Treap s k } 
    | Empty

instance treapFunctor :: Functor (Treap s) where 
    map f (TreapNode n@{ key: k, left: l, right: r }) 
        = TreapNode n { 
            key = f k,
            left = map f l, 
            right = map f r 
        }
    map f Empty = Empty

rotateLeft :: forall k s. Treap s k -> Treap s k
rotateLeft (TreapNode n@{ right: TreapNode r }) = 
    TreapNode r { left = TreapNode (n { right = r.left }) }
rotateLeft n = n

rotateRight :: forall k s. Treap s k -> Treap s k
rotateRight (TreapNode n@{ left: TreapNode l }) =
    TreapNode l { right = TreapNode (n { left = l.right }) } 
rotateRight t = t

treapAdd :: forall k s. Ord k => Monoid s => Treap s k -> s -> k -> Number -> Treap s k
treapAdd Empty s k p = TreapNode { left: Empty, key: k, size: s, sum: s, priority: p, right: Empty }
treapAdd (TreapNode n) s k p = 
    case compare k n.key of
        EQ -> recalcSum(TreapNode n { key = k })
        LT -> case TreapNode n { left = (treapAdd n.left s k p) } of
                t@(TreapNode { left: TreapNode { priority: pri }, priority: p' }) -> 
                    recalcSum (if p' > pri then rotateRight t else t)
                t -> t
        GT -> case TreapNode n { right = (treapAdd n.right s k p) } of
                t@(TreapNode { right: TreapNode { priority: pri }, priority: p' }) -> 
                    recalcSum(if p' > pri then rotateLeft t else t)
                t -> t

class Ord b <= Keyed a b where
    key :: a -> b

class Monoid b <= Sized a b where
    size :: a -> b

instance sizeTreap :: Monoid s => Sized (Treap s k) s where
    size Empty = mempty
    size (TreapNode n) = n.size <> treapNodeSum n.left <> treapNodeSum n.right

treapNodeSum :: forall k s. Monoid s => Treap s k -> s
treapNodeSum Empty = mempty
treapNodeSum (TreapNode n) = n.sum

recalcSum :: forall k s. Monoid s => Treap s k -> Treap s k
recalcSum Empty = Empty
recalcSum t@(TreapNode n) = TreapNode n { sum = (size t) }

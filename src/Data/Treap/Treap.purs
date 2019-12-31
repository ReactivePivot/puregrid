module Data.Treap(
    Treap(..), 
    TreapRec,
    treapAdd,
    leaf,
    itemAt,
    itemAtIndex,
    treapRange,
    indexRange,
    mkTreap
) where 

import Prelude

import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), foldMap, head, singleton)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Treap.Countable (class Countable, count)
import Data.Treap.Keyed (class Keyed, key)
import Data.Treap.Sized (class Sized, sizeof)

type TreapRec s v = { left :: Treap s v
                    , value :: v
                    , sum :: s
                    , priority :: Number
                    , right :: Treap s v 
                    } 

data Treap s v 
    = TNode (TreapRec s v)
    | Empty

mkTreap :: forall s v. Sized v s => Number -> (Treap s v) -> v -> (Treap s v) -> Treap s v
mkTreap p l v r = TNode { left: l, value: v, right: r, priority: p, sum: sizeof v }

mkTreap' ::  forall s v. Number -> s -> (Treap s v) -> v -> (Treap s v) -> Treap s v
mkTreap' p s l v r = TNode { left: l, value: v, right: r, priority: p, sum: s }


leaf :: forall s v. Sized v s => v -> Number -> Treap s v
leaf v p = TNode { left: Empty
                , sum: sizeof v
                , value: v
                , priority: p
                , right: Empty 
                }

rotateLeft :: forall v s. Sized v s => Treap s v -> Treap s v
rotateLeft (TNode n@{ right: TNode r }) = 
    TNode r { left = recalcSum $ TNode (n { right = recalcSum r.left }) }
rotateLeft n = n

rotateRight :: forall v s. Sized v s =>  Treap s v -> Treap s v
rotateRight (TNode n@{ left: TNode l }) =
    TNode l { right = recalcSum $ TNode (n { left = l.right }) } 
rotateRight t = t

treapAdd :: forall s v k. Sized v s => Keyed v k =>
    v -> Number -> Treap s v -> Treap s v
treapAdd v p Empty = leaf v p
treapAdd v p (TNode n) = 
    case compare (key v) (key n.value) of
        EQ -> recalcSum $ TNode n { value = v }
        LT -> addLeft $ TNode n { left = (treapAdd v p n.left) }
        GT -> addRight $ TNode n { right = (treapAdd v p n.right) }

addLeft :: forall s v. Sized v s => Treap s v -> Treap s v
addLeft t@(TNode { left: TNode { priority: pri }, priority: p' }) =
    recalcSum $ if p' > pri then rotateRight t else t
addLeft t = t

addRight :: forall s v. Sized v s => Treap s v -> Treap s v
addRight t@(TNode { right: TNode { priority: pri }, priority: p' }) =
    recalcSum $ if p' > pri then rotateLeft t else t
addRight t = t

treapRange :: forall s k o. Sized k s => Ord o => Monoid o => 
    (s -> o) -> o -> o -> Treap s k -> List k
treapRange = treapRange' mempty

treapRange' :: forall s k o. Sized k s => Ord o => Monoid o => 
    o -> (s -> o) -> o -> o -> Treap s k -> List k
treapRange' _ _ _ _ Empty = Nil
treapRange' start prop from to n@(TNode { left: l, value: v, right: r }) =
    let node_start = prop (sizeof l) <> start  
        node_end = node_start <> prop (sizeof v)
        leftNodes = 
            if node_start >= from
            then treapRange' start prop from to l 
            else Nil
        thisNode = 
            if node_end > from && node_start <= to
            then singleton v
            else Nil
        rightNodes = 
            if node_end <= to
            then treapRange' node_end prop from to r
            else Nil
    in leftNodes <> thisNode <> rightNodes

itemAt :: forall s k o. Sized k s => Ord o => Monoid o => 
    (s -> o) -> o -> Treap s k -> Maybe k
itemAt f at = head <<< treapRange f at at

itemAtIndex :: forall s k. Countable s => Sized k s => 
    Int -> Treap s k -> Maybe k
itemAtIndex = itemAt count <<< Additive

indexRange :: forall s k. Countable s => Sized k s => 
    Int -> Int -> Treap s k -> List k 
indexRange b e = treapRange count (Additive b) (Additive e)

contains :: forall s k o. Countable s => Keyed k o => o -> Treap s k -> Boolean
contains o t = (indexOf o t) /= Nothing

indexOf :: forall s k o. Countable s => Keyed k o => o -> Treap s k -> Maybe Int
indexOf o (TNode { left: l, right: r, value: v }) = 
    case key v `compare` o of
        EQ -> Just 1
        GT -> indexOf o l
        LT -> indexOf o r
indexOf _ Empty = Nothing

treapNodeSum :: forall k s. Monoid s => Treap s k -> s
treapNodeSum Empty = mempty
treapNodeSum (TNode n) = n.sum

recalcSum :: forall v s. Sized v s => Treap s v -> Treap s v
recalcSum Empty = Empty
recalcSum t@(TNode n) = TNode n { sum = (sizeof t) }

derive instance genericTreap :: Generic (Treap s v) _

instance treapShow :: (Show s, Show v) => Show (Treap s v) where
    show x = genericShow x

instance sizeTreap :: Sized v s => Sized (Treap s v) s where
    sizeof Empty = mempty
    sizeof (TNode n) = sizeof n.value <> treapNodeSum n.left <> treapNodeSum n.right

instance treapFunctor :: Functor (Treap s) where 
    map f (TNode n@{ value: v, left: l, right: r }) 
        = TNode n { 
            value = f v,
            left = map f l, 
            right = map f r 
        }
    map f Empty = Empty

instance treapFoldable :: Foldable (Treap s) where
    foldr f = foldrDefault f 
    foldl f = foldlDefault f
    foldMap f Empty = mempty
    foldMap f (TNode { left: l, value: v, right: r }) =
        foldMap f l <> f v <> foldMap f r

instance treapTraversable :: Traversable (Treap s) where
    traverse f Empty = pure Empty
    traverse f (TNode { left: l
                    , value: v
                    , right: r
                    , priority: p
                    , sum: s 
                    }) = 
        (mkTreap' p s) <$> traverse f l <*> f v <*> traverse f r
    sequence = sequenceDefault

instance countableTreap :: Countable s => Countable (Treap s k) where
    count (TNode { sum: s }) = count s    
    count Empty = Additive 0
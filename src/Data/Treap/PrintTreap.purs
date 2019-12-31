module Data.Treap.Print where

import Prelude
import Data.Tuple (Tuple(..), fst, snd)
import Data.Treap (Treap(..))
import Data.Treap.Random (RTreap(..))
import Data.Unfoldable (replicate)
import Data.String.CodeUnits (fromCharArray)

class Print a c where
    print :: a -> c -> String

printRoot :: forall x. Print x (Tuple Int String) => x -> String
printRoot x = print x (Tuple 0 "")

instance printTreap :: (Show s, Show v) => Print (Treap s v) (Tuple Int String) where
    print (Empty) c = ""
    print (TNode { left: left, right: right, value: v, priority: p, sum: s }) c = 
        let 
            indent = fromCharArray $ replicate (fst c) ' '
            l = (fst c) + 1
        in indent 
            <> snd c <> " "
            <> show v <> " "
            <> show p <> " "
            <> show s <> "\n"
            <> print left (Tuple l "🠘") 
            <> print right (Tuple l "🠚")

printR :: forall g s k. Show s => Show k => RTreap g s k -> String
printR (RTreap _ n) = print n (Tuple 0 "[*]")

printT :: forall s v. Show s => Show v => Treap s v -> String
printT t = print t (Tuple 0 "[*]")
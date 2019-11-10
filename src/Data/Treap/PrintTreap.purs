module Data.Treap.Print where

import Prelude
import Data.Tuple (Tuple(..), fst, snd)
import Data.Treap (Treap(..))
import Data.Treap.Random (RTreap(..))
import Data.Unfoldable (replicate)
import Data.String.CodeUnits (fromCharArray)

class Print a c where
    print :: a -> c -> String

instance printTreap :: (Show s, Show k) => Print (Treap s k) (Tuple Int String) where
    print (Empty) c = ""
    print (TreapNode { left: left, right: right, key: k, priority: p, size: sz, sum: s }) c = 
        let 
            indent = fromCharArray $ replicate (fst c) ' '
            l = (fst c) + 1
        in indent 
            <> snd c <> " "
            <> show k <> " "
            <> show (p / 4248525000.0 + 0.5) <> " "
            <> show sz <> " "
            <> show s <> "\n"
            <> print left (Tuple l "[L]") 
            <> print right (Tuple l "[R]")

printR :: forall g s k. Show s => Show k => RTreap g s k -> String
printR (RT _ n) = print n (Tuple 0 "[*]")
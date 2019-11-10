module Grid.Axis where

import Prelude
import Data.Treap.Random (RTreap)
import Data.Random.XorShift (XorShiftGen)
  
data AxisEntry = AX Number Int

instance axisEntrySemigroup :: Semigroup AxisEntry where
  append (AX s1 c1) (AX s2 c2) = AX (s1 + s2) (c1 + c2)

instance axisEntryMonoid :: Monoid AxisEntry where
  mempty = AX 0.0 0

instance axisEntryShow :: Show AxisEntry where
  show (AX s c) = "<" <> show s <> ", " <> show c <> ">"

type AxisTreap k = RTreap XorShiftGen AxisEntry k
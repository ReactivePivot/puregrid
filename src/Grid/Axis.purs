module Grid.Axis where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Monoid.Additive (Additive(..))
import Data.Random.XorShift (XorShiftGen(..))
import Data.Treap (Treap(..))
import Data.Treap.Countable (class Countable)
import Data.Treap.Keyed (class Keyed)
import Data.Treap.Random (RTreap(..))
import Data.Treap.Sized (class Sized)
  
data AxisSize = AxSize Number Int
data AxisEntry k = AxEntry k AxisSize

instance axisEntrySemigroup :: Semigroup AxisSize where
  append (AxSize s1 c1) (AxSize s2 c2) = AxSize (s1 + s2) (c1 + c2)

instance axisEntryMonoid :: Monoid AxisSize where
  mempty = AxSize 0.0 0

derive instance axisSizeGeneric :: Generic AxisSize _

instance axisSizeShow :: Show AxisSize where
  show x = genericShow x

derive instance axisEntryGeneric :: Generic (AxisEntry k) _

instance axisEntryShow :: Show k => Show (AxisEntry k) where 
  show x = genericShow x

instance axisEntryKeyed :: Ord k => Keyed (AxisEntry k) k where
  key (AxEntry k _) = k

instance axisSized :: Sized (AxisEntry k) AxisSize where
  sizeOf (AxEntry _ s) = s

instance axisSizeCount :: Countable AxisSize where
  count (AxSize _ c) = Additive c

type AxisTreap k = RTreap XorShiftGen AxisSize (AxisEntry k)

getExtent :: AxisSize -> Number
getExtent (AxSize w _) = w

mkAxisTreap :: forall k. Int -> AxisTreap k
mkAxisTreap n = RTreap (XSG n) Empty
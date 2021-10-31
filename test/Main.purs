module Test.Main where

import Prelude

import Data.Array (filter, fromFoldable, (..))
import Data.Foldable (all, foldMap, sum)
import Data.List (length, mapWithIndex, slice, (:), List(..))
import Data.Treap.Keyed (key)
import Data.Treap.Random (rIndexRange, rItemAtIndex)
import Data.Treap.Sized (sizeOf)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect (Effect)
import Math (round)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.Utils (
  TreapTestData(..), 
  assertApproxEquals, 
  checkEntry, 
  isSorted, 
  keyList, 
  msg, 
  passed, 
  mkTreapTestData
)

import Grid.Axis (getExtent)

checkLength :: TreapTestData -> Result
checkLength (TD sorted treap) = 
  let list = keyList treap 
  in (length list) == (length sorted) <?> msg sorted

checkOrdered :: TreapTestData -> Result
checkOrdered  (TD sorted treap) =
  let list = keyList treap 
  in isSorted list <?> msg sorted

checkSize :: TreapTestData -> Result
checkSize (TD sorted treap) =
  let treapSize = getExtent (sizeOf treap)
      sumSets = sum (snd <$> sorted)
      eq = round treapSize == round sumSets
  in assertApproxEquals treapSize sumSets

checkItemAt :: TreapTestData -> Result
checkItemAt (TD sorted treap) =
  let termChecks = mapWithIndex 
        (flip (checkEntry <<< fst) <<< flip rItemAtIndex treap) 
        sorted
  in all passed termChecks <?> show termChecks <> "\n\n" <> show treap

checkRange :: TreapTestData -> Result
checkRange d@(TD sorted treap) =
  let l = length sorted
      pairs = pure Tuple  <*> 0..(1) <*> 1..1 -- <*> 0..(l-1) <*> 1..l
      indices = filter (uncurry (<)) pairs
      expected = map (\(Tuple a b) -> fromFoldable $ fst <$> slice a b sorted) indices
      actual = map (\(Tuple a b) -> fromFoldable $ key <$> (rIndexRange a b treap)) indices
  in (l == 0 || expected == actual) <?> show (fromFoldable expected) <> show (fromFoldable actual) <> "\n" <> show indices <> "\n" <> show d 

main :: Effect Unit
main = do  
  foldMap quickCheck arr
  where 
    l = (Tuple "X" 1.0):(Tuple "Y" 2.0):(Tuple "Z" 3.0):Nil
    td = mkTreapTestData 1 l
    --arr = [ checkRange td ]
    arr = [ checkLength
      , checkOrdered
      , checkSize
      , checkItemAt
      , checkRange
      ]

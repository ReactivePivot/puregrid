module Test.Main where

import Prelude

import Data.Array (filter, fromFoldable, (..))
import Data.Foldable (all, foldMap, sum)
import Data.List (length, mapWithIndex, slice)
import Data.Treap.Keyed (key)
import Data.Treap.Random (rIndexRange, rItemAtIndex)
import Data.Treap.Sized (sizeof)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect (Effect)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.Utils (TreapTestData(..), assertApproxEquals, checkEntry, isSorted, keyList, msg, passed, widthOf)

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
  let treapSize = widthOf (sizeof treap)
      sumSets = sum (snd <$> sorted)
  in assertApproxEquals treapSize sumSets

checkItemAt :: TreapTestData -> Result
checkItemAt (TD sorted treap) =
  let termChecks = mapWithIndex 
        (flip (checkEntry <<< fst) <<< flip rItemAtIndex treap) 
        sorted
  in all passed termChecks <?> show termChecks <> "\n\n" <> show treap

checkRange :: TreapTestData -> Result
checkRange (TD sorted treap) =
  let l = length sorted
      pairs = pure Tuple <*> 0..(l-1) <*> 1..l
      indices = filter (uncurry (<)) pairs
      expected = map (\(Tuple a b) -> fromFoldable $ fst <$> slice a b sorted) indices
      actual = map (\(Tuple a b) -> fromFoldable $ key <$> (rIndexRange a b treap)) indices
  in (l == 0 || expected == actual) <?> show (fromFoldable expected) <> show (fromFoldable actual)

main :: Effect Unit
main = do 
  foldMap quickCheck 
    [ checkLength
    , checkOrdered
    , checkSize
    , checkItemAt
    , checkRange
    ]

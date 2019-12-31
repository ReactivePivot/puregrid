module Test.Utils where

import Prelude

import Data.Bifunctor (lmap)
import Data.List (List(..), fromFoldable, nubBy, sort, (:))
import Data.Maybe (Maybe(..))
import Data.Number.Approximate ((~=))
import Data.String (toLower)
import Data.Tuple (Tuple(..), fst, uncurry)
import Grid.Axis (AxisEntry(..), AxisSize(..), AxisTreap)
import Main (AxisJson, mkFromList)
import Test.AlphaNumString (AlphaNumString, str)
import Test.QuickCheck (Result, arbitrary, (<?>))
import Test.QuickCheck.Arbitrary (class Arbitrary)

data CheckResult a = EqCheck Boolean a a

instance showCheckResult :: (Eq a, Show a) => Show (CheckResult a) where
    show (EqCheck b l r) = show l <> (if b then " == " else " /= ") <> show r <> if not b && l == r then "!!" else "."

convertStrings :: List (Tuple AlphaNumString Number) -> List (Tuple String Number)
convertStrings = map (lmap (toLower <<< str))

passed :: forall a. CheckResult a -> Boolean
passed (EqCheck b _ _) = b

axJs :: String -> Number -> AxisJson
axJs k s = { key: k, width: s }

checkEntry :: String -> Maybe (AxisEntry String) -> CheckResult String
checkEntry s (Just (AxEntry k _))= EqCheck (k == s) k s
checkEntry s Nothing = EqCheck false "<NOT FOUND>" s

ce :: String -> Number -> AxisEntry String
ce s e = AxEntry s (AxSize e 1)

widthOf :: AxisSize -> Number
widthOf (AxSize w _) = w

isSorted :: forall a. Ord a => List a -> Boolean
isSorted Nil = true
isSorted (a:Nil) = true
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

mkTestTreap :: Int -> List (Tuple String Number) -> AxisTreap String
mkTestTreap seed pairs = mkFromList seed $ uncurry axJs <$> pairs

uniqueKeys :: List (Tuple String Number) -> List (Tuple String Number)
uniqueKeys = nubBy $ fst >>> flip ((==) <<< fst)

keyList :: AxisTreap String -> List String
keyList t =  (\(AxEntry k _) -> k) <$> fromFoldable t

msg :: forall a. (Show a) => a -> String
msg a = "Test failed for input: " <> show a

assertApproxEquals :: Number -> Number -> Result
assertApproxEquals a b = a ~= b <?> show a <> " !~= " <> show b  

data TreapTestData = TD (List (Tuple String Number)) (AxisTreap String)

instance arbitraryTreapTestData :: Arbitrary TreapTestData where
    arbitrary = map fromTestInit arbitrary
        where
        fromTestInit (Tuple n l) = TD sorted (mkTestTreap n pairs)
            where
            pairs = uniqueKeys $ convertStrings l :: List (Tuple String Number)
            sorted = nubBy (==) (sort pairs)

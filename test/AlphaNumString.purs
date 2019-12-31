module Test.AlphaNumString where

import Prelude

import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (fromCharArray, toCharArray)

import Test.QuickCheck.Gen (Gen, arrayOf, oneOf)
import Test.QuickCheck.Arbitrary (class Coarbitrary, class Arbitrary)

newtype AlphaNumString = AlphaNumString String

str :: AlphaNumString -> String
str (AlphaNumString s) = s

derive instance newtypeAlphaNumString :: Newtype AlphaNumString _
derive newtype instance eqAlphaNumString :: Eq AlphaNumString
derive newtype instance ordAlphaNumString :: Ord AlphaNumString
derive newtype instance showAlphaNumString :: Show AlphaNumString

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = t <$> fca
    where
    t = AlphaNumString <<< fromCharArray :: Array Char -> AlphaNumString
    fca =  arrayOf anyChar :: Gen (Array Char)
    rest :: Array Char
    rest = toCharArray "bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    anyChar :: Gen Char
    anyChar = oneOf $ (pure 'a') :| (map pure rest)

derive newtype instance coarbAlphaNumString :: Coarbitrary AlphaNumString

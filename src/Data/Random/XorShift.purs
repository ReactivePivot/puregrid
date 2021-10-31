module Data.Random.XorShift where

import Data.Int (toNumber)
import Data.Int.Bits ((.^.), shl, shr)
import Data.Random (class RandomGen)
import Data.Tuple (Tuple(..))
import Prelude (class Show, top, (+), (/))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype XorShiftGen = XSG Int

derive instance genericRandomGen :: Generic XorShiftGen _

instance showRandomGen :: Show XorShiftGen where
    show = genericShow

nextInt :: Int -> Tuple Int XorShiftGen
nextInt s = let x = s .^. (s `shl` 13)
                y = x .^. (x `shr` 17)
                z = (y .^. (y `shl` 5))
            in Tuple z (XSG z)

instance xorShiftIntGen :: RandomGen Int XorShiftGen where
    next (XSG s) = nextInt s

instance xorShiftGen :: RandomGen Number XorShiftGen where
    next (XSG s) 
        =   let (Tuple z g) = nextInt s
                r = toNumber z
                t = toNumber (top :: Int)
                n = r / 2.0 / t + 0.5
            in Tuple n g
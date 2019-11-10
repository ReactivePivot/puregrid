module Data.Random.XorShift where

import Prelude
import Data.Int.Bits ((.^.), shl, shr)
import Data.Random (class RandomGen)
import Data.Tuple
import Data.Int (toNumber)

newtype XorShiftGen = XSG Int

instance xorShiftGen :: RandomGen Number XorShiftGen where
    next (XSG s) 
        = let x = s .^. (s `shl` 13)
              y = x .^. (x `shr` 17)
              z = (y .^. (y `shl` 5))
          in Tuple (toNumber z) (XSG z)

instance showXorShiftGen :: Show XorShiftGen where
    show (XSG s) = show s 
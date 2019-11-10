module Data.Random where

import Data.Tuple

class RandomGen a g where
    next :: g -> Tuple a g
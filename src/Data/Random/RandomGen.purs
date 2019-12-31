module Data.Random where

import Data.Tuple

class RandomGen a g where
    next :: g -> Tuple a g

class RandomGen Number g <= RNG g where
  nextNum :: g -> Tuple Number g

instance randomGenRNG :: RandomGen Number g => RNG g where
    nextNum = next
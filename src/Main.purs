module Main where

import Prelude
import Data.Tuple
import Effect (Effect)
import Effect.Console (log)
import Data.Random.XorShift (XorShiftGen(..))
import Data.Treap.Random (RTreap(..), rTreapAdd)
import Data.Treap (Treap(..), class Sized)
import Data.Treap.Print
import Grid.Axis (AxisTreap, AxisEntry(..))

main :: Effect Unit
main = do
  let a = RT (XSG 22) Empty :: AxisTreap String
  let b = rTreapAdd (AX 33.0 1) "B"
            <<< rTreapAdd (AX 20.0 1) "C"  
            <<< rTreapAdd (AX 70.0 1) "D"
            <<< rTreapAdd (AX 35.0 1) "E"
            <<< rTreapAdd (AX 100.0 1) "F"
            <<< rTreapAdd (AX 70.0 1) "G"
            <<< rTreapAdd (AX 50.0 1) "H"
            <<< rTreapAdd (AX 35.5 1) "B"
            $ a
  let c = map (flip (<>) "X") b
  log $ printR c
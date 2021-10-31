module Grid.Axis.Serialization where

import Grid.Axis

import Data.Either (Either(..))
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Treap.Random (rTreapAdd)
import Prelude (($), (<$>))
import Simple.JSON as JSON

type AxisJson = { key :: String, extent :: Number }

mkAxisFromJson :: Int -> String -> Maybe (AxisTreap String)
mkAxisFromJson s j = (mkAxisFromList s) <$> loadJson j

loadJson :: String -> Maybe (List AxisJson)
loadJson j = 
  case JSON.readJSON j of
    Right (r :: Array AxisJson) -> Just $ fromFoldable r
    Left e -> Nothing

mkAxisFromList :: Int -> List AxisJson -> AxisTreap String
mkAxisFromList i l = axisFromList (mkAxisTreap i) l

axisFromList :: AxisTreap String -> List AxisJson -> AxisTreap String
axisFromList t Nil = t
axisFromList t (Cons x xs) = axisFromList (rTreapAdd (fromAxisJson x) t) xs

fromAxisJson :: AxisJson -> AxisEntry String
fromAxisJson j = AxEntry j.key (AxSize j.extent 1)

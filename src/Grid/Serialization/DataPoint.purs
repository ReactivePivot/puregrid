module Grid.Model.Serialization where
  
import Grid.Model

import Data.Either (Either(..))
import Data.HashMap (fromFoldableBy)
import Data.List (List, fromFoldable, groupBy, sortBy)
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..))
import Prelude (compare, ($), (<$>), (==))
import Simple.JSON as JSON

type DataPointJson = { row :: String, column :: String, value :: String }

mkDataPointMapFromJson :: String -> Maybe DataPointMap
mkDataPointMapFromJson j = mkDataPointMapFromList <$> loadJson j

loadJson :: String -> Maybe (List DataPointJson)
loadJson j =
    case JSON.readJSON j of
        Right (r :: Array DataPointJson) -> Just $ fromFoldable r
        Left e -> Nothing

mkDataPointMapFromList :: List DataPointJson -> DataPointMap
mkDataPointMapFromList l = fromFoldableBy (\a -> (head a).row) createColumnHash groupedRows
    where
      groupedRows = sortGroup (\a -> a.row) l
      createColumnHash = fromFoldableBy (\a -> a.column) (\a -> DataPoint a.value)
      groupColumns cl = sortGroup (\a -> a.column) cl
      sortGroup f list = groupBy (sameProp f) $ sortBy (compareProp f) list
      compareProp f a b = compare (f a) (f b)
      sameProp (f :: DataPointJson -> String) a b = (f a) == (f b)


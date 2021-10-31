module Main where

import Data.Maybe (Maybe(..))
import Data.Treap.Print (printR)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Grid.Axis (AxisEntry(..), AxisSize(..))
import Grid.Axis.Serialization (mkAxisFromJson, mkAxisFromList)
import Grid.Model (GridModel(..))
import Grid.Model.Serialization (mkDataPointMapFromJson, mkDataPointMapFromList)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, pure, void, ($), (<>))
import Sample.Data (cols, dataPoints, rows) as Sample

addszr :: AxisEntry String -> String -> String
addszr (AxEntry k (AxSize _ c)) i = i <> k

getKey :: forall a. AxisEntry a -> a
getKey (AxEntry k _) = k


createModel :: GridModel
createModel =
  GridModel { rowAxis, colAxis, dataPoints }
  where 
    rowAxis = mkAxisFromList 22 Sample.rows
    colAxis = mkAxisFromList 22 Sample.cols
    dataPoints = mkDataPointMapFromList Sample.dataPoints
    
-- createModel :: Maybe GridModel
-- createModel = do
--   columns    <- readTextFile UTF8 "src\\columns.json"
--   rows       <- readTextFile UTF8 "src\\rows.json"
--   points     <- readTextFile UTF8 "src\\data.json"
--   pure $ do
--     colAxis    <- mkAxisFromJson 22 columns
--     rowAxis    <- mkAxisFromJson 22 rows
--     dataPoints <- mkDataPointMapFromJson points
--     Just (GridModel { rowAxis, colAxis, dataPoints })


main = void $ unsafePartial do
  let model = createModel
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  log $ "Done"

-- main =  launchAff $ do
--   model <- createModel
  
--   liftEffect do 
--     case model of 
--       Nothing ->
--         log $ "Error"
--       Just (GridModel m) ->
--         log $ printR m.rowAxis
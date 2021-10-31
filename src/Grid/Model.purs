module Grid.Model where

import Grid.Axis (AxisTreap)
import Data.HashMap

data DataPoint = DataPoint String

type DataPointMap = HashMap String (HashMap String DataPoint)

data GridModel = GridModel { 
        rowAxis    :: AxisTreap String, 
        colAxis :: AxisTreap String,
        dataPoints :: DataPointMap
    }

data GMod = GM (HashMap String (HashMap String DataPoint))
    
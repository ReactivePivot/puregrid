module Sample.Data where

import Data.List (List, fromFoldable)
import Grid.Axis.Serialization (AxisJson)
import Grid.Model.Serialization (DataPointJson)

rows :: List AxisJson
rows = fromFoldable ([
    { key: "A", extent: 25.0},
    { key: "B", extent: 25.0}
])

cols :: List AxisJson
cols = fromFoldable ([
    { key: "A", extent: 70.0 },
    { key: "B", extent: 100.0 },
    { key: "C", extent: 120.0 },
    { key: "D", extent: 80.0 },
    { key: "E", extent: 60.0 },
    { key: "F", extent: 40.0 },
    { key: "G", extent: 70.0 },
    { key: "H", extent: 100.0 },
    { key: "I", extent: 120.0 },
    { key: "J", extent: 80.0 },
    { key: "K", extent: 60.0 },
    { key: "L", extent: 40.0 }
])

dataPoints :: List DataPointJson
dataPoints = fromFoldable ([
    { row: "A", column: "A", value: "X" },
    { row: "A", column: "B", value: "X" },
    { row: "A", column: "C", value: "X" },
    { row: "A", column: "D", value: "X" },
    { row: "A", column: "E", value: "X" },
    { row: "A", column: "F", value: "X" },
    { row: "A", column: "G", value: "X" },
    { row: "A", column: "H", value: "X" },
    { row: "A", column: "I", value: "X" },
    { row: "A", column: "J", value: "X" },
    { row: "A", column: "K", value: "X" }
])
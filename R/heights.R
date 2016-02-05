testPayload <- function() {
  json = '[{"tag": "tag1", "diameter": 30.4, "actual_height1": 90, "E_Value": -0.5},
          {"tag": "tag2", "diameter": 20.4,  "E_Value": -0.6},
  {"tag": "tag3",  "E_Value": -0.6},
 {"tag": "tag3",  "diameter": 20.4}
  ]'
  receiveHeightsPayload(json)
}
receiveHeightsPayload <- function(json) {
  library(jsonlite)
  args = fromJSON(json)
  computeHeights(args)
}

computeHeights <-
  function(frame)
  {
    result = apply(frame, 1, function(frameRow) {
      print(frameRow)
      tag = frameRow["tag"]
      height = as.numeric(frameRow["actual_height"])
      diameter = as.numeric(frameRow['diameter'])
      eValue = as.numeric(frameRow["E_Value"])
      rowResult = frame()
      if (! is.finite(height)) { height = 0 }
      if (! is.finite(diameter) || ! is.finite(eValue)) {
        rowResult$tag = tag
      }
      else {
        regression = heightRegression(tag, diameter, 
                         height, 
                         eValue)
  
        rowResult$predictedHeight = regression$predht
        rowResult$tag = tag
      }
      rowResult
      }
     
    )
    result
  }

autorun <- testPayload()

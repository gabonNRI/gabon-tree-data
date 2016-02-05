testPayload <- function() {
  json = '[{"tag": "tag1", "diameter": 30.4, "actual_height1": 90, "E_Value": -0.5},
          {"tag": "tag2", "diameter": 20.4,  "E_Value": -0.6}]'
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
    result = apply(frame, 1, function(row) {
    
      tag = row["tag"]
      height = row["actual_height"]
      if (! is.finite(height)) { height = 0 }
      
      regression = heightRegression(tag, row['diameter'], 
                       height, 
                       row["E_Value"])
      rowResult = frame()
      rowResult$predictedHeight = regression$predht
      rowResult$tag = tag
      rowResult
      }
     
    )
    result
  }

autorun <- testPayload()

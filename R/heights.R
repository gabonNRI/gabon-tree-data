testPayload <- function() {
  json = '[{"tag": "tag1", "diameter": 30.4, "actual_height": 409, "E_Value": -0.5}]'
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
      heightRegression(row$tag, row$diameter, row$actual_height, row$E_Value)
      }
    )
  }


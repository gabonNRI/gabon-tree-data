testPayload <- function() {
  json = '[{"tag": "tag1", "diameter": 30.4, "actual_height2": 409, "E_Value": -0.5}]'
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
      print("CAlling reqression on row")
      print (row);
      height = row["actual_height"]
      if (! is.finite(height)) { height = 0}
      heightRegression(row["tag"], row['diameter'], 
                       height, 
                       row["E_Value"])
      }
    )
    result = result$'1'
    result["equation"] = Reduce(paste, deparse(result$equation))
    result
  }

autorun <- testPayload()

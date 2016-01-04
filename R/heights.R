
heightRegression <- function (df) {
  df$height <- 42
  df
}
testPayload <- function() {
  json = '[{"tag": "tag1", "diameter": 30.4, "calculatedHeight": 409}]'
  receiveHeightsPayload(json)
}
receiveHeightsPayload <- function(json) {
  library(jsonlite)
  args = fromJSON(json, simplifyDataFrame = TRUE)
  computeHeights(args)
}
computeHeights <-
  function(frame)
  {
    
 #   trees.str = '"tag1",30.4, 399
 #"tag2", 42, 100'
#    frame = read.csv(text=trees.str, header=FALSE)
    colnames(frame) <- c("tag", "diameter", "calculated_height" )
    result = heightRegression(frame)
  }


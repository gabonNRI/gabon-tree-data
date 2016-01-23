
heightRegression2 <- function (df) {
  df$height <- 42
  df
}
testPayload <- function() {
  json = '[{"tag": "tag1", "diameter": 30.4, "actual_height": 409}]'
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
    colnames(frame) <- c("tag", "diameter", "actual_height" )
    result = heightRegression(frame)
  }


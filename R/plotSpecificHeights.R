library(jsonlite)

computePlotSpecificHeights <- function(measurements) {
  # {data: [ {  "Tag": "3407", "D": 40.8, "MH": 15.4, EH: <result> }], function: <string function>}

  # dat <- read.csv("nri-p005-trees.csv", header = T, na.string = "null" )

  
  dat = fromJSON(measurements)
  ## Step 1 - Calculate best height model for each stand
  "stephen test"
}
# Sample function that simulates a JSON payload that takes three arguments:

# tagAndDiameterWithHeightsFrame (tag, diameter, measure height)
# unheightedDiameterFrame (tag, diameter)
# single e_value for the plot

# and returns a frame with calculated heights and the algorithm used to compute it.
# returnFrame (tag, diameter, calculatedHeight, algorithm)
testPayload <- function() {
  tagAndDiameterWithHeightsJSON = '[{"tag": "tag1", "diameter": 30.4, "measured_height": 90 },
    {"tag": "tag2", "diameter": 25.4, "measured_height": 70 },
    {"tag": "tag3", "diameter": 20.4, "measured_height": 60 }
    ]'
  unheightedDiameterJSON = '[{"tag": "tag4", "diameter": 32.4 },
    {"tag": "tag5", "diameter": 35.4 },
    {"tag": "tag6", "diameter": 40.4 }
    ]'
  eValue = -.5
  receiveHeightsPayload(tagAndDiameterWithHeightsJSON, unheightedDiameterJSON, eValue)
}
receiveHeightsPayload <- function(tagAndDiameterWithHeightsJSON, unheightedDiameterJSON, eValue) {
  library(jsonlite)
  tagAndDiameterWithHeightsFrame = fromJSON(tagAndDiameterWithHeightsJSON)
  unheightedDiameterFrame = fromJSON(unheightedDiameterJSON)
  
  computeHeights(tagAndDiameterWithHeightsFrame, unheightedDiameterFrame, eValue )

}

computeHeights <-
  function(tagAndDiameterWithHeightsFrame, unheightedDiameterFrame, eValue)
  {
    unheightedDiameterFrame$calculatedHeight = 42
    unheightedDiameterFrame$algorithm = "answer to life the universe and everything"
    unheightedDiameterFrame

  }

autorun <- testPayload()

# Sample function that simulates a JSON payload that takes three arguments:

# tagAndDiameterWithHeightsFrame (tag, diameter, measure height)
# unheightedDiameterFrame (tag, diameter)
# single e_value for the plot

# and returns a frame with calculated heights and the algorithm used to compute it.
# returnFrame (tag, diameter, calculatedHeight, algorithm)
testPayload <- function() {
  tagAndDiameterWithHeightsJSON = '[{"Tag": "tag1", "D": 30.4, "HMean": 90 },
    {"Tag": "tag2", "D": 25.4, "HMean": 70 },
    {"Tag": "tag3", "D": 20.4, "HMean": 60 }
    ]'
  unheightedDiameterJSON = '[{"tag": "tag4", "D": 32.4 },
    {"Tag": "tag5", "D": 35.4 },
    {"Tag": "tag6", "D": 40.4 }
    ]'
  eValue = -.5
  receiveHeightsPayload(tagAndDiameterWithHeightsJSON, unheightedDiameterJSON, eValue)
}
receiveHeightsPayload <- function(tagAndDiameterWithHeightsJSON, unheightedDiameterJSON, eValue) {
  library(jsonlite)
  tagAndDiameterWithHeightsFrame = fromJSON(tagAndDiameterWithHeightsJSON)
  unheightedDiameterFrame = fromJSON(unheightedDiameterJSON)

  computeHeights(tagAndDiameterWithHeightsFrame, unheightedDiameterFrame, eValue)
 
  
}

computeHeights <-
  function(tagAndDiameterWithHeightsFrame, unheightedDiameterFrame, eValue)
  {
    fit <- heightfit(hdat = tagAndDiameterWithHeightsFrame,
                     sub.dat = unheightedDiameterFrame,
                     E_Value = eValue)
    unheightedDiameterFrame$Ht <- fit$predht
    
    DiameterHeightFrame <- unheightedDiameterFrame
    DH_vars <- fit$vars
    DH_equation <- formula(paste("y ~ ", fit$Xequ, sep = ""))
    #DH_equ2 <- substitute(expr = DH_equation, list(a = DH_vars[1], b = DH_vars[2]))
    DH_equ3 <- gsub("a", round(DH_vars[1], 3), DH_equation)
    DH_equ3 <- gsub("b", round(DH_vars[2], 3), DH_equ3)
    DH_equ3 <- gsub("c", round(DH_vars[3], 3), DH_equ3)
    DH_equ3 <- gsub("d", round(DH_vars[4], 3), DH_equ3)
    DH_equ3 <- formula(paste("y ~ ", DH_equ3[3], sep = ""))
    

  }

autorun <- testPayload()

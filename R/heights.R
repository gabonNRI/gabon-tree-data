# Sample function that simulates a JSON payload that takes three arguments:

# tagAndDiameterWithHeightsFrame (tag, diameter, measure height)
# unheightedDiameterFrame (tag, diameter)
# single e_value for the plot

# and returns a frame with calculated heights and the algorithm used to compute it.
# returnFrame (tag, diameter, calculatedHeight, algorithm)

# This payload represents NRI_P005 
testPayload <- function() {
  tagAndDiameterWithHeightsJSON = '
[ { "Tag": "7970", "D": 11.6, "HMean": 10.2 },
  { "Tag": "7971", "D": 12.5, "HMean": 17 },
  { "Tag": "7973", "D": 11.3, "HMean": 8.4 },
  { "Tag": "7974", "D": 128, "HMean": 35.4 },
  { "Tag": "7975", "D": 12.6, "HMean": 9.4 },
  { "Tag": "7976", "D": 19.4, "HMean": 11.6 },
  { "Tag": "7978", "D": 10.3, "HMean": 10.8 },
  { "Tag": "7979", "D": 71.2, "HMean": 36.8 },
  { "Tag": "7980", "D": 23.8, "HMean": 16.4 },
  { "Tag": "7982", "D": 13.6, "HMean": 13.1 },
  { "Tag": "7983", "D": 23.9, "HMean": 21 },
  { "Tag": "7984", "D": 13.9, "HMean": 17 },
  { "Tag": "7985", "D": 13.5, "HMean": 15.7 },
  { "Tag": "7987", "D": 49.3, "HMean": 21.9 },
  { "Tag": "7988", "D": 33.4, "HMean": 13.6 },
  { "Tag": "7989", "D": 24.4, "HMean": 17.4 },
  { "Tag": "7990", "D": 32.1, "HMean": 17.6 },
  { "Tag": "7993", "D": 27.7, "HMean": 24.9 },
  { "Tag": "7994", "D": 46.2, "HMean": 38.7 },
  { "Tag": "7996", "D": 21.5, "HMean": 17.5 },
  { "Tag": "7997", "D": 22, "HMean": 10 },
  { "Tag": "7998", "D": 134, "HMean": 37.9 },
  { "Tag": "8000", "D": 49.5, "HMean": 23.3 },
  { "Tag": "8383", "D": 37.9, "HMean": 24.5 },
  { "Tag": "8384", "D": 33.6, "HMean": 17.6 },
  { "Tag": "8385", "D": 24.1, "HMean": 8.2 },
  { "Tag": "8386", "D": 37.3, "HMean": 6.3 },
  { "Tag": "8387", "D": 25.9, "HMean": 11.1 },
  { "Tag": "8389", "D": 32.1, "HMean": 20.7 },
  { "Tag": "8390", "D": 66.4, "HMean": 20.5 },
  { "Tag": "8391", "D": 24, "HMean": 13.9 },
  { "Tag": "8396", "D": 82, "HMean": 19 },
  { "Tag": "601", "D": 112, "HMean": 30.2 },
  { "Tag": "612", "D": 23.1, "HMean": 6.1 },
  { "Tag": "621", "D": 48.3, "HMean": 32.3 },
  { "Tag": "622", "D": 58.2, "HMean": 32.2 },
  { "Tag": "627", "D": 110, "HMean": 34.1 } ]
  '
  unheightedDiameterJSON = '
[ { "Tag": "7972", "D": 50 },
  { "Tag": "7977", "D": 12.2 },
  { "Tag": "7986", "D": 11.8 },
  { "Tag": "7991", "D": 11.4 },
  { "Tag": "7992", "D": 12.1 },
  { "Tag": "7995", "D": 14.4 },
  { "Tag": "7999", "D": 15.9 },
  { "Tag": "8388", "D": 14.8 },
  { "Tag": "8392", "D": 18.8 },
  { "Tag": "8393", "D": 15.7 },
  { "Tag": "8394", "D": 12.5 },
  { "Tag": "8395", "D": 12.4 },
  { "Tag": "8397", "D": 11.9 },
  { "Tag": "8398", "D": 11.8 },
  { "Tag": "8399", "D": 12 },
  { "Tag": "8400", "D": 19.7 },
  { "Tag": "7981", "D": 10.1 },
  { "Tag": "602", "D": 19.5 },
  { "Tag": "603", "D": 10.8 },
  { "Tag": "604", "D": 18 },
  { "Tag": "605", "D": 11.3 },
  { "Tag": "606", "D": 14 },
  { "Tag": "607", "D": 19.4 },
  { "Tag": "608", "D": 13.6 },
  { "Tag": "609", "D": 15.6 },
  { "Tag": "610", "D": 13.2 },
  { "Tag": "611", "D": 13.2 },
  { "Tag": "613", "D": 14.1 },
  { "Tag": "614", "D": 17.1 },
  { "Tag": "615", "D": 15.7 },
  { "Tag": "616", "D": 14 },
  { "Tag": "617", "D": 15 },
  { "Tag": "618", "D": 15.4 },
  { "Tag": "619", "D": 12.2 },
  { "Tag": "620", "D": 16.2 },
  { "Tag": "623", "D": 10.4 },
  { "Tag": "624", "D": 13.8 },
  { "Tag": "625", "D": 11.7 },
  { "Tag": "626", "D": 12.8 },
  { "Tag": "628", "D": 10 },
  { "Tag": "Aux_A_0", "D": 24.1 },
  { "Tag": "Aux_A_1", "D": 18.3 },
  { "Tag": "Aux_A_2", "D": 11.2 },
  { "Tag": "Aux_A_3", "D": 11.8 },
  { "Tag": "Aux_A_4", "D": 15.4 },
  { "Tag": "Aux_A_5", "D": 13.8 },
  { "Tag": "Aux_A_6", "D": 16 },
  { "Tag": "Aux_A_7", "D": 20.1 },
  { "Tag": "Aux_A_8", "D": 15.3 },
  { "Tag": "Aux_A_9", "D": 22.9 },
  { "Tag": "Aux_B_10", "D": 11.6 },
  { "Tag": "Aux_B_11", "D": 12.9 },
  { "Tag": "Aux_B_12", "D": 18.8 },
  { "Tag": "Aux_B_13", "D": 10.2 },
  { "Tag": "Aux_B_14", "D": 20.3 },
  { "Tag": "Aux_B_15", "D": 21.4 },
  { "Tag": "Aux_B_16", "D": 17.1 },
  { "Tag": "Aux_B_17", "D": 13.8 },
  { "Tag": "Aux_B_18", "D": 11.3 },
  { "Tag": "Aux_B_19", "D": 11.5 },
  { "Tag": "Aux_B_20", "D": 13.4 },
  { "Tag": "Aux_B_21", "D": 13.3 },
  { "Tag": "Aux_B_22", "D": 10 },
  { "Tag": "Aux_B_23", "D": 26.6 },
  { "Tag": "Aux_B_24", "D": 14.7 },
  { "Tag": "Aux_B_25", "D": 24.3 },
  { "Tag": "Aux_B_26", "D": 10 },
  { "Tag": "Aux_B_27", "D": 24.2 },
  { "Tag": "Aux_B_28", "D": 12.8 },
  { "Tag": "Aux_B_29", "D": 25.6 },
  { "Tag": "Aux_B_30", "D": 16.3 },
  { "Tag": "Aux_B_31", "D": 10.3 },
  { "Tag": "Aux_B_32", "D": 10.4 },
  { "Tag": "Aux_B_33", "D": 27 },
  { "Tag": "Aux_B_34", "D": 14 },
  { "Tag": "Aux_B_36", "D": 10.4 },
  { "Tag": "Aux_B_37", "D": 17.2 },
  { "Tag": "Aux_B_38", "D": 29.4 },
  { "Tag": "Aux_B_39", "D": 18.6 },
  { "Tag": "Aux_B_40", "D": 13.1 },
  { "Tag": "Aux_B_41", "D": 21.7 },
  { "Tag": "Aux_B_42", "D": 12.6 },
  { "Tag": "Aux_B_43", "D": 11.4 },
  { "Tag": "Aux_B_44", "D": 36 },
  { "Tag": "Aux_B_45", "D": 23 },
  { "Tag": "Aux_B_46", "D": 14.8 },
  { "Tag": "Aux_B_47", "D": 15.8 },
  { "Tag": "Aux_B_48", "D": 17 },
  { "Tag": "Aux_B_49", "D": 17 },
  { "Tag": "Aux_B_50", "D": 17.7 },
  { "Tag": "Aux_B_51", "D": 19 },
  { "Tag": "Aux_B_52", "D": 10.5 },
  { "Tag": "Aux_B_53", "D": 12.8 },
  { "Tag": "Aux_B_54", "D": 15.7 },
  { "Tag": "Aux_B_55", "D": 18.4 },
  { "Tag": "Aux_B_56", "D": 24.2 },
  { "Tag": "Aux_B_57", "D": 26.6 },
  { "Tag": "Aux_B_58", "D": 13.3 },
  { "Tag": "Aux_B_59", "D": 15.5 },
  { "Tag": "Aux_B_60", "D": 14.9 },
  { "Tag": "Aux_B_61", "D": 11.2 },
  { "Tag": "Aux_B_62", "D": 12.1 },
  { "Tag": "Aux_B_63", "D": 13.9 },
  { "Tag": "Aux_B_64", "D": 15.4 },
  { "Tag": "Aux_B_65", "D": 19.6 },
  { "Tag": "Aux_B_66", "D": 15.2 },
  { "Tag": "Aux_D_67", "D": 18.7 },
  { "Tag": "Aux_D_68", "D": 34.7 },
  { "Tag": "Aux_D_69", "D": 16.1 },
  { "Tag": "Aux_D_70", "D": 20.3 },
  { "Tag": "Aux_D_71", "D": 27.7 },
  { "Tag": "Aux_D_72", "D": 61.2 },
  { "Tag": "Aux_D_73", "D": 23.3 },
  { "Tag": "Aux_D_74", "D": 21.3 },
  { "Tag": "Aux_D_76", "D": 26.5 },
  { "Tag": "Aux_D_77", "D": 54.8 },
  { "Tag": "Aux_D_78", "D": 16.1 },
  { "Tag": "Aux_D_79", "D": 15.7 },
  { "Tag": "Aux_D_80", "D": 130 },
  { "Tag": "Aux_D_81", "D": 50.3 },
  { "Tag": "Aux_D_82", "D": 27.1 },
  { "Tag": "Aux_D_83", "D": 12.2 },
  { "Tag": "Aux_D_84", "D": 110 },
  { "Tag": "Aux_D_85", "D": 21.2 },
  { "Tag": "Aux_D_86", "D": 16.3 },
  { "Tag": "Aux_D_87", "D": 14.5 },
  { "Tag": "Aux_D_88", "D": 14.3 },
  { "Tag": "Aux_D_89", "D": 29 },
  { "Tag": "Aux_D_90", "D": 35.9 },
  { "Tag": "Aux_D_91", "D": 19.2 },
  { "Tag": "Aux_D_92", "D": 17.7 } ]
  '
  eValue = 0.0358
  receiveHeightsPayload(tagAndDiameterWithHeightsJSON, unheightedDiameterJSON, eValue)
}
receiveHeightsPayload <- function(tagAndDiameterWithHeightsJSON, unheightedDiameterJSON, eValue) {
  library(jsonlite)
  tagAndDiameterWithHeightsFrame = fromJSON(tagAndDiameterWithHeightsJSON)
  unheightedDiameterFrame = fromJSON(unheightedDiameterJSON)

  answer = computeHeights(tagAndDiameterWithHeightsFrame, unheightedDiameterFrame, eValue)
  toJSON(answer)
  
}

computeHeights <-
  function(tagAndDiameterWithHeightsFrame, unheightedDiameterFrame, eValue = -0.059)
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
    payload <- list(unheightedDiameterFrame, 
                    Reduce(paste, deparse(DH_equation, width.cutoff = 500)), 
                    Reduce(paste, deparse(DH_equ3, width.cutoff = 500)))
    
    payload
    
    

  }

autorun <- testPayload()

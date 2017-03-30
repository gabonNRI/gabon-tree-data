library('devtools')
#install_github("BIOMASSR/BIOMASS")
#install('BIOMASS')
#require('BIOMASS')
#
install.packages(BIOMASS)
#install.packages("BIOMASS", repos = "https://cran.opencpu.org", method = "libcurl")
library(BIOMASS)

# Sample function that simulates a JSON payload that takes three arguments:
# This payload represents NRI_P005 
testPayloadSimple <- function() {
  D <- 10:19
  WD <- runif(length(D), min = 0.1, max = 1)
  H <- D^(2/3)
  # If you have height data
  AGB <- computeAGB(D,WD,H)
}
testPayload <- function() {
  tagAndDiameterWithHeightsJSON = '

[ {  "Tag": "3407", "D": 40.8, "H": 15.4, "WD": 0.21059999999999998 },
 {  "Tag": "3408", "D": 14.6, "H": 6.8, "WD": 0.40890000000000004 },
  {  "Tag": "3409", "D": 14.5, "H": 5.6, "WD": 0.915 },
  {  "Tag": "3410", "D": 39.7, "H": 7.7, "WD": 0.21059999999999998 },
  {  "Tag": "3411", "D": 14.7, "H": 3.7, "WD": 0.21059999999999998 },
{  "Tag": "3412", "D": 15.7, "H": 4.5, "WD": 0.35786558862981777 },
  {  "Tag": "3413", "D": 11.8, "H": 6, "WD": 0.35786558862981777 },
  {  "Tag": "3414", "D": 11.4, "H": 3.3, "WD": 0.35786558862981777 },
  {  "Tag": "3415", "D": 74.7, "H": 19.7, "WD": 0.21059999999999998 },
  {  "Tag": "3416", "D": 27.2, "H": 19.2, "WD": 0.35786558862981777 },
  {  "Tag": "3417", "D": 13.2, "H": 5.2, "WD": 0.35786558862981777 },
  {  "Tag": "3418", "D": 34.2, "H": 12.6, "WD": 0.21059999999999998 },
  {  "Tag": "3419", "D": 14.4, "H": 3.8, "WD": 0.35786558862981777 },
  {  "Tag": "3420", "D": 33.5, "H": 15.7, "WD": 0.21059999999999998 },
  {  "Tag": "3421", "D": 12.4, "H": 4.3, "WD": 0.5124000000000001 },
  {  "Tag": "3422", "D": 72, "H": 23.5, "WD": 0.21059999999999998 },
  {  "Tag": "3423", "D": 11, "H": 5.7, "WD": 0.553 },
  {  "Tag": "3427", "D": 24.9, "H": 14.3, "WD": 0.35786558862981777 },
  {  "Tag": "3432", "D": 20.3, "H": 19.2, "WD": 0.553 },
  {  "Tag": "3438", "D": 45.7, "H": 11.6, "WD": 0.21059999999999998 },
  {  "Tag": "3441", "D": 40.4, "H": 12.9, "WD": 0.35786558862981777 },
  {  "Tag": "3451", "D": 24.9, "H": 9.6, "WD": 0.35786558862981777 },
  {  "Tag": "3458", "D": 25.7, "H": 18.5, "WD": 0.6577857142857144 },
  {  "Tag": "3461", "D": 37.5, "H": 10.4, "WD": 0.5567999999999999 },
  {  "Tag": "3467", "D": 76.7, "H": 37.4, "WD": 0.35786558862981777 },
  {  "Tag": "3468", "D": 44.7, "H": 12.1, "WD": 0.35786558862981777 },
  {  "Tag": "3474", "D": 31.4, "H": 15.2, "WD": 0.21059999999999998 },
  {  "Tag": "3483", "D": 42.8, "H": 20.1, "WD": 0.24283333333333332 },
  {  "Tag": "3484", "D": 22.6, "H": 14.1, "WD": 0.35786558862981777 },
  {  "Tag": "3487", "D": 76.2, "H": 41.1, "WD": 0.35786558862981777 },
  {  "Tag": "3489", "D": 44.7, "H": 21.5, "WD": 0.749 },
  {  "Tag": "3501", "D": 24.3, "H": 15.5, "WD": 0.21059999999999998 },
  {  "Tag": "3502", "D": 65.3, "H": 25.5, "WD": 0.35786558862981777 },
  {  "Tag": "3504", "D": 20.7, "H": 10.1, "WD": 0.21059999999999998 },
  {  "Tag": "3510", "D": 25.8, "H": 18.5, "WD": 0.21059999999999998 },
  {  "Tag": "3522", "D": 30.9, "H": 18.4, "WD": 0.21059999999999998 },
  {  "Tag": "3523", "D": 30.6, "H": 16, "WD": 0.21059999999999998 },
  {  "Tag": "3529", "D": 30.5, "H": 16, "WD": 0.21059999999999998 },
  {  "Tag": "3530", "D": 29.5, "H": 16.8, "WD": 0.21059999999999998 }
]'
  latitude = -2.8433055556
  longitude = 11.57453
  eValue = 0.0358
  library(jsonlite)
  tagAndDiameterWithHeightsFrame = fromJSON(tagAndDiameterWithHeightsJSON)
  
  computeHeightsWithBiomass(tagAndDiameterWithHeightsFrame, latitude, longitude, eValue)
}
receiveHeightsPayload <- function(tagAndDiameterWithHeightsJSON, eValue) {
  library(jsonlite)
  tagAndDiameterWithHeightsFrame = fromJSON(tagAndDiameterWithHeightsJSON)
  #print(tagAndDiameterWithHeightsFrame)
  #unheightedDiameterFrame = fromJSON(unheightedDiameterJSON)
  latitude = tagAndDiameterWithHeightsFrame$latitude
  longitude = tagAndDiameterWithHeightsFrame$longitude
  #answer = computeHeights(tagAndDiameterWithHeightsFrame, unheightedDiameterFrame, eValue)
  toJSON(answer)
  
}

computeHeightsWithBiomass <- function(measurements, latitude, longitude, eValue) {
  coordinates = cbind(longitude, latitude)
  #answer = tagAndDiameterWithHeightsFrame
  agb = computeAGB(measurements$D, measurements$WD, measurements$H, coordinates)
  measurements$AGB = agb
  measurements
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
simple <- testPayloadSimple()

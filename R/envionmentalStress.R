require(raster)
require(ncdf4)
## source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")

## This assumes upload of a file called "gpsdata.csv"
testEnvStress <- function() {
  json = '[{"Longitude": "9.34106", "Latitude": 0.58583, "Code": "NRI_001", "id": "01"},
          {"Longitude": "9.32992", "Latitude": 0.57147, "Code": "NRI_002", "id": "02"}
  ]'
  testEnvStressJson(json)
}
testEnvStressJson <- function(json) {
  library(jsonlite)
  args = fromJSON(json)
  compute_evals_code(args)
}

testLocal = function() {
  gps.dat.file <- read.csv("~/repos/gabontreedata/data/gpsdata.csv", header = T, stringsAsFactors = F)
  coord = compute_evals_code(gps.dat.file)
  write.csv(coord, "~/repos/gabontreedata/data/Evals_151214.csv")
}
### Accepts a dataframe with Longitude, Latitude, Code.  Assumes that the data
### is for a principle plot, however, it will compute for any NW corner (0,0) coordinates.
compute_evals_code = function(frame) {
  ## frame1 <- frame[frame$Coordonnees %in% c("40. 40", "Mid") & frame$Placette == "Principale",]
  frame$Longitude <- as.numeric(frame$Longitude)
  frame$Latitude <- as.numeric(frame$Latitude)
  
  coord <- with(frame, cbind(longitude = Longitude, latitude = Latitude))
  print("coords")
  print(coord)
  Evals <- retrieve_raster("E", coord)
  coord <- as.data.frame(coord)
  coord$Evals <- Evals
  coord$Plot.Code <- frame$Code
  coord$mongoid <- frame$id  
  coord
}

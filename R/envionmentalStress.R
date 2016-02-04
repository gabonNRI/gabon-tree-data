require(raster)
require(ncdf4)
## source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")

## This assumes upload of a file called "gpsdata.csv"

gps.dat.file <- read.csv("~/repos/gabontreedata/R/gpsdata.csv", header = T, stringsAsFactors = F)
coord = compute_evals_code(gps.dat.file)
write.csv(coord, "Evals_151214.csv")

### Accepts a dataframe with Longitude, Latitude, Code.  Assumes that the data
### is for a principle plot, however, it will compute for any NW corner (0,0) coordinates.
compute_evals_code = function(gps.dat) {
  ## gps.dat1 <- gps.dat[gps.dat$Coordonnees %in% c("40. 40", "Mid") & gps.dat$Placette == "Principale",]
  gps.dat$Longitude <- as.numeric(gps.dat$Longitude)
  gps.dat$Latitude <- as.numeric(gps.dat$Latitude)
  
  coord <- with(gps.dat, cbind(longitude = Longitude, latitude = Latitude))
  Evals <- retrieve_raster("E", coord)
  coord <- as.data.frame(coord)
  coord$Evals <- Evals
  coord$Plot.Code <- gps.dat$Code
  coord
}

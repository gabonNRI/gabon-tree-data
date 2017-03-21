require(BIOMASS)
require(reshape2)
require(ggplot2)

setwd("~/repos/gabontreedata/workspace")

dat <- read.csv("agb0.csv")
check <- with(dat, tapply(D, list(Plot.Code2), function(x)length(x)))
bad.sat <- names(check[check < 10])
dat <- dat[!(dat$Plot.Code2 %in% bad.sat),]

hdat <- read.csv("htall0.csv")
hdat <- hdat[hdat$D >= 10, ]
hdat <- hdat[hdat$Plot.Code != "LME.04",]

## Step 1 - Taxonomy
dat$Species <- as.character(dat$Species)
sppname <- strsplit(dat$Species, " ")
dat$Spp <- sapply(sppname, "[", 2)

Taxo <- correctTaxo(genus = dat$Genus, species = dat$Spp)

dat$genusCorr <- Taxo$genusCorrected
dat$sppCorr <- Taxo$speciesCorrected

APG <- getTaxonomy(dat$genusCorr, findOrder = T)
dat$familyAPG <- APG$family
dat$orderAPG <- APG$order

## Step 2 - Wood density

WdDen <- getWoodDensity(genus = dat$genusCorr,
                        species = dat$sppCorr,
                        stand = dat$Plot.Code)

dat$WdDen <- WdDen$meanWD
cor.test(dat$WdDen, dat$WD4)
dat$sdWD <- WdDen$sdWD

## Step 3 - Calculate best height model for each stand

# Compute models for each stand
HDmod.log1 <- by(hdat, hdat$Plot.Code,
                 function(x) modelHD(D = x$D, H = x$Hmean, method = "log1", useWeight = F),
                 simplify = F)

Coeffmods <- lapply(HDmod.log1, function(x) x$coefficients)
Formmods <- sapply(HDmod.log1, function(x) x$formula)
log1 <- sapply(HDmod.log1, function(x) x$RSE)

HDmod.log2 <- by(hdat, hdat$Plot.Code,
                 function(x) modelHD(D = x$D, H = x$Hmean, method = "log2", useWeight = F),
                 simplify = F)
log2 <- sapply(HDmod.log2, function(x) x$RSE)

HDmod.log3 <- by(hdat, hdat$Plot.Code,
                 function(x) modelHD(D = x$D, H = x$Hmean, method = "log3", useWeight = F),
                 simplify = F)
log3 <- sapply(HDmod.log3, function(x) x$RSE)

HDmod.wb <- by(hdat, hdat$Plot.Code,
               function(x) modelHD(D = x$D, H = x$Hmean, method = "weibull", useWeight = F),
               simplify = F)
weibull <- sapply(HDmod.wb, function(x) x$RSE)

HDmod.mch <- by(hdat, hdat$Plot.Code,
                function(x) modelHD(D = x$D, H = x$Hmean, method = "michaelis", useWeight = F),
                simplify = F)
michaelis <- sapply(HDmod.mch, function(x) x$RSE)


HRes <- data.frame(cbind(log1, log2, log3, weibull, michaelis))
pnames <- rownames(HRes)
HRes <- data.frame(sapply(HRes, function(x) as.numeric(as.character(x))))
rownames(HRes) <- pnames
sapply(HRes, class)


min.pick <- function(x){names(HRes)[which.min(apply(x, MARGIN = 2, min))]}
minMod <- list()
minMod <- lapply(1:nrow(HRes), function(x) min.pick(HRes[x,]))
minMod <- data.frame(do.call("rbind", minMod))
minMod$Plot <- rownames(HRes)
minMod <- cbind(Mod = minMod[,1], Plot = rownames(HRes))
#write.csv(minMod, "minMod.csv")


## Step 4 - AGB Calculations: Calculates AGB by site (main + satellite)
## Calculates tree hts, propogates error, calculates AGB
## Develops height models which are used in Step 5 to get plot-level AGB

minMod <- read.csv("minMod.csv")
plot.list <- unique(as.character(dat$Plot.Code))
res.list <- list()
Hmod.list <- list()

for (i in 1:length(plot.list)){
  
  # Get D data for plot
  dat.sub <- dat[dat$Plot.Code == plot.list[i],]
  #dat.sub <- dat.sub[dat.sub$PlacetteAux == "Principale", ]
  
  # Get Ht data for plot
  hdat.sub <- hdat[hdat$Plot.Code == plot.list[i], ]
  
  # Get Ht model for plot, if no model use "CAfrica"
  meth <- minMod[minMod$Plot == plot.list[i],][[1]]
  
  if(length(meth) > 0){
    Hmod <- modelHD(D = hdat.sub$D, H = hdat.sub$Hmean, method = meth, useWeight = F)
    Hlocal <- retrieveH(D = dat.sub$D, model = Hmod)
    dat.sub$Hlocal <- Hlocal$H
    dat.sub$HtRSE <- Hlocal$RSE
  }
  
  if(length(meth) == 0){
    Hlocal <- retrieveH(D = dat.sub$D, region = "CAfrica")
    dat.sub$Hlocal <- Hlocal$H
    dat.sub$HtRSE <- Hlocal$RSE
  }
  
  Hmod.list[[i]] <- Hmod
  
  # Get AGB per tree, kg
  dat.sub$AGBtree <- computeAGB(D = dat.sub$D, WD = dat.sub$WdDen,
                                H = dat.sub$Hlocal)
  
  with(dat.sub, cor.test(WD4, WdDen))
  with(dat.sub, cor.test(Ht, Hlocal))
  with(dat.sub, cor.test(C14/1000, AGBtree))
  
  # Propogate error
  if(length(meth) > 0){
    AGBmc <- AGBmonteCarlo(D = dat.sub$D, WD = dat.sub$WdDen, H = dat.sub$Hlocal,
                           errWD = dat.sub$sdWD, HDmodel = Hmod,Dpropag = "chave2004")
  }
  
  if(length(meth) == 0){
    AGBmc <- AGBmonteCarlo(D = dat.sub$D, WD = dat.sub$WdDen, errWD = dat.sub$sdWD,
                           H = dat.sub$Hlocal, errH = dat.sub$HtRSE, Dpropag = "chave2004")
  }
  
  
  # Results
  res.list[[i]] <- list(Data = dat.sub, MeanAGB = AGBmc$meanAGB, MedAGB = AGBmc$medAGB,
                        SdAGB = AGBmc$sdAGB, CredAGB = AGBmc$credibilityAGB,
                        Area = unique(dat.sub$Area)))
}

Mn.AGB <- unlist(sapply(res.list, "[", 2))
Md.AGB <- unlist(sapply(res.list, "[", 3))
Sd.AGB <- unlist(sapply(res.list, "[", 4))
Cred.AGB <- sapply(res.list, "[", 5)
C2.5 <- sapply(Cred.AGB, "[", 1)
C97.5 <- sapply(Cred.AGB, "[", 2)

Plot.AGB <- as.data.frame(cbind(Mn.AGB, Md.AGB, Sd.AGB, C2.5, C97.5))
rownames(Plot.AGB) <- plot.list
Plot.AGB$Code <- plot.list
Plot.AGB$AGB.Mg <- with(Plot.AGB, Md.AGB/Area)

Plot.data <- sapply(res.list, "[", 1)
Plot.data <- do.call("rbind", Plot.data)

#write.csv(Plot.AGB, "Stand.Res.csv") # Need to divide by area?
#write.csv(Plot.data, "Tree.Res.csv")
#save(Hmod.list, file = "Hmod.list")

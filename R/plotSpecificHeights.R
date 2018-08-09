library(jsonlite)
library(BIOMASS)

computePlotSpecificHeights <- function(measurements) {
  # {data: [ {  "Tag": "3407", "D": 40.8, "MH": 15.4, EH: <result> }], function: <string function>}

  # dat <- read.csv("nri-p005-trees.csv", header = T, na.string = "null" )

  dat = fromJSON(measurements)

  ## Step 1 - Calculate best height model for each stand
  # Compute models for each stand

  HDmod.log1 <- with(dat, BIOMASS::modelHD(D = diameter, H = actual_height, method = "log1", useWeight = F), simplify = F)
  log1 <- HDmod.log1$RSE

  HDmod.log2 <- with(dat, BIOMASS::modelHD(D = diameter, H = actual_height, method = "log2", useWeight = F), simplify = F)
  log2 <- HDmod.log2$RSE

  HDmod.log3 <- with(dat, BIOMASS::modelHD(D = diameter, H = actual_height, method = "log3", useWeight = F), simplify = F)
  log3 <- HDmod.log3$RSE

  HDmod.wb <- with(dat, BIOMASS::modelHD(D = diameter, H = actual_height, method = "weibull", useWeight = F), simplify = F)
  weibull <- HDmod.wb$RSE

  HDmod.mch <- with(dat, BIOMASS::modelHD(D = diameter, H = actual_height, method = "michaelis", useWeight = F), simplify = F)
  michaelis <- HDmod.mch$RSE


  HRes <- data.frame(rbind(log1, log2, log3, weibull, michaelis))
  colnames(HRes) <- "RSE" 
  pnames <- rownames(HRes)
  HRes <- na.omit(HRes, cols=c("log1", "log2", "log3", "weibull", "michaelis"))

  min.pick <- function(x){rownames(HRes)[which.min(apply(x, MARGIN = 1, min))]}
  meth <- min.pick(x = HRes)

  ## Step 2 - AGB Calculations: Calculates AGB by site (main + satellite)
  ## Calculates tree hts, propogates error

  Hmod.list <- list() 
   
  if(length(meth) > 0){
    Hmod   <- with(dat, BIOMASS::modelHD(D = diameter, H = actual_height, method = meth, useWeight = F))
    Hlocal <- with(dat, retrieveH(D = diameter, model = Hmod))
    Hloc   <- Hlocal$H
    HtRSE  <- Hlocal$RSE
    Hcoef  <- Hmod$coefficients[,1]
      
    if(meth == "michaelis"){
      fmla <- paste("H ~ (", round(Hcoef[1], 2), " * D) / (", round(Hcoef[2], 2), "+ D)", sep = "")
    }
    
    if(meth == "log1"){
      fmla <- paste("log(H) ~ ", round(Hcoef[1], 2), "+", round(Hcoef[2], 2), "* log(D)", sep = "")
    }
    
    if(meth == "log2"){
      fmla <- paste("log(H) ~ ", round(Hcoef[1], 2), "+", round(Hcoef[2], 2), "* log(D) + ", 
                    round(Hcoef[3], 2), "* log(D)^2", sep = "")
    }
    
    if(meth == "log3"){
      fmla <- paste("log(H) ~ ", round(Hcoef[1], 2), "+", round(Hcoef[2], 2), "* log(D) + ", 
                    round(Hcoef[3], 2), "* log(D)^2 + ", round(Hcoef[4], 2), "* log(D)^3", sep = "")
    }

    if(meth == "weibull"){
      fmla <- paste("H ~ ", round(Hcoef[1], 2), " * (1 - exp(-(D/", round(Hcoef[2], 2), ")^", 
                    round(Hcoef[3], 2),"))", sep = "")
    }
  }

  if(length(meth) == 0){
    Hlocal <- with(dat, retrieveH(D = diameter, region = "CAfrica"))
    Hloc <- Hlocal$H
    HtRSE <- Hlocal$RSE
    fmla <- paste("H ~ 50.453 * (1 - exp(-0.0471*D*0.8120))", sep = "")
  }

  Hmod.list$tag      <- dat$tag
  Hmod.list$diameter <- dat$diameter 
  Hmod.list$PredHt   <- Hloc
  Hmod.list$RSE      <- HtRSE
  Hmod.list$formula  <- fmla
  toJSON(Hmod)

}
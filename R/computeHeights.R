
###################################################################################
## Compute heights for Gabon trees
###################################################################################
## We now have a function up that takes three arguments,
## tagAndDiameterWithHeightsFrame, unheightedDiameterFrame, eValue
## and returns a single data frame with (tag, diameter, calculatedHeight, algorithm)
## Adapt your existing code to match the signature of computeHeights in this code
##  computeHeights(tagAndDiameterWithHeightsFrame, unheightedDiameterFrame, eValue )

## 1. computeHeights should take the two dataframes (tagAndDiameterWithHeightsFrame &
## unheightedDiameterFrame) and an eValue
## 2. tagAndDiameterWithHeightsFrame(tag, diameter, measure height) &
##    unheighted DiameterFrame(tag, diameter)
## 3. return a dataframe with fields of tag, diameter, calculatedHeight, and algorithm

###################################################################################

## Step 1: Get example data for the function... this would already be completed on website

setwd("~/Documents/Projects/NRI Website")
   require(ggplot2)
   tree.dat <- read.csv("GabonTreedat_151214.csv", header = T, stringsAsFactors = F)
   height.dat <- read.csv("GabonHtdat_151214.csv", header = T, stringsAsFactors = F)
   htaux.dat <- read.csv("GabonAuxHtdat_151214.csv", header = T, stringsAsFactors = F)
    ht.list <- list()

    height.list <- sort(unique(height.dat$Plot.Code))
     plot.list <- sort(unique(tree.dat$Plot.Code))
      length(pdiff <- setdiff(plot.list, height.list))
      sort(pdiff)
       plot.pick <- 154
     sub.dat <- tree.dat[tree.dat$Plot.Code == as.character(plot.list[plot.pick]),]

    if(plot.list[plot.pick] %in% as.character(height.dat$Plot.Code)){
     subH.dat <- subset(height.dat, as.character(height.dat$Plot.Code) == as.character(plot.list[plot.pick]))
      subHa.dat <- subset(htaux.dat, as.character(htaux.dat$Plot.Code) == as.character(plot.list[plot.pick]))
     unheightedDiameterFrame <- data.frame(Tag = sub.dat$Tag, D = sub.dat$D)

     ht.dat <- merge(subH.dat, unheightedDiameterFrame, by = "Tag") # Only ~ 55 rows
     if(sum(!is.na(subH.dat$DBH)) >0 ){ht.dat <- subH.dat; ht.dat$D <- as.numeric(ht.dat$DBH)}
     if(nrow(ht.dat)<1){ht.dat <- subH.dat; names(ht.dat)[names(ht.dat)=="DBH"] = "D"}

     ht.dat<- data.frame(cbind(D = c(ht.dat$D, subHa.dat$DBH),
                               Hmean = c(ht.dat$Hmean, subHa.dat$Hmean),
                               Tag = c(ht.dat$Tag, rep(NA, nrow(subHa.dat)))))
     ht.dat <- data.frame(D = as.numeric(as.character(ht.dat$D)),
                          Hmean = as.numeric(as.character(ht.dat$Hmean)),
                          Tag = as.numeric(as.character(ht.dat$Tag)))

     #ht.dat$Plot.Code <- rep(unique(subH.dat$Plot.Code), nrow(ht.dat))
     eValue <- unique(sub.dat$Evals)

  #### Take out extreme height points with lots of leverage
     #ht.dat <- ht.dat[!(ht.dat$D > 150 & ht.dat$Plot.Code == "MYZA.75"),]
     #ht.dat <- ht.dat[!(ht.dat$D > 100 & ht.dat$Plot.Code == "MUPI.36"),]
     #ht.dat <- ht.dat[!(ht.dat$D > 50 & ht.dat$Plot.Code == "WWG.220"),]
     #ht.dat <- ht.dat[!(ht.dat$D > 120 & ht.dat$Plot.Code == "KGO.P031"),]
     #ht.dat <- ht.dat[!(ht.dat$Hmean < 15 & ht.dat$Plot.Code == "KGO.P002"),]

     tagAndDiameterWithHeightsFrame <- ht.dat
    }

## Step 2: Send data to function to calculate predicted tree heights

 source("DHModels_NRIDbase.R")

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


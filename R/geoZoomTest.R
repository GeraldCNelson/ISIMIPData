# testing of the use of extents to zoom in on particular locations using livestock data
source("R/globallyUsed.R")
library(RColorBrewer)
library(colorspace)# use pal <- choose_palette() to see what this is about
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9

northerHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-  c( -180, 180, -90, 0)
northernGhanaExtent <- c(8, 15, -2, -.5)
westAfricaxlim <- c(-20,30), ylim=c(-10,40)

#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2091
yearSpan <- paste0(l, "_", l + yearRange)
speciesName <- "cattle"

bpList <- as.data.table(read_excel("data-raw/animals/AnimalbreakpointslistRaw.xlsx"))
thiList <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.swine")
# ensemble graphics
# apply masks, can only do this to animals we have in THIlist and that have area mask raster
thiListReduced <- thiList[!thiList %in% c("thi.yak", "thi.broiler", "thi.layer")]
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

fileNameMean.masked <- paste0("data/cmip6/THI/THI_ensembleMean_masked_", speciesName, "_",  yearSpan, "_", k, ".tif")
print(paste0("fileNameMean.masked: ", fileNameMean.masked))
fileNameCV.masked <- paste0("data/cmip6/THI/THI_ensembleCV_masked_", speciesName, "_",  yearSpan, "_", k, ".tif")
print(paste0("fileNameCV.masked: ", fileNameCV.masked))
meanData <- rastfileNameMean.masked)
CVData <- rastfileNameCV.masked)
names(meanData) <- month.abb
names(CVData) <- month.abb

# plot Ensemble mean
titleText <- paste0("THI stress levels by month, ", speciesName, "\n ", yearSpan, ", SSP = ", k, ", ensemble mean")
zeroLevel <- bpList[species %in% speciesName, zeroLevel]
noStress <- bpList[species %in% speciesName, noStress]
moderateStress <- bpList[species %in% speciesName, moderateStress]
extremeStress <- bpList[species %in% speciesName, extremeStress]
col.l <- c("darkslategray1", "blue", "yellow", "red")
mapTheme <- rasterTheme(region = col.l)  
mapTheme$panel.background$col = 'white' 
myat <- c(zeroLevel, noStress, moderateStress, extremeStress, 100)

g <- levelplot(meanData,  xlim=c(5,50), ylim=c(-25,20), main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
               colorkey = list(at = myat, col = col.l, labels = c( "","No stress", "moderate stress", "extreme stress", "maximum")),
               xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))

g <- g + latticeExtra::layer(sp.polygons(wrld_land, col = "black", lwd = 0.5))
g


test <- rast"data/cmip6/tmaxMonthlySums/tmaxGT_48_ukesm1-0-ll_ssp585_2021_2030.tif")

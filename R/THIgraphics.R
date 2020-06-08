# script to do THI graphics
source("R/globallyUsed.R")
library(RColorBrewer)
library(colorspace)# use pal <- choose_palette() to see what this is about
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9

#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2091

bpList <- as.data.table(read_excel("data-raw/animals/AnimalbreakpointslistRaw.xlsx"))
thiList <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.swine")

# ensemble graphics
# apply masks, can only do this to animals we have in THIlist and that have area mask raster
thiListReduced <- thiList[!thiList %in% c("thi.yak", "thi.broiler", "thi.layer")]
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l))
    for (j in 1:length(thiListReduced)) {
      speciesName <- gsub("thi.", "", thiListReduced[j])
      fileNameMean.masked <- paste0("data/cmip6/THI/THI_ensembleMean_masked_", speciesName, "_",  yearSpan, "_", k, ".tif")
      print(paste0("fileNameMean.masked: ", fileNameMean.masked))
      fileNameCV.masked <- paste0("data/cmip6/THI/THI_ensembleCV_masked_", speciesName, "_",  yearSpan, "_", k, ".tif")
      print(paste0("fileNameCV.masked: ", fileNameCV.masked))
      meanData <- brick(fileNameMean.masked)
      CVData <- brick(fileNameCV.masked)
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
      g <- levelplot(meanData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myat, col = col.l, labels = c( "","No stress", "moderate stress", "extreme stress", "maximum")),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
      plotFileName <- paste0("graphics/cmip6/THI/THI_ensembleMean_masked_",  speciesName, "_",  yearSpan, "_", k, ".jpg")
      print(paste0("plot file name: ", plotFileName, " for species ", speciesName))
      jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
      
      # plot Ensemble CV
      titleText <- paste0("THI CV by month, ", speciesName, "\n ", yearSpan, ", SSP = ", k, ", ensemble CV")
      myat <- c(0, .5, 1.0, 1.5, 2.0)
      g <- levelplot(CVData, main = titleText, col.regions = col.l, at = myat,
                     colorkey = list(at = myat, col = col.l),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
      plotFileName <- paste0("graphics/cmip6/THI/THI_ensembleCV_masked_",   speciesName, "_",  yearSpan, "_", k, ".jpg")
      jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
    }
  }
}
# do observed data
yearRange <- 9
yearSpan <- "2001_2010"

for (j in 1:length(thiListReduced)) {  
  speciesName <- gsub("thi.", "", thiListReduced[j])
  
  fileNameMean.masked <- paste0("data/cmip6/THI/THI_masked_", speciesName, "_observed_", yearSpan, ".tif")
  print(paste0("filenamein ", fileNameMean.masked))
  meanData <- brick(fileNameMean.masked)
  names(meanData) <- month.abb
  
  # plot Ensemble mean
  titleText <- paste0("THI stress levels by month, ", speciesName, "\n ", yearSpan)
  zeroLevel <- bpList[species %in% speciesName, zeroLevel]
  noStress <- bpList[species %in% speciesName, noStress]
  moderateStress <- bpList[species %in% speciesName, moderateStress]
  extremeStress <- bpList[species %in% speciesName, extremeStress]
  col.l <- c("darkslategray1", "blue", "yellow", "red")
  mapTheme <- rasterTheme(region = col.l)  
  mapTheme$panel.background$col = 'white' 
  
  myat <- c(zeroLevel, noStress, moderateStress, extremeStress, 100)
  g <- levelplot(meanData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                 colorkey = list(at = myat, col = col.l, labels = c( "","No stress", "moderate stress", "extreme stress", "maximum")),
                 xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
  
  g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
  plotFileName <- paste0("graphics/cmip6/THI/masked_",  speciesName, "_observed_",  yearSpan, ".jpg")
  print(paste0("plot file name: ", plotFileName, " for species ", speciesName))
  jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
  print(g)
  dev.off()
}




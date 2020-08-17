# create tave - the daily average of tmax and tmin
source("R/globallyUsed.R")
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9
woptList <- list(gdal=c("COMPRESS=LZW"))

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2021

for (k in sspChoices)  {
  for (i in modelChoices)  {
    for (l in startyearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      
      filePrefix.tmax <- "tasmax_global_daily"
      filePrefix.tmin <- "tasmin_global_daily"
      fileSuffix <- paste0("_", yearSpan, ".tif")
      fileName.tmax <- paste0(locOfCMIP6ncFiles, k, "/", i, "/", modelName.lower, "_", k, "_", filePrefix.tmax, fileSuffix)
      fileName.tmin <- paste0(locOfCMIP6ncFiles,  k, "/", i, "/", modelName.lower, "_", k, "_", filePrefix.tmin, fileSuffix)
      tmax <- rast(fileName.tmax)
      tmin <- rast(fileName.tmin)
      system.time(tave <- (tmax + tmin)/2)
      outFile <- gsub("tasmax", "tave", fileName.tmax)
      print(paste0("file out: ", outFile))
      print(system.time(writeRaster(tave, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
    }
  }
}

# do observed version
startyearChoices <- 2001
modelChoices <- "observed"
for (l in startyearChoices) {
  for (i in modelChoices)  {
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    
    filePrefix.tmax <- "gswp3-w5e5_obsclim_tasmax_global_daily"
    filePrefix.tmin <- "gswp3-w5e5_obsclim_tasmin_global_daily"
    fileSuffix <- paste0("_", yearSpan, ".tif")
    fileName.tmax <- paste0(locOfCMIP6ncFiles, i, "/", filePrefix.tmax, fileSuffix)
    fileName.tmin <- paste0(locOfCMIP6ncFiles, i, "/", filePrefix.tmin, fileSuffix)
    tmax <- rast(fileName.tmax)
    tmin <- rast(fileName.tmin)
    system.time(tave <- (tmax + tmin)/2)
    outFile <- gsub("tasmax", "tave", fileName.tmax)
    print(paste0("file out: ", outFile))
    print(system.time(writeRaster(tave, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
  }
}


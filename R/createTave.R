# create tave - the daily average of tmax and tmin
source("R/globallyUsed.R")
sspChoices <- c("ssp585", "ssp126")
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startYearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 19
library(terra)
terraOptions(memfrac = .9, progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path, memfrac = .9,  
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2041

for (k in sspChoices)  {
  for (i in modelChoices)  {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      
      filePrefix.tmax <- "tasmax_global_daily"
      filePrefix.tmin <- "tasmin_global_daily"
      fileSuffix <- paste0("_", yearSpan, ".tif")
      fileName.tmax <- paste0("/Volumes/PassportMac/bigFiles/", modelName.lower, "_", k, "_", filePrefix.tmax, fileSuffix)
      fileName.tmin <- paste0("/Volumes/PassportMac/bigFiles/", modelName.lower, "_", k, "_", filePrefix.tmin, fileSuffix)
      tmax <- rast(fileName.tmax)
      tmin <- rast(fileName.tmin)
      system.time(tave <- (tmax + tmin)/2)
      outFile <- gsub("tasmax", "tave", fileName.tmax)
      print(paste0("file out: ", outFile))
      print(system.time(writeRaster(tave, outFile, overwrite = TRUE,  wopt= woptList))); flush.console()
    }
  }
}

# do observed version
startYearChoices <- 2001
modelChoices <- "observed"
for (l in startYearChoices) {
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
    print(system.time(writeRaster(tave, outFile, overwrite = TRUE,  wopt= woptList))); flush.console()
  }
}


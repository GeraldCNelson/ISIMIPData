# tave calculations
# this script converts the units of nc tmin, tmax, and pr
#source("R/globallyUsed.R")
library(data.table)
#library(raster)
library(terra)
woptList <- list(gdal=c("COMPRESS=LZW"))

modelChoices <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL") #"observed", 
startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
sspChoices <- c("ssp126", "ssp585")
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/"
yearRange <- 9
# textvals
k = "ssp126"
l = 2021
i = "GFDL-ESM4"

for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      fileName_tasmax <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower,  "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      fileName_tave <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tave_global_daily_", yearSpan, ".tif")
      tasmax <- rast(fileName_tasmax) 
      tasmin <- rast(fileName_tasmin)
      print(system.time(tave <- (tasmax + tasmin)/2))
      print(paste0("writing out: ", fileName_tave))
      writeRaster(tave, filename = fileName_tave, format = "GTiff", overwrite = TRUE, wopt = woptList)
      tasmax <- tasmin <- tave <- NULL
    }
  }
}

# do historical 
yearRange <- 9
startyearChoices <-  c(1981, 1991, 2001) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/"
modelChoices <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL") #"observed", 

  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      fileName_tasmax <- paste0(locOfFiles, i, "/", modelName.lower,  "_", "historical", "_tasmax_global_daily_", yearSpan, ".tif")
      fileName_tasmin <- paste0(locOfFiles, i, "/", modelName.lower, "_", "historical", "_tasmin_global_daily_", yearSpan, ".tif")
      fileName_tave <- paste0(locOfFiles, i, "/", modelName.lower, "_", "historical", "_tave_global_daily_", yearSpan, ".tif")
      tasmax <- rast(fileName_tasmax) 
      tasmin <- rast(fileName_tasmin)
      print(system.time(tave <- (tasmax + tasmin)/2))
      print(paste0("writing out: ", fileName_tave))
      writeRaster(tave, filename = fileName_tave, format = "GTiff", overwrite = TRUE, wopt = woptList)
      tasmax <- tasmin <- tave <- NULL
    }
  }

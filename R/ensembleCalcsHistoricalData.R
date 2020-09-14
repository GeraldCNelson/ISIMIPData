# ensemble calcs for historical data, to be used in place of 'observed' data
# combine 10 year rasters across models to get ensemble means and coeffients of variation

source("R/globallyUsed.R")
#library(raster)
woptList <- list(gdal=c("COMPRESS=LZW"))

# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
yearRange <- 9
startyearChoices <-  c(1991, 2001) #1981, 1991, 2001) 
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/"
climateVars <- c( "pr", "hurs", "tave", "rsds", "sfcwind") # "tasmax", "tasmin",
#climateVars <- c("pr", "hurs", "tave", "rsds", "sfcwind")
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
l <- 1981

readRast <- function(m) {
  model.name <- toupper(m)
  fileNameIn <- paste0(locOfFiles, model.name, "/", m,  "_historical_", j, "_global_daily_", yearSpan, ".tif")
  r <- rast(fileNameIn)
  r
}

for (j in climateVars) {
  for (l in startyearChoices) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices_mean_day <- format(indices, format = "%j")
    indices_mean_day <- as.numeric(indices_mean_day)
    
    # make a list of SpatRasters
    x <- lapply(modelChoices.lower, readRast)
    r <- rast(x)
    print(r)
    print(system.time(rast.dailyMean <- tapp(r, indices_mean_day, fun = mean))) # daily mean for the 10 year period
    fileNameOut_dailyMean <- paste0("ensemble_historical_", j, "_", yearSpan, ".tif")
    
    print(paste0("writing ensemble historical daily mean: ", fileNameOut_dailyMean))
    writeRaster(rast.dailyMean, filename = paste0(locOfFiles, "ensemble/", fileNameOut_dailyMean), format = "GTiff", overwrite = TRUE, wopt= woptList)
    
  }
}

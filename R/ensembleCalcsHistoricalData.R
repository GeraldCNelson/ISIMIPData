# ensemble calcs for historical data, to be used in place of 'observed' data
# combine 10 year rasters across models to get ensemble means and coeffients of variation

source("R/globallyUsed.R")
#library(raster)
woptList <- list(gdal=c("COMPRESS=LZW"))

# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
yearRange <- 9
startyearChoices <-  c(1991, 2001)#, 1991, 2001) #1981, 1991, 2001) 
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/"
climateVars <- c("tasmax", "tasmin",  "tave", "pr", "hurs", "rsds", "sfcwind") # "tasmax", "tasmin" 
#climateVars <- "tave"
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
l <- 2001
j = "tave"

readRast <- function(m) {
  model.name <- toupper(m)
  fileNameIn <- paste0(locOfFiles, model.name, "/", m,  "_historical_", j, "_global_daily_", yearSpan, ".tif")
  print(paste0("fileNameIn: ", fileNameIn))
  print(paste0("m: ", m))
  r <- rast(fileNameIn)
  # indices_modSpecific <- paste0(substring(m, 1, 4), indices_mean_day)
  # print(head(indices_modSpecific))
  names(r) <- indices_as_char
 print(r)
}

for (j in climateVars) {
  print(paste0("climate var: ", j))
  for (l in startyearChoices) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices_as_char <- paste0("X", indices)
    
    # make a list of SpatRasters
    x <- lapply(modelChoices.lower, readRast)
    r <- rast(x)
    print(system.time(r.mean <- tapp(r, 1:length(indices), fun = mean)))
    names(r.mean) <- indices_as_char
    print(r)
    fileNameOut_dailyMean <- paste0("ensemble_historical_", j, "_", yearSpan, ".tif")
    
    print(paste0("writing ensemble historical daily mean: ", fileNameOut_dailyMean))
    writeRaster(r.mean, filename = paste0(locOfFiles, "ensemble/", fileNameOut_dailyMean), format = "GTiff", overwrite = TRUE, wopt= woptList)
    r.mean <- x <- r <- NULL
  }
}


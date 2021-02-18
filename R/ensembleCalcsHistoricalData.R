# ensemble calcs for historical data, to be used in place of 'observed' data. 
#The end result is a single file that has daily weather data for 'yearRange' years with each observation being the mean of the equivalent observations in the ESM data.
# combine 10 year rasters across models to get ensemble means and coeffients of variation

source("R/globallyUsed.R")
#library(raster)
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
yearRange <- 9
startYearChoices <-  c(1991, 2001)#, 1991, 2001) #1981, 1991, 2001) 
startYearChoices <-  c(2001)#, 1991, 2001) #1981, 1991, 2001) 
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/"
climateVars <- c("tasmax", "tasmin",  "tas", "pr", "hurs", "rsds", "sfcwind") # "tasmax", "tasmin" 
climateVars <- "hurs"
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
l <- 1991
j = "hurs"

readRast <- function(m) {
  model.name <- toupper(m)
  fileName_in <- paste0(locOfFiles, model.name, "/", m,  "_historical_", j, "_global_daily_", yearSpan, ".tif")
  print(paste0("fileName_in: ", fileName_in))
  print(paste0("m: ", m))
  r <- rast(fileName_in)
  # indices_modSpecific <- paste0(substring(m, 1, 4), indices_mean_day)
  # print(head(indices_modSpecific))
  names(r) <- indices_as_char
 print(r)
}

for (j in climateVars) {
  print(paste0("climate var: ", j))
  for (l in startYearChoices) {
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
    fileName_out_dailyMean <- paste0("ensemble_historical_", j, "_", yearSpan, ".tif")
    
    print(paste0("writing ensemble historical daily mean: ", fileName_out_dailyMean))
    writeRaster(r.mean, filename = paste0(locOfFiles, "ensemble/", fileName_out_dailyMean), format = "GTiff", overwrite = TRUE, wopt= woptList)
    r.mean <- x <- r <- NULL
  }
}

# do 20 years

yearRange <- 19
startYearChoices <-  c(1991)#, 1991, 2001) #1981, 1991, 2001) 
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/"
climateVars <- c("tasmax", "tasmin",  "tas", "pr", "hurs", "rsds", "sfcwind") # "tasmax", "tasmin" 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
l <- 1991
j = "hurs"

readRast <- function(m) {
  model.name <- toupper(m)
  fileName_in <- paste0(locOfFiles, model.name, "/", m,  "_historical_", j, "_global_daily_", yearSpan, ".tif")
  print(paste0("fileName_in: ", fileName_in))
  print(paste0("m: ", m))
  r <- rast(fileName_in)
  # indices_modSpecific <- paste0(substring(m, 1, 4), indices_mean_day)
  # print(head(indices_modSpecific))
  names(r) <- indices_as_char
  print(r)
}

for (j in climateVars) {
  print(paste0("climate var: ", j))
  for (l in startYearChoices) {
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
    fileName_out_dailyMean <- paste0("ensemble_historical_", j, "_", yearSpan, ".tif")
    
    print(paste0("writing ensemble historical daily mean: ", fileName_out_dailyMean))
    writeRaster(r.mean, filename = paste0(locOfFiles, "ensemble/", fileName_out_dailyMean), format = "GTiff", overwrite = TRUE, wopt= woptList)
    r.mean <- x <- r <- NULL
  }
}

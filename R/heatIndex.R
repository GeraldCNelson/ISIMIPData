#  Calculate the number of growing degrees per day 
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2021
useCores <- detectCores() - 2 # max number of cores
#useCores <- 2 # better for memory intensive activities

varList <- c("startyearChoices", "sspChoices", "modelChoices", "locOfFiles", "IPCC_WG2_Ch5_crop_temperature_table")
libList <- c("raster", "ncdf4")

cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R
foreach(l = startyearChoices) %:%
  foreach(i = modelChoices) %:%
  #  foreach(j = variableChoices) %:%
  foreach(k = sspChoices)  %:%
  foreach(m = cropChoices) %dopar% {
    print(paste0("start year: ", l, " ssp: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
    tmpDirName <- paste0(locOfFiles, "rasterTmp_", Sys.getpid(), "/")
    
    rasterOptions(tmpdir = tmpDirName)
    dir.create(tmpDirName)
    
    modelName.lower <- tolower(i)
    startTime <-  Sys.time()
    yearSpan <- paste0(l, "_", l + yearRange)
    j <- "tasmax"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    temp <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    print(paste0("Working on : ", temp, " pid: ", Sys.getpid()))
    tmax <- brick(temp)
    
    j <- "hurs"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    temp <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    print(paste0("Working on : ", temp, " pid: ", Sys.getpid()))
    rh <- brick(temp)
   
    # heat index formulas use temperature in farenheit. Need to convert tmax to farenheit
#    tmax_F <- tmax * 9/5 + 32
    starttime <-  Sys.time()
    tmax_F <- calc(tmax, fun = function(x){x * 9/5 + 32})
    endtime <-  Sys.time()
    endtime - starttime
    
  #  hiSimple = 0.5 * (tmax_F + 61.0 + ((tmax_F - 68.0)*1.2) + (rh * 0.094))
    #   hiSimple <- 0.5 * (-20.6 + 2.2 * t) + (rh * 0.094))
    # hiSimple <- -10.3 + 1.1 * tmax_F + rh * 0.047 # simplified math version
    
    starttime <-  Sys.time()
    hiSimple <- overlay(tmax_F, rh, fun = function(x,y) {-10.3 + 1.1 * (x * 9/5 + 32) + y * 0.047}) # note: x * 9/5 + 32 converts celsius to fahrenheit
    endtime <-  Sys.time()
    endtime - starttime
    
    hi = -42.379 + 2.04901523 *  tmax + 10.14333127 * rh - .22475541 *  tmax * rh - .00683783 *  tmax^2 - .05481717 * rh^2 + 
      .00122874 *  tmax^2 * rh + .00085282 *  tmax * rh^2 - .00000199 *  tmax^2 * rh^2
    
    endTime <-  Sys.time()
    endTime - startTime
    fileOutLoc <- "data/cmip6/growingDegreeDays/"
    fileNameOut <-    paste(modelName.lower, k, "gdd", "global_daily", yearSpan, sep = "_")
    writeRaster(gdd, filename = paste0(fileOutLoc, fileNameOut, ".tif"), format = "GTiff", overwrite = TRUE)
    
    unlink(tmpDirName, recursive = TRUE)
    gc()
    
  }
stopCluster(cl)

# do same calculations on observed data


gc(TRUE)

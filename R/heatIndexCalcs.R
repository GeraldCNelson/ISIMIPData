#  Calculate the number of growing degrees per day 
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9
hifileOutLoc <- "data/cmip6/heatIndex/"


tStressF <- 100 # in farenheit
tStress <- (tStressF- 32) * 5/9 # convert to C

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2021
m <- "Wheat"
useCores <- detectCores() - 2 # max number of cores
#useCores <- 2 # better for memory intensive activities

varList <- c("startyearChoices", "sspChoices", "modelChoices", "locOfFiles", "ann_crop_temp_table")
libList <- c("raster", "ncdf4")

cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R
foreach(l = startyearChoices) %:%
  foreach(i = modelChoices) %:%
  #  foreach(j = variableChoices) %:%
  foreach(k = sspChoices)  %:%
  foreach(m = cropChoices) %dopar% {
    print(paste0("start year: ", l, " ssp: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
    
    modelName.lower <- tolower(i)
    startTime <-  Sys.time()
    yearSpan <- paste0(l, "_", l + yearRange)
    j <- "tasmax"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    tmaxFile <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    # print(paste0("Working on : ", temp, " pid: ", Sys.getpid()))
    # tmax <- rasttemp)
    
    j <- "hurs"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    rhFile <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    print(paste0("Working on : ", temp, " pid: ", Sys.getpid()))
    #      rh <- rasttemp)
    hiFilesCompleted <- list.files(hifileOutLoc)
    hiFilesCompleted <- hiFilesCompleted[!grepl("aux.xml", hiFilesCompleted, fixed = TRUE)]
    
    # check if global daily HI file already exists
    fileNameOut <-    paste(modelName.lower, "_heatIndexSimple_", "global_daily_", yearSpan, ".tif")
    if (!paste0(hifileOutLoc, fileNameOut) %in% hiFilesCompleted) {
      
      startTime <-  Sys.time()
      tmaxRhIn(tmaxFile, rhFile)
      endTime <-  Sys.time()
      print(paste("time to read in tmax and rh: ", round(difftime(endTime, startTime, units = "mins")), " mins"))
      
      # heat index formulas use temperature in fahrenheit. Need to convert tmax to farenheit. Not needed for the moment. Have converted formula to use tmax in c.
      #    tmax_F <- tmax * 9/5 + 32
      # startTime <-  Sys.time()
      # f.tmaxF <- function(tmax) {
      #   tmax_F <- tmax * 9/5 + 32
      # }
      # system.time(tmax_F <- setValues(tmax, f.tmaxF(values(tmax))))
      
      # endTime <-  Sys.time()
      # print(paste("time to create F version of Tmax: ", round(difftime(endTime, startTime, units = "mins")), " mins"))
      
      #  hiSimple = 0.5 * (tmax_F + 61.0 + ((tmax_F - 68.0)*1.2) + (rh * 0.094))
      #   hiSimple <- 0.5 * (-20.6 + 2.2 * t) + (rh * 0.094))
      # hiSimple <- -10.3 + 1.1 * tmax_F + rh * 0.047 # simplified math version
      
      f.hiSimple <- function(tmax, rh){
        hiSimple <-  24.9 + 0.047 * rh + 1.98 * tmax
      }
      startTime <-  Sys.time()
      hiSimple <- f.hiSimple(tmax_F, rh)
      system.time(hiSimple <- setValues(tmax_F, f.hiSimple(values(tmax_F), values(rh))))
      names(hiSimple) <- names(tmax)
      
      #    hiSimple <- overlay(tmax_F, rh, fun = function(x,y) {-10.3 + 1.1 * (x * 9/5 + 32) + y * 0.047}) # note: x * 9/5 + 32 converts celsius to fahrenheit
      endTime <-  Sys.time()
      print(paste("time to create hiSimple: ", round(difftime(endTime, startTime, units = "mins"), digits = 2), " mins"))
      
      # hi = -42.379 + 2.04901523 *  tmax + 10.14333127 * rh - .22475541 *  tmax * rh - .00683783 *  tmax^2 - .05481717 * rh^2 + 
      #   .00122874 *  tmax^2 * rh + .00085282 *  tmax * rh^2 - .00000199 *  tmax^2 * rh^2
      # 
      fileOutLoc <- "data/cmip6/heatIndex/"
      writeRaster(hiSimple, filename = paste0(fileOutLoc, fileNameOut), format = "GTiff", overwrite = TRUE)
    }

for (o in 1:length(cropChoices)) {
  for (m in get(cropChoices[o])) {
    cropName <- m
    fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(m), ".tif")
    cropMask <- rast(fileNameMask.in)
    hiMasked <- overlay(hiSimple, cropMask, fun = overlayfunction_mask)
    names(hiMasked) <- names(hiSimple)
    indices <- format(as.Date(names(hiMasked), format = "X%Y.%m.%d"), format = "%j") # %j is day of the year
    indices <- as.numeric(indices)
    
    cropCalendarName <- ann_crop_temp_table[crop %in% cropName, crop.calendar]
    cropCalFilesLoc <- paste0("data-raw/crops/cropCalendars/ALL_CROPS_netCDF_0.5deg_filled/")
    fileInName <- paste0(cropCalendarName, ".crop.calendar.fill.nc")
    locNFileIn <- paste0(cropCalFilesLoc, fileInName)
    R.utils::gunzip(paste0(locNFileIn, ".gz"), remove = FALSE)
    
    croppingCalendar_plant <- rast(locNFileIn, var = "plant")
    croppingCalendar_harvest <- rast(locNFileIn, var = "harvest")
    calendarDelta <- croppingCalendar_harvest - croppingCalendar_plant
    calendarDeltaMasked <- overlay(calendarDelta, cropMask, fun = overlayfunction_mask)

  }
}

  }
stopCluster(cl)

# do same calculations on observed data



#  Calculate the number of growing degrees per day 
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

# locOfFiles <- locOfCMIP6tifFiles
sspChoices <- c("ssp126", "ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startYearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

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
useCores <- 2 # better for memory intensive activities

varList <- c("startYearChoices", "sspChoices", "modelChoices", "locOfFiles", "cropCharacteristics_annual")
libList <- c("terra", "ncdf4")

cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R
foreach(l = startYearChoices) %:%
  foreach(i = modelChoices) %:%
  foreach(k = sspChoices)  %:%
  foreach(m = cropChoices) %dopar% {
    print(paste0("start year: ", l, " ssp: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
    terraOptions(memfrac = 1,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE)
    modelName.lower <- tolower(i)
    yearSpan <- paste0(l, "_", l + yearRange)
    j <- "tasmax"
    fileName_in <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileName_in <- paste0(fileName_in, ".tif")
    tmaxFile <- paste0(locOfFiles, k,"/", i, "/", fileName_in)
    
    j <- "hurs"
    fileName_in <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileName_in <- paste0(fileName_in, ".tif")
    rhFile <- paste0(locOfFiles, k,"/", i, "/", fileName_in)
    hiFilesCompleted <- list.files(hifileOutLoc)
    hiFilesCompleted <- hiFilesCompleted[!grepl("aux.xml", hiFilesCompleted, fixed = TRUE)]
    
    # check if global daily HI file already exists
    fileName_out <-    paste0(modelName.lower, "_heatIndexSimple_", "global_daily_", yearSpan, ".tif")
    if (!paste0(hifileOutLoc, fileName_out) %in% hiFilesCompleted) {
      
      print(system.time(tmaxRhIn(tmaxFile, rhFile)))
      
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
      print(system.time(hiSimple <- f.hiSimple(tmax, rh)))
      startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      names(hiSimple) <- indices
      
      #    hiSimple <- overlay(tmax_F, rh, fun = function(x,y) {-10.3 + 1.1 * (x * 9/5 + 32) + y * 0.047}) # note: x * 9/5 + 32 converts celsius to fahrenheit
      
      # hi = -42.379 + 2.04901523 *  tmax + 10.14333127 * rh - .22475541 *  tmax * rh - .00683783 *  tmax^2 - .05481717 * rh^2 + 
      #   .00122874 *  tmax^2 * rh + .00085282 *  tmax * rh^2 - .00000199 *  tmax^2 * rh^2
      # 
      fileOutLoc <- "data/cmip6/heatIndex/"
      print(paste0("Writing out : " , paste0(fileOutLoc, fileName_out)))
      writeRaster(hiSimple, filename = paste0(fileOutLoc, fileName_out), format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      
    }
    
    #    for (o in 1:length(cropChoices)) {
    #      for (m in get(cropChoices[o])) {
    #        cropName <- m
    #        fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(m), ".tif")
    #        cropMask <- rast(fileNameMask.in)
    #        print(system.time(hiMasked <- crop(hiSimple, cropMask)))
    # #       names(hiMasked) <- names(hiSimple)
    #        indices <- format(as.Date(names(hiMasked), format = "X%Y.%m.%d"), format = "%j") # %j is day of the year
    #        indices <- as.numeric(indices)
    #        
    #        cropCalFilesLoc <- paste0("data-raw/crops/cropCalendars/ALL_CROPS_netCDF_0.5deg_filled/")
    #        fileName_in <- paste0(cropCalendarName, ".crop.calendar.fill.nc")
    #        locNFileIn <- paste0(cropCalFilesLoc, fileName_in)
    #        R.utils::gunzip(paste0(locNFileIn, ".gz"), remove = FALSE, overwrite = TRUE)
    #        
    #        temp <- rast(locNFileIn)
    #        croppingCalendar_plant <- temp$plant
    #        croppingCalendar_harvest <- temp$harvest
    #        calendarDelta <- croppingCalendar_harvest - croppingCalendar_plant
    #        calendarDeltaMasked <- mask(calendarDelta, cropMask)
    #        
    #      }
    #    }
  }
stopCluster(cl)

# do same calculations on historical data
yearRange <- 9
l = 2001
yearSpan <- paste0(l, "_", l + yearRange)

print(paste0("start year: ", l, " ssp: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))

yearSpan <- paste0(l, "_", l + yearRange)
# j <- tasmax.observed
# fileName_in <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
# fileName_in <- paste0(fileName_in, ".tif")
tmaxFile <- tasmax.historical

# j <- "hurs"
# fileName_in <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
# fileName_in <- paste0(fileName_in, ".tif")
rhFile <-hurs.historical
hiFilesCompleted <- list.files(hifileOutLoc)
hiFilesCompleted <- hiFilesCompleted[!grepl("aux.xml", hiFilesCompleted, fixed = TRUE)]

# check if global daily HI file already exists
fileName_out <-    paste("observed", "_heatIndexSimple_", "global_daily_", yearSpan, ".tif")
if (!paste0(hifileOutLoc, fileName_out) %in% hiFilesCompleted) {
  
  print(system.time(tmaxRhIn(tmaxFile, rhFile)))
  
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
  print(system.time(hiSimple <- f.hiSimple(tmax, rh)))
  startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
  indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  indices <- paste0("X", as.character(indices))
  names(hiSimple) <- indices
  
  #    hiSimple <- overlay(tmax_F, rh, fun = function(x,y) {-10.3 + 1.1 * (x * 9/5 + 32) + y * 0.047}) # note: x * 9/5 + 32 converts celsius to fahrenheit
  
  # hi = -42.379 + 2.04901523 *  tmax + 10.14333127 * rh - .22475541 *  tmax * rh - .00683783 *  tmax^2 - .05481717 * rh^2 + 
  #   .00122874 *  tmax^2 * rh + .00085282 *  tmax * rh^2 - .00000199 *  tmax^2 * rh^2
  # 
  fileOutLoc <- "data/cmip6/heatIndex/"
  print(paste0("Writing out : ", paste0(fileOutLoc, fileName_out)))
  writeRaster(hiSimple, filename = paste0(fileOutLoc, fileName_out), format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
}

# # do same calculations on observed data
# yearRange <- 9
# l = 2001
# yearSpan <- paste0(l, "_", l + yearRange)
# 
# print(paste0("start year: ", l, " ssp: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
# 
# yearSpan <- paste0(l, "_", l + yearRange)
# # j <- tasmax.observed
# # fileName_in <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
# # fileName_in <- paste0(fileName_in, ".tif")
# tmaxFile <- tasmax.observed
# 
# # j <- "hurs"
# # fileName_in <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
# # fileName_in <- paste0(fileName_in, ".tif")
# rhFile <-hurs.observed
# hiFilesCompleted <- list.files(hifileOutLoc)
# hiFilesCompleted <- hiFilesCompleted[!grepl("aux.xml", hiFilesCompleted, fixed = TRUE)]
# 
# # check if global daily HI file already exists
# fileName_out <-    paste("observed", "_heatIndexSimple_", "global_daily_", yearSpan, ".tif")
# if (!paste0(hifileOutLoc, fileName_out) %in% hiFilesCompleted) {
#   
#   print(system.time(tmaxRhIn(tmaxFile, rhFile)))
#   
#   # heat index formulas use temperature in fahrenheit. Need to convert tmax to farenheit. Not needed for the moment. Have converted formula to use tmax in c.
#   #    tmax_F <- tmax * 9/5 + 32
#   # startTime <-  Sys.time()
#   # f.tmaxF <- function(tmax) {
#   #   tmax_F <- tmax * 9/5 + 32
#   # }
#   # system.time(tmax_F <- setValues(tmax, f.tmaxF(values(tmax))))
#   
#   # endTime <-  Sys.time()
#   # print(paste("time to create F version of Tmax: ", round(difftime(endTime, startTime, units = "mins")), " mins"))
#   
#   #  hiSimple = 0.5 * (tmax_F + 61.0 + ((tmax_F - 68.0)*1.2) + (rh * 0.094))
#   #   hiSimple <- 0.5 * (-20.6 + 2.2 * t) + (rh * 0.094))
#   # hiSimple <- -10.3 + 1.1 * tmax_F + rh * 0.047 # simplified math version
#   
#   f.hiSimple <- function(tmax, rh){
#     hiSimple <-  24.9 + 0.047 * rh + 1.98 * tmax
#   }
#   print(system.time(hiSimple <- f.hiSimple(tmax, rh)))
#   startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
#   indices <- seq(as.Date(startDate), as.Date(endDate), 1)
#   indices <- paste0("X", as.character(indices))
#   names(hiSimple) <- indices
#   
#   #    hiSimple <- overlay(tmax_F, rh, fun = function(x,y) {-10.3 + 1.1 * (x * 9/5 + 32) + y * 0.047}) # note: x * 9/5 + 32 converts celsius to fahrenheit
#   
#   # hi = -42.379 + 2.04901523 *  tmax + 10.14333127 * rh - .22475541 *  tmax * rh - .00683783 *  tmax^2 - .05481717 * rh^2 + 
#   #   .00122874 *  tmax^2 * rh + .00085282 *  tmax * rh^2 - .00000199 *  tmax^2 * rh^2
#   # 
#   fileOutLoc <- "data/cmip6/heatIndex/"
#   print(paste0("Writing out : ", paste0(fileOutLoc, fileName_out)))
#   writeRaster(hiSimple, filename = paste0(fileOutLoc, fileName_out), format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
# }



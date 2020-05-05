#  Calculate the number of growing degrees per day 
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9

IPCC_WG2_Ch5_crop_temperature_table <- as.data.table(read_excel("data-raw/crops/Crop_temperature_table_summary_02052020.xlsx", range = "A1:S26"))
cropChoices <- unique(IPCC_WG2_Ch5_crop_temperature_table$crop)

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
    
    Tbase <- IPCC_WG2_Ch5_crop_temperature_table[crop %in% m, Tbase]
    Tbase_max <- IPCC_WG2_Ch5_crop_temperature_table[crop %in% m, Tbase_max]
    tmax_clamped <- clamp(tmax, lower = Tbase, upper = Tbase_max, useValues = TRUE)
    
    j <- "tasmin"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    temp <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    print(paste0("Working on : ", temp, " pid: ", Sys.getpid()))
    tmin <- brick(temp)
    tmin_clamped <- clamp(tmin, lower = Tbase, upper = Tbase_max, useValues = TRUE)
    
    
    j <- "hurs"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    temp <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    print(paste0("Working on : ", temp, " pid: ", Sys.getpid()))
    rh <- brick(temp)
    
    
    indices <- format(as.Date(names(tmax_clamped), format = "X%Y.%m.%d"), format = "%j") # %j is day of the year
    indices <- as.numeric(indices)
    
    # heat index formulas use temperature in farenheit. Need to conver tmax to farenheit
    tmax_F <- tmax * 9/5 + 32
  #  hiSimple = 0.5 * (tmax_F + 61.0 + ((tmax_F - 68.0)*1.2) + (rh * 0.094))
    #   hiSimple <- 0.5 * (-20.6 + 2.2 * t) + (rh * 0.094))
    hiSimple <- -10.3 + 1.1 * tmax_F + rh * 0.047 # simplified math version
    
    hi = -42.379 + 2.04901523 *  tmax + 10.14333127 * rh - .22475541 *  tmax * rh - .00683783 *  tmax^2 - .05481717 * rh^2 + 
      .00122874 *  tmax^2 * rh + .00085282 *  tmax * rh^2 - .00000199 *  tmax^2 * rh^2
    
    endTime <-  Sys.time()
    endTime - startTime
    fileOutLoc <- "data/cmip6/growingDegreeDays/"
    fileNameOut <-    paste(modelName.lower, k, "gdd", "global_daily", yearSpan, sep = "_")
    writeRaster(gdd, filename = paste0(fileOutLoc, fileNameOut, ".tif"), format = "GTiff", overwrite = TRUE)
    
    cropName <- m
    filesLoc <- "data-raw/crops/cropCalendars/ALL_CROPS_netCDF_0.5deg_filled/"
    fileInName <- paste0(cropName, ".crop.calendar.fill.nc")
    #    locNFileIn <- paste0(filesLoc, fileInName, ".gz")
    locNFileIn <- paste0(filesLoc, fileInName)
    R.utils::gunzip(paste0(locNFileIn, ".gz"), remove = FALSE)
    
    croppingCalendar_plantstart <- readAll(raster(locNFileIn, var = "plant.start"))
    croppingCalendar_plantend <-  readAll(raster(locNFileIn, var = "plant.end"))
    # croppingCalendar_plantrange <- raster(locNFileIn, var = "plant.range")
    # croppingCalendar_plant <- raster(locNFileIn, var = "plant")
    # croppingCalendar_harvest <- raster(locNFileIn, var = "harvest")
    # croppingCalendar_harveststart <- raster(locNFileIn, var = "harvest.start")
    # croppingCalendar_harvestend <- raster(locNFileIn, var = "harvest.end")
    # 
    
    unlink(locNFileIn) # delete the .nc file when no longer needed.
    
    #  b <- brick(croppingCalendar_plantstart, croppingCalendar_plantend, gdd)
    starttime <- Sys.time()
    s <- stack(croppingCalendar_plantstart, croppingCalendar_plantend, gdd)
    endtime <- Sys.time()
    endtime - starttime
    starttime <- Sys.time()
    x <- calc(s, fun = function(x) sum(x[(x[1]:x[2]) + 2]))
    endtime <- Sys.time()
    endtime - starttime
    
    for (i in 1:nrow(croppingCalendar_plantstart)) { 
      for (j in 1:ncol(croppingCalendar_plantend)) { 
        print(paste0("row: ", i, " column :", j))
        start <- croppingCalendar_plantstart[i,j] # get the starting day
        end <- croppingCalendar_plantend[i,j] # get the ending day
        print(paste0("start: ", start, " end: ", end))
        
        datasum.sub1 <- stackApply(gdd[[start:end]], indices, fun = sum) 
        print(datasum.sub1)
      } 
    }
    
    indices <- format(as.Date(names(tmin), format = "X%Y.%m.%d"), format = "%m")
    indices <- as.numeric(indices)
    monthZeroCount <- stackApply(tmin, indices, fun = function(x, ...){sum(x <= 0)}) 
    names(monthZeroCount) <- month.abb
    fileNameOutZero <- paste0("belowZeroCount_", modelName.lower, "_", k, "_", yearSpan, ".tif")
    writeRaster(monthZeroCount, filename = paste0("data/cmip6/belowZero/", fileNameOutZero), format = "GTiff", overwrite = TRUE)
    
    # now do count above tmax limit
    f.tmaxLimit <- function(tmax, tmaxLimit) {
      tmaxSum <- stackApply(tmax, indices, fun = function(x, ...){sum(x >= tmaxLimit)}) 
      names(tmaxSum) <- month.abb
      fileNameOut <- paste0("tmaxGT_", tmaxLimit, "_", modelName.lower, "_", k, "_", yearSpan, ".tif")
      writeRaster(tmaxSum, filename = paste0("data/cmip6/tmaxMonthlySums/", fileNameOut), format = "GTiff", overwrite = TRUE)
    }
    tmaxfunctionStart <- Sys.time()
    #tmax > 31
    f.tmaxLimit(tmax, tmaxLimit = 31)
    tmaxfunctionEnd <- Sys.time()
    print(difftime(Sys.time(), tmaxfunctionStart, units = "mins"))
    
    print(paste("One tmax function loop", " pid: ", Sys.getpid()))
    print(tmaxfunctionEnd - tmaxfunctionStart) 
    
    
    
    rm(list = c("tmax", "tmin"))
    
    
    writeRaster(chillHrsNorthernHem, filename = paste0("data/cmip6/chillingHours/", fileNameNH), format = "GTiff", overwrite = TRUE)
    writeRaster(chillHrsSouthernHem, filename = paste0("data/cmip6/chillingHours/", fileNameSH), format = "GTiff", overwrite = TRUE)
    unlink(tmpDirName, recursive = TRUE)
    gc()
    
  }
stopCluster(cl)

# do same calculations on observed data
tmax <- tasmax.observed
tmin <- tasmin.observed
# tmin <- readAll(brick(tmin))
# tmax <- readAll(brick(tmax))
print("done with readAll tmin and tmax")
# tmin <- fixUnits(var = "tmin", ncin.brick = tmin) # fixes temp and precip units; assumes ncin.brick values are raw units
# tmax <- fixUnits(var = "tmax", ncin.brick = tmin) # fixes temp and precip units; assumes ncin.brick values are raw units
yearSpan <- "2001_2010"

chillHrs <- overlay(tmin, tmax, fun = f.chillhrs)
names(chillHrs) <- names(tmax) # put the date info back into the names

# do several count days in a month
# first days with temp below zero
print("Done with chillHrs function")
indices <- format(as.Date(names(tmin), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
monthZeroCount <- stackApply(tmin, indices, fun = function(x, ...){sum(x <= 0)}) 
names(monthZeroCount) <- month.abb
fileNameOutZero <- paste0("belowZeroCount", "_observed_", yearSpan, ".tif")
writeRaster(monthZeroCount, filename = paste0("data/cmip6/belowZero/", fileNameOutZero), format = "GTiff", overwrite = TRUE)

# now do count above tmax limit
f.tmaxLimit <- function(tmax, tmaxLimit, indices) {
  tmaxSum <- stackApply(tmax, indices, fun = function(x, ...){sum(x >= tmaxLimit)}) 
  names(tmaxSum) <- month.abb
  fileNameOut <- paste0("tmaxGT_", tmaxLimit, "_observed_", yearSpan, ".tif")
  writeRaster(tmaxSum, filename = paste0("data/cmip6/tmaxMonthlySums/", fileNameOut), format = "GTiff", overwrite = TRUE)
}
tmaxfunctionStart <- Sys.time()
#tmax > 31
f.tmaxLimit(tmax, tmaxLimit = 31, indices)
print(paste("Completed tmaxlimit for 31C"))
#tmax > 35
f.tmaxLimit(tmax, tmaxLimit = 35, indices)
#tmax > 38
f.tmaxLimit(tmax, tmaxLimit = 38, indices)
#tmax > 45
f.tmaxLimit(tmax, tmaxLimit = 45, indices)
#tmax > 48
f.tmaxLimit(tmax, tmaxLimit = 48, indices)
print(paste("Completed tmaxlimit for 48C"))

rm(list = c("tmax", "tmin"))
chillHrs.sumMonth <- stackApply(chillHrs, indices, fun = sum, na.rm = TRUE)
chillHrs.sumMonth <- chillHrs.sumMonth/10 # to get to the monthly average over 10 years
names(chillHrs.sumMonth) <- month.abb
chillHrsNorthernHem <- dropLayer(chillHrs.sumMonth, southernHemWinter) # note dropping layers for southern hemisphere winter
chillHrsSouthernHem <- dropLayer(chillHrs.sumMonth, northernHemWinter) # note dropping layers for northern hemisphere winter
chillHrsNorthernHem <- sum(chillHrsNorthernHem)
chillHrsSouthernHem <- sum(chillHrsSouthernHem)
endCompleteLoop <- Sys.time()

#print(endCompleteLoop - startTime)

fileNameNH <- paste0("chillHrsNorthernHem", "_observed_", yearSpan, ".tif")
fileNameSH <- paste0("chillHrsSouthernHem", "_observed_", yearSpan, ".tif")

writeRaster(chillHrsNorthernHem, filename = paste0("data/cmip6/chillingHours/", fileNameNH), format = "GTiff", overwrite = TRUE)
writeRaster(chillHrsSouthernHem, filename = paste0("data/cmip6/chillingHours/", fileNameSH), format = "GTiff", overwrite = TRUE)

gc(TRUE)

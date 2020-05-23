#  Calculate the number of growing degrees per day 
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, 
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
hemisphereList <- c("Northern", "Southern")
northerHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-  c( -180, 180, -90, 0)

yearRange <- 9

# commented out, now in the globallyUsed.R script
#IPCC_WG2_Ch5_crop_temperature_table <- as.data.table(read_excel("data-raw/crops/Crop_temperature_table_summary_02052020.xlsx", range = "A1:S26"))
data.table::setnames(IPCC_WG2_Ch5_crop_temperature_table, old = names(IPCC_WG2_Ch5_crop_temperature_table), new = make.names(names(IPCC_WG2_Ch5_crop_temperature_table)))
cropChoices <- unique(IPCC_WG2_Ch5_crop_temperature_table$crop)
#cropChoices <- c("Barley")
#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
m <- "Barley"
useCores <- detectCores() - 2 # max number of cores
useCores <- 2 # better for memory intensive activities

varList <- c("startyearChoices", "sspChoices", "modelChoices", "locOfFiles", "IPCC_WG2_Ch5_crop_temperature_table", "cropChoices")
libList <- c("raster", "ncdf4", "data.table")

cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R
foreach(k = sspChoices)  %:%
  foreach(l = startyearChoices) %:%
  foreach(i = modelChoices) %dopar% {
    print(paste0("start year: ", l, " ssp: ", k,  " model: ", i, " start year: ", l, " ssp choice: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
    # tmpDirName <- paste0(locOfFiles, "rasterTmp_", Sys.getpid(), "/")
    # 
    # rasterOptions(tmpdir = tmpDirName)
    # dir.create(tmpDirName)
    # 
    modelName.lower <- tolower(i)
    startTime <-  Sys.time()
    yearSpan <- paste0(l, "_", l + yearRange)
    j <- "tasmax"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    temp <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    print(paste0("Working on : ", temp, " pid: ", Sys.getpid()))
    tmax <- readAll(brick(temp))
    print(paste0("tmax brick created, ", temp,  " pid: ", Sys.getpid()))
    
    j <- "tasmin"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    temp <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    tmin <- readAll(brick(temp))
    print(paste0("tmin brick created, ", temp, " pid: ", Sys.getpid()))
    
    for (m in cropChoices) {
     
      
    }
    
    unlink(tmpDirName, recursive = TRUE)
    gc()
    
  }
stopCluster(cl)

# code above generates gdds for each day for a specific crop
# code below sums over the cropping calender to get gdds during the calendar

gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"
modelName.lower <- tolower(i)
yearSpan <- paste0(l, "_", l + yearRange)

for (m in cropChoices) {
  gddIn_crop <- paste0(gddsfileOutLoc, modelName.lower, "_", m, "_", k, "_gdd", "_global_daily_", yearSpan, ".tif")
  gdd <- brick(gddIn_crop)
  names(gdd) <- readRDS("data-raw/ISIMIP/ISIMIPLayerNames.RDS")
  indices <- format(as.Date(names(gdd), format = "X%Y.%m.%d"), format = "%j") # %j is day of the year
  indices <- as.numeric(indices)
  
  startTime <-  Sys.time()
  gdd_north1 <- crop(gdd, extent(northerHemExtent))
  endTime <-  Sys.time()
  endTime - startTime
  
  startTime <-  Sys.time()
  gdd_north2 <- gdd[1:180, 1:720, drop = FALSE]
  endTime <-  Sys.time()
  endTime - startTime
  
    # crop the gdd brick to just the area of the crop from the 
  
  # cropArea <- getcropAreaYield(cropName = tolower(m), dataType = "area")
  # rInArea <- raster(cropArea)
  # rInAreaAgg <- aggregate(rInArea, fact = 6, fun = "sum")
  # cutoff <- 1000 # only include 1/2 cells where crop area is great than cutoff
  # rInAreaAgg[rInAreaAgg < cutoff] <- NA
  # rInAreaAgg[rInAreaAgg >= cutoff] <- 1
  
  #
  
  # cropping calendar ncs have the following variables - index, filled.index, plant, plant.start, plant.end, plant.range, harvest, harvest.start, harvest.end, harvest.range, tot.days
  # The following variables are included in all files:
  #   - plant: mean planting day of year
  # - plant.start: day of year of start of planting period
  # - plant.end: day of year of end of planting period
  # - plant.range: number of days between start and end of planting period
  # - harvest: mean harvest day of year
  # - harvest.start: day of year of start of harvest period
  # - harvest.end: day of year of end of harvest period
  # - harvest.range: number of days between start and end of harvest period
  # - tot.days: number of days between planting and harvest
  # Source: https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/netCDF0-5degree.php
  cropName <- m
  fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(m), ".tif")
  mask <- raster(fileNameMask.in)
  
  cropCalendarName <- IPCC_WG2_Ch5_crop_temperature_table[crop %in% cropName, crop.calendar]
  cropCalFilesLoc <- paste0("data-raw/crops/cropCalendars/ALL_CROPS_netCDF_0.5deg_filled/")
  fileInName <- paste0(cropCalendarName, ".crop.calendar.fill.nc")
  #    locNFileIn <- paste0(filesLoc, fileInName, ".gz")
  locNFileIn <- paste0(cropCalFilesLoc, fileInName)
  R.utils::gunzip(paste0(locNFileIn, ".gz"), remove = FALSE)
  
  croppingCalendar_plant <- raster(locNFileIn, var = "plant")
  croppingCalendar_harvest <- raster(locNFileIn, var = "harvest")
  croppingCalendar_plant_crop <- mask(croppingCalendar_plant, mask)
  croppingCalendar_plant_crop_north <- crop(croppingCalendar_plant_crop, extent(northerHemExtent))
  
  croppingCalendar_harvest_crop <- mask(croppingCalendar_harvest, mask)
  croppingCalendar_harvest_crop_north <- crop(croppingCalendar_harvest_crop, extent(northerHemExtent))
  
  croppingCalendar_plant_crop <- mask(croppingCalendar_plant, mask)
  croppingCalendar_harvest_crop <- mask(croppingCalendar_harvest, mask)
  cal <- readAll(stack(croppingCalendar_plant_crop, croppingCalendar_harvest_crop))
  unlink(locNFileIn) # delete the .nc file when no longer needed.
  
  # this function sums the number of degree days during the cropping calendar
  # i - is the stack cal; two rasters - when the crop is planted and when it is harvested
  # v - is the set of 365 rasters in a year (or 366 in a leap year). It is called gddy_subset and generated below
  gddSum <- function(i, v) {
    j <- !is.na(i[,1])
    r <- rep(NA, nrow(i))
    x <- cbind(i[j,,drop=FALSE], v[j,,drop=FALSE])
    r[j] <- apply(x, 1, function(y) sum(y[ (y[1]:y[2])+2 ] )) 
    r
  }
  
  # need to subset and run this by year. This means figuring out what are the leap years
  starttime <- Sys.time()
  cumDays <- 0
  stack.temp <- stack() #, ..., bands=NULL, varname="", native=FALSE, RAT=TRUE, quick=FALSE)
  for (cntr in 0:yearRange) {
    startTime1 <- Sys.time()
    yearIndex <- l + cntr
    daysInYear <- 365
    if (leap_year(yearIndex)) daysInYear <- 366
    daysStart <- cumDays + 1
    daysEnd <- daysStart + daysInYear - 1
    endTime1 <- Sys.time()
    endTime1 - startTime1
    endTime2 <- Sys.time()
    gddy_subset <- subset(gdd, daysStart:daysEnd)
     endTime2 - endTime1
    x <- overlay(cal, gddy_subset, fun = gddSum, recycle = FALSE)
    endTime3 <- Sys.time()
    endTime3 - endTime2
    
    stack.temp <- addLayer(stack.temp, x)
    cumDays <- cumDays + daysInYear
  }
  
  gddfileOut <- paste0(gddsfileOutLoc, "cumgdds_", m, "_", yearSpan, "_", i,  ".tif")
  writeRaster(stack.temp, filename = gddfileOut, format = "GTiff", overwrite = TRUE)
  endtime <- Sys.time()
  print(endtime - starttime)
  
  
  # for (i in 1:nrow(croppingCalendar_plantstart)) { 
  #   for (j in 1:ncol(croppingCalendar_plantend)) { 
  #     print(paste0("row: ", i, " column :", j))
  #     start <- croppingCalendar_plantstart[i,j] # get the starting day
  #     end <- croppingCalendar_plantend[i,j] # get the ending day
  #     print(paste0("start: ", start, " end: ", end))
  #     
  #     datasum.sub1 <- stackApply(gdd[[start:end]], indices, fun = sum) 
  #     print(datasum.sub1)
  #   } 
  # }
  # 
  # indices <- format(as.Date(names(tmin), format = "X%Y.%m.%d"), format = "%m")
  # indices <- as.numeric(indices)
  # monthZeroCount <- stackApply(tmin, indices, fun = function(x, ...){sum(x <= 0)}) 
  # names(monthZeroCount) <- month.abb
  # fileNameOutZero <- paste0("belowZeroCount_", modelName.lower, "_", k, "_", yearSpan, ".tif")
  # writeRaster(monthZeroCount, filename = paste0("data/cmip6/belowZero/", fileNameOutZero), format = "GTiff", overwrite = TRUE)
  
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

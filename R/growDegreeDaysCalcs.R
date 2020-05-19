#  Calculate the number of growing degrees per day 
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, 
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9

IPCC_WG2_Ch5_crop_temperature_table <- as.data.table(read_excel("data-raw/crops/Crop_temperature_table_summary_02052020.xlsx", range = "A1:S26"))
setnames(IPCC_WG2_Ch5_crop_temperature_table, old = names(IPCC_WG2_Ch5_crop_temperature_table), new = make.names(names(IPCC_WG2_Ch5_crop_temperature_table)))
cropChoices <- unique(IPCC_WG2_Ch5_crop_temperature_table$crop)
#cropChoices <- c("Barley")
#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
m <- "Broadbean"
useCores <- detectCores() - 2 # max number of cores
useCores <- 2 # better for memory intensive activities

varList <- c("startyearChoices", "sspChoices", "modelChoices", "locOfFiles", "IPCC_WG2_Ch5_crop_temperature_table", "cropChoices")
libList <- c("raster", "ncdf4", "data.table")

cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R
foreach(k = sspChoices)  %:%
  foreach(l = startyearChoices) %:%
  foreach(i = modelChoices) %dopar% {
    #  foreach(j = variableChoices) %:%
    
    #  foreach(m = cropChoices)  {
    #    require(data.table)
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
      
      Tbase <- IPCC_WG2_Ch5_crop_temperature_table[(crop %in% m), Tbase]
      Tbase_max <- IPCC_WG2_Ch5_crop_temperature_table[(crop %in% m), Tbase_max]
      print(paste0("crop: ", m, " tbase: ", Tbase, " Tbase_max: ", Tbase_max))
      
      tmax_clamped <- clamp(tmax, lower = Tbase, upper = Tbase_max, useValues = TRUE)
      print(paste0("Done with tmax_clamped for ", m,  " pid: ", Sys.getpid()))
      
      tmin_clamped <- clamp(tmin, lower = Tbase, upper = Tbase_max, useValues = TRUE)
      print(paste0("Done with tmin_clamped for ", m, " pid: ", Sys.getpid()))
      endTime <-  Sys.time()
      print(endTime - startTime)
      
      startTime <-  Sys.time()
      gdd <- (tmax_clamped + tmin_clamped)/2 - Tbase
      names(gdd) <- names(tmax)
      indices <- format(as.Date(names(tmax_clamped), format = "X%Y.%m.%d"), format = "%j") # %j is day of the year
      indices <- as.numeric(indices)
      endTime <-  Sys.time()
      print(endTime - startTime)
      
      gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"
      fileNameOut <-    paste(modelName.lower, m, k, "gdd", "global_daily", yearSpan, sep = "_")
      print(paste0("gdd file out name: ", gddsfileOutLoc, fileNameOut, ".tif"))
      
      writeRaster(gdd, filename = paste0(gddsfileOutLoc, fileNameOut, ".tif"), format = "GTiff", overwrite = TRUE)  
    }
    
    unlink(tmpDirName, recursive = TRUE)
    gc()
    
  }
stopCluster(cl)

gdd <- readAll(brick(paste0(fileOutLoc, fileNameOut, ".tif")))
# indices <- format(as.Date(names(gdd), format = "X%Y.%m.%d"), format = "%j") # %j is day of the year
# indices <- as.numeric(indices)
# 

# crop the gdd brick to just the area of the crop from the 

cropArea <- getcropAreaYield(cropName = tolower(m), dataType = "area")
rInArea <- raster(cropArea)
rInAreaAgg <- aggregate(rInArea, fact = 6, fun = "sum")
cutoff <- 1000 # only include 1/2 cells where crop area is great than cutoff
rInAreaAgg[rInAreaAgg < cutoff] <- NA
rInAreaAgg[rInAreaAgg >= cutoff] <- 1


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
cropCalFilesLoc <- "data-raw/crops/cropCalendars/ALL_CROPS_netCDF_0.5deg_filled/"
fileInName <- paste0(cropName, ".crop.calendar.fill.nc")
#    locNFileIn <- paste0(filesLoc, fileInName, ".gz")
locNFileIn <- paste0(cropCalFilesLoc, fileInName)
R.utils::gunzip(paste0(locNFileIn, ".gz"), remove = FALSE)

#   croppingCalendar_plantstart <- readAll(raster(locNFileIn, var = "plant.start"))
#   croppingCalendar_plantend <-  readAll(raster(locNFileIn, var = "plant.end"))
# croppingCalendar_plantrange <- raster(locNFileIn, var = "plant.range")
croppingCalendar_plant <- raster(locNFileIn, var = "plant")
croppingCalendar_harvest <- raster(locNFileIn, var = "harvest")
# croppingCalendar_harveststart <- raster(locNFileIn, var = "harvest.start")
# croppingCalendar_harvestend <- raster(locNFileIn, var = "harvest.end")
# 
#gdd_cropped <- mask(gdd, rInAreaAgg)
croppingCalendar_plant_crop <- mask(croppingCalendar_plant, rInAreaAgg)
croppingCalendar_harvest_crop <- mask(croppingCalendar_harvest, rInAreaAgg)
cal <- readAll(stack(croppingCalendar_plant_crop, croppingCalendar_plant_crop))
unlink(locNFileIn) # delete the .nc file when no longer needed.

#    gddy1 <- subset(gdd_cropped, 1:365)
#    gddy1 <- gdd_cropped

#  b <- brick(croppingCalendar_plantstart, croppingCalendar_plantend, gdd)
# starttime <- Sys.time()
# s <- stack(croppingCalendar_plant, croppingCalendar_harvest, gddy1)
# endtime <- Sys.time()
# print(endTime - startTime)
# starttime <- Sys.time()

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
  yearIndex <- l + cntr
  daysInYear <- 365
  if (leap_year(yearIndex)) daysInYear <- 366
  daysStart <- cumDays + 1
  daysEnd <- daysStart + daysInYear - 1
  gddy_subset <- subset(gdd_cropped, daysStart:daysEnd)
  x <- overlay(cal, gddy_subset, fun = gddSum, recycle = FALSE)
  stack.temp <- addLayer(stack.temp, x)
  cumDays <- cumDays + daysInYear
  
}
gddfileOut <- paste0(gddsfileOutLoc, "cumgdds_", m, "_", yearSpan, "_", i,  ".tif")
writeRaster(stack.temp, filename = gddfileOut, format = "GTiff", overwrite = TRUE)
endtime <- Sys.time()
print(endTime - startTime)


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

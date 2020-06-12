# code sums over the cropping calender to get gdds during the calendar period
source("R/globallyUsed.R")
#library(doParallel) #Foreach Parallel Adapter 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c("UKESM1-0-LL", "IPSL-CM6A-LR") #"MPI-ESM1-2-HR", "MRI-ESM2-0")# "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, 
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2051) #, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
hemisphereList <- c("Northern", "Southern")
northerHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-  c( -180, 180, -90, 0)

yearRange <- 9
gddsfilesLoc <- "data/cmip6/growingDegreeDays/"

#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
m <- "Wheat"
cropName <- m

for (k in sspChoices)  {
  for (i in modelChoices)  {
    for (l in startyearChoices) {
      for (o in 1:length(cropChoices)) {
        for (m in get(cropChoices[o])) {
          print(paste0("crop: ", m))  
          modelName.lower <- tolower(i)
          yearSpan <- paste0(l, "_", l + yearRange)
          gddIn_crop <- paste0(gddsfilesLoc, modelName.lower, "_", m, "_", k, "_gdd", "_global_daily_", yearSpan, ".tif")
          gdd <- brick(gddIn_crop)
          names(gdd) <- readRDS(paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))
          indices <- format(as.Date(names(gdd), format = "X%Y.%m.%d"), format = "%j") # %j is day of the year
          indices <- as.numeric(indices)
          
          startTime <-  Sys.time()
          gdd_north <- gdd[1:180, 1:720, drop = FALSE]
          endTime <-  Sys.time()
          round(difftime(endTime, startTime,  units = "mins"), digits = 2)
          
          startTime <-  Sys.time()
          gdd_south <- gdd[181:360, 1:720, drop = FALSE]
          endTime <-  Sys.time()
          round(difftime(endTime, startTime,  units = "mins"), digits = 2)
          
          
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
          
          # the overlay function needs a user defined function on the relationship between the two rasters
          overlayFunction <- function(x,y) {
            return(x * y)
          }
          
          cropName <- m
          
          fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(m), ".tif")
          mask <- raster(fileNameMask.in)
          
          
          cropCalendarName <- ann_crop_temp_table[crop %in% cropName, crop.calendar]
          cropCalFilesLoc <- paste0("data-raw/crops/cropCalendars/ALL_CROPS_netCDF_0.5deg_filled/")
          fileInName <- paste0(cropCalendarName, ".crop.calendar.fill.nc")
          #    locNFileIn <- paste0(filesLoc, fileInName, ".gz")
          locNFileIn <- paste0(cropCalFilesLoc, fileInName)
          R.utils::gunzip(paste0(locNFileIn, ".gz"), remove = FALSE)
          
          croppingCalendar_plant <- raster(locNFileIn, var = "plant")
          croppingCalendar_harvest <- raster(locNFileIn, var = "harvest")
          croppingCalendar_plant_crop <- mask(croppingCalendar_plant, mask)
          
          unlink(locNFileIn) # delete the .nc file when no longer needed.
          
          croppingCalendar_plant_crop_north <- crop(croppingCalendar_plant_crop, extent(northerHemExtent))
          
          croppingCalendar_harvest_crop <- mask(croppingCalendar_harvest, mask)
          croppingCalendar_harvest_crop_north <- crop(croppingCalendar_harvest_crop, extent(northerHemExtent))
          
          croppingCalendar_plant_crop <- mask(croppingCalendar_plant, mask)
          croppingCalendar_harvest_crop <- mask(croppingCalendar_harvest, mask)
          cal <- readAll(stack(croppingCalendar_plant_crop, croppingCalendar_harvest_crop))
          
          calN <- calS <- croppingCalendar_harvest_crop - croppingCalendar_plant_crop
          calN[calN <= 0] <- NA # raster where all non-NC values are where the harvest date is greater than the plant date
          calN[calN > 0] <- 1 # raster where all non-NC values are where the harvest date is greater than the plant date
          calS[calS <= 0] <- NA # raster where all non-NC values are where the harvest date is less than the plant date, ie is in the new year
          calS[calS > 0] <- 1 # raster where all non-NC values are where the harvest date is less than the plant date, ie is in the new year
          
          
          calN <- overlay(cal, calN, fun = overlayFunction, recycle = FALSE)
          
          
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
          
          gddy_subset <- readAll(gdd)
          
          
          # need to subset and run this by year. This means figuring out what are the leap years
          cumDays <- 0
          stack.temp <- stack() #, ..., bands=NULL, varname="", native=FALSE, RAT=TRUE, quick=FALSE)
          startTime <- Sys.time()
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
            gddy_subset <- readAll(subset(gdd, daysStart:daysEnd))
            startTime <- Sys.time()
            x <- overlay(calN, gddy_subset, fun = gddSum, recycle = FALSE)
            endTime <- Sys.time()
            round(difftime(endTime, startTime, units = "mins"), digits = 2)
            endTime3 - endTime2
            
            stack.temp <- addLayer(stack.temp, x)
            cumDays <- cumDays + daysInYear
          }
          endTime <- Sys.time()
          round(difftime(endTime, startTime,  units = "mins"), digits = 2)
          
          gddfileOut <- paste0(gddsfileOutLoc, "cumgdds_", m, "_", yearSpan, "_", i,  ".tif")
          writeRaster(stack.temp, filename = gddfileOut, format = "GTiff", overwrite = TRUE)
          endtime <- Sys.time()
          round(difftime(endTime, startTime,  units = "mins"), digits = 2)
          
        }
      }
    }
  }
}
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





rm(list = c("tmax", "tmin"))

unlink(tmpDirName, recursive = TRUE)
gc(reset = FALSE, full = TRUE)

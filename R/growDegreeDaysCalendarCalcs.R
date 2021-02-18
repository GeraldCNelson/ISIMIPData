# code sums over the cropping calender to get gdds during the calendar period
source("R/globallyUsed.R")
#library(doParallel) #Foreach Parallel Adapter 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c("UKESM1-0-LL", "IPSL-CM6A-LR") #"MPI-ESM1-2-HR", "MRI-ESM2-0")# "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, 
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startYearChoices <-  c(2051) #, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
hemisphereList <- c("Northern", "Southern")
northerHemExtent <- c( -180, 180, 0, 90) # this is latlong
southernHemExtent <-  c( -180, 180, -60, 0) # this is latlong

yearRange <- 9
gddsfilesLoc <- "data/cmip6/growingDegreeDays/"

#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2051
m <- "Wheat"
cropName <- m

for (k in sspChoices)  {
  for (i in modelChoices)  {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      
      for (o in 1:length(cropChoices)) {
        for (m in get(cropChoices[o])) {
          print(paste0("crop: ", m))  
          modelName.lower <- tolower(i)
          gddIn_crop <- paste0(gddsfilesLoc, modelName.lower, "_", m, "_", k, "_gdd", "_global_daily_", yearSpan, ".tif")
          gdd <- rast(gddIn_crop)
          names(gdd) <- indices
          indices <- format(as.Date(names(gdd), format = "X%Y.%m.%d"), format = "%j") # %j is day of the year
          indices <- as.numeric(indices)
          
           extent.north <- extent(northerHemExtent)
          extent.south <- extent(southernHemExtent)
          system.time(gdd_north <- crop(gdd, extent.north))
          system.time(gdd_south <- crop(gdd, extent.south))
 
          
          #Sage cropping calendar info
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
          
          # ggcmi cropping calendar info
          # The ggcmi data include planting day, maturity day, and growing season length. The SAGE cropping calendar data have plant and harvest days (these are mean of plant.start to plant.end and same for harvest). Is there some use for the maturity day numbers?
          
          # # the overlay function needs a user defined function on the relationship between the two rasters
          # overlayFunction <- function(x,y) {
          #   return(x * y)
          # }
          
          cropName <- m
          
          fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(m), ".tif")
          cropMask <- rast(fileNameMask.in)
          
          cropCalendarName <- ann_crop_temp_table[crop %in% cropName, crop.calendar]
          cropCalFilesLoc <- paste0("data-raw/crops/cropCalendars/ALL_CROPS_netCDF_0.5deg_filled/")
          fileName_in <- paste0(cropCalendarName, ".crop.calendar.fill.nc")
          #    locNFileIn <- paste0(filesLoc, fileName_in, ".gz")
          locNFileIn <- paste0(cropCalFilesLoc, fileName_in)
          R.utils::gunzip(paste0(locNFileIn, ".gz"), remove = FALSE)
          croppingCalendar <- rast(locNFileIn)
          crs(croppingCalendar) <- crs(cropMask) # needed because cropping calendar doesn't have an explicit crs
          croppingCalendar_mask <- mask(croppingCalendar, cropMask)
          croppingCalendar_plant <- croppingCalendar_mask$plant
          croppingCalendar_harvest <- croppingCalendar_mask$harvest
          
          unlink(locNFileIn) # delete the .nc file when no longer needed.
          
          croppingCalendar_plant_crop_north <- crop(croppingCalendar_plant, extent(northerHemExtent))
          croppingCalendar_harvest_crop_north <- crop(croppingCalendar_harvest, extent(northerHemExtent))
          cal <- c(croppingCalendar_plant_crop, croppingCalendar_plant_crop)
          
          calS[calS > 0] <- NA # raster where all non-NC values are where the harvest date is less than the plant date, ie is in the new year
          calS[calS < 0] <- 1 # raster where all non-NC values are where the harvest date is less than the plant date, ie is in the new year
          
          
          calN <- overlay(cal, calN, fun = overlayfunction_mask, recycle = FALSE)
          calS <- overlay(cal, calS, fun = overlayfunction_mask, recycle = FALSE)
          
          
          # this function sums the number of degree days during the cropping calendar
          # i - is the stack cal; two rasters - when the crop is planted and when it is harvested
          # v - is the set of 365 rasters in a year (or 366 in a leap year). It is called gdd_year and generated below
          gddSum <- function(i, v) {
            j <- !is.na(i[,1])
            r <- rep(NA, nrow(i))
            x <- cbind(i[j,,drop=FALSE], v[j,,drop=FALSE])
            r[j] <- apply(x, 1, function(y) sum(y[ (y[1]:y[2])+2 ] )) 
            r
          }
          #names edited
          gddSum <- function(cal, gdd_year) {
            j <- !is.na(cal[,1]) # test if there are entries in the first layer of cal; j is TRUE/FALSE
            r <- rep(NA, nrow(cal)) # create r, a vector of NAs, one for each row in cal
            x <- cbind(cal[j,,drop=FALSE], gdd_year[j,,drop=FALSE])
            r[j] <- apply(x, 1, function(y) sum(y[ (y[1]:y[2])+2 ] )) 
            r
          }
          
          # return a year's worth of day layers from the yearHolder brick
          yearSubsetter <- function(l, yearRange, yearHolder) {
            yearSpan <- paste0(l, "_", l + yearRange)
            layerNames <- readRDS(paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))
            indices <- format(as.Date(layerNames, format = "X%Y.%m.%d"), format = "%Y")
            indexList <- which(!indices %in% l)
            yearLayers <- dropLayer(yearHolder, indexList)
          }
          
          gdd_year <- yearSubsetter(l, yearRange, yearHolder = gdd)
          
          x <- overlay(calN, gdd_year, fun = gddSum, recycle = FALSE)
          
          system.time(x <- setValues(gdd_year, gddSum(values(calN), values(gdd_year))))
          
          # need to subset and run this by year. 
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
            gdd_year <- readAll(dropLayer(gdd, daysStart:daysEnd))
            startTime <- Sys.time()
            x <- overlay(calN, gdd_year, fun = gddSum, recycle = FALSE)
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
#     datasum.sub1 <- tapp(gdd[[start:end]], indices, fun = sum) 
#     print(datasum.sub1)
#   } 
# }
# 
# indices <- format(as.Date(names(tmin), format = "X%Y.%m.%d"), format = "%m")
# indices <- as.numeric(indices)
# monthZeroCount <- tapp(tmin, indices, fun = function(x, ...){sum(x <= 0)}) 
# names(monthZeroCount) <- month.abb
# fileName_outZero <- paste0("belowZeroCount_", modelName.lower, "_", k, "_", yearSpan, ".tif")
# writeRaster(monthZeroCount, filename = paste0("data/cmip6/belowZero/", fileName_outZero), format = "GTiff", overwrite = TRUE)





rm(list = c("tmax", "tmin"))

unlink(tmpDirName, recursive = TRUE)
gc(reset = FALSE, full = TRUE)


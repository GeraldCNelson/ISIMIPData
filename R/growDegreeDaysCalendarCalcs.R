# code sums over the cropping calender to get gdds during the calendar period
source("R/globallyUsed.R")

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c("UKESM1-0-LL", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4")# "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, 
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startYearChoices <-  c(2041, 2081) #, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
hemispheres <- c("NH", "SH")
extent_NH <- c( -180, 180, 0, 90)
extent_SH <-c( -180, 180, -60, 0) #-60 gets rid of Antarctica

yearRange <- 19
gddsfilesLoc <- "data/cmip6/growingDegreeDays/"

#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2041
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
          m <- tolower(m)
          print(paste0("crop: ", m))  
          modelName.lower <- tolower(i)
          fileName_in_gdd_crop <- paste0(gddsfilesLoc, modelName.lower, "_gdd", "_", m, "_", k,  "_global_daily_", yearSpan, ".tif")
          gdd <- rast(fileName_in_gdd_crop)
          names(gdd) <- indices
          indices <- as.numeric(indices)
          
          system.time(gdd_north <- crop(gdd, extent_NH))
          system.time(gdd_south <- crop(gdd, extent_SH))
          
          
          #Sage cropping calendar info
          # Sacks, W.J., D. Deryng, J.A. Foley, and N. Ramankutty (2010). Crop
          # planting dates: An analysis of global patterns. Global Ecology and
          # Biogeography, 19: 607-620.
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
          
          fileName_in_mask <- paste0("data/crops/rasterMask_", cropName, ".tif")
          cropMask <- rast(fileName_in_mask)
          
          cropCalendarName <- cropCharacteristics_annual[crop %in% cropName, crop.calendar]
          cropCalFilesLoc <- paste0("data-raw/crops/cropCalendars/ALL_CROPS_netCDF_0.5deg_filled/")
          fileName_in <- paste0(cropCalendarName, ".crop.calendar.fill.nc")
          fileName_in <- paste0(cropCalFilesLoc, fileName_in)
          R.utils::gunzip(paste0(fileName_in, ".gz"), remove = FALSE)
          croppingCalendar <- rast(fileName_in)
          crs(croppingCalendar) <- crs(cropMask) # needed because cropping calendar doesn't have an explicit crs
          croppingCalendar_mask <- mask(croppingCalendar, cropMask)
          croppingCalendar_plant <- croppingCalendar_mask$plant
          croppingCalendar_harvest <- croppingCalendar_mask$harvest
          unlink(locNFileIn) # delete the .nc file when no longer needed.
          
          croppingCalendar_plant_crop_NH <- crop(croppingCalendar_plant, extent_NH)
          croppingCalendar_harvest_crop_NH <- crop(croppingCalendar_harvest, extent_NH)

          croppingCalendar_plant_crop_SH <- crop(croppingCalendar_plant, extent_SH)
          croppingCalendar_harvest_crop_SH <- crop(croppingCalendar_harvest, extent_SH)
          
          cal <- c(croppingCalendar_plant_crop, croppingCalendar_plant_crop)
          
          calS[calS > 0] <- NA # raster where all non-NC values are where the harvest date is less than the plant date, ie is in the new year
          calS[calS < 0] <- 1 # raster where all non-NC values are where the harvest date is less than the plant date, ie is in the new year
          
          calN <- overlay(cal, calN, fun = overlayfunction_mask, recycle = FALSE)
          calS <- overlay(cal, calS, fun = overlayfunction_mask, recycle = FALSE)
          
          
          # this function sums the number of degree days during the cropping calendar
          # i - is the stack cal; two rasters - when the crop is planted and when it is harvested
          # v - is the set of 365 rasters in a year (or 366 in a leap year). It is called gdd_year and generated below
          f_gddSum <- function(i, v) {
            j <- !is.na(i[,1])
            r <- rep(NA, nrow(i))
            x <- cbind(i[j,,drop=FALSE], v[j,,drop=FALSE])
            r[j] <- apply(x, 1, function(y) sum(y[ (y[1]:y[2])+2 ] )) 
            r
          }
          #names edited
          f_gddSum <- function(cal, gdd_year) {
            j <- !is.na(cal[,1]) # test if there are entries in the first layer of cal; j is TRUE/FALSE
            r <- rep(NA, nrow(cal)) # create r, a vector of NAs, one for each row in cal
            x <- cbind(cal[j,,drop=FALSE], gdd_year[j,,drop=FALSE])
            r[j] <- apply(x, 1, function(y) sum(y[ (y[1]:y[2])+2 ] )) 
            r
          }
          
          # return a year's worth of day layers from the yearHolder raster
          yearSubsetter <- function(l, yearRange, yearHolder) {
            yearSpan <- paste0(l, "_", l + yearRange)
            layerNames <- readRDS(paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))
            indices <- format(as.Date(layerNames, format = "X%Y.%m.%d"), format = "%Y")
            indexList <- which(!indices %in% l)
            yearLayers <- dropLayer(yearHolder, indexList)
          }
          
          gdd_year <- yearSubsetter(l, yearRange, yearHolder = gdd)
          
          x <- overlay(calN, gdd_year, fun = f_gddSum, recycle = FALSE)
          
          system.time(x <- setValues(gdd_year, f_gddSum(values(calN), values(gdd_year))))
          
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
            x <- overlay(calN, gdd_year, fun = f_gddSum, recycle = FALSE)
            endTime <- Sys.time()
            round(difftime(endTime, startTime, units = "mins"), digits = 2)
            endTime3 - endTime2
            
            stack.temp <- addLayer(stack.temp, x)
            cumDays <- cumDays + daysInYear
          }
          
          gddfileOut <- paste0(gddsfileOutLoc, "cumgdds_", m, "_", yearSpan, "_", i,  ".tif")
          writeRaster(stack.temp, filename = gddfileOut, format = "GTiff", overwrite = TRUE)
          endtime <- Sys.time()
          round(difftime(endTime, startTime,  units = "mins"), digits = 2)
          
        }
      }
    }
  }
}

# perennials hemisphere specific runs calculations -----
tminVal <- 0
test_logic <- paste0("x > ", tmin)
k <- "ssp585"
l = "2041"
logicDirection <- ">"


f_runs <- function(x) {
  runResult <- c(NA, NA) 
  if (is.nan(x[1])) {
    return(runResult)
  }
  seqLengthCode <- paste0("1{", runlength, ",}") #A regular expression  to get the first item of gregexpr. It says look for  run_length times See http://xenon.stanford.edu/~xusch/regexp/
  g <- gregexpr(seqLengthCode, paste(+eval(parse(text = test_logic)), collapse = ""))[[1]] # The + converts TRUE and FALSE to 1 and 0
  #  print(paste0("g1: ", g[1]))
  if ((g[1] == -1)) { # no need to write to growing season if g returns -1, return 0,0
    runResult <- c(0, 0) 
    #    print("no runs")
  } else {
    startDays <- unlist(g)
    runLengths <- sum(as.numeric(attributes(g)$match.length))
    runResult <- c(length(startDays), runLengths)
  }
  return(runResult)
}

runlengthChoices <- c(100) # at least 100 days of tmin > 0
for (runlength in runlengthChoices) {
  for (k in sspChoices) {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      
      for (m in speciesChoice) {
        for (s in "extremeStress") {
          logicDirection <- ">"
          if (m == "humans") logicDirection <- "<" # pwc less than thiStressVal is bad
          
          if (logicDirection == ">") ldtext <-"gt"
          if (logicDirection == "<") ldtext <-"lt"
          test_logic <- paste0("x ", logicDirection, " ", stressValue)
          fileName_in <- paste0("data/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
          r <- rast(fileName_in)
          fileName_out <- paste0("data/cmip6/THI/run_", runlength, "_lim_", logicDirection, stressValue, "_ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
          print(system.time(r_runs <- app(r, f_runs, filename = fileName_out,  overwrite = TRUE, wopt= woptList)))
          #   print(system.time(writeRaster(r_runs, filename = fileName_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          mainrl = paste0(m, ", THI gt ", stressValue, ", \nlongest no. of days in a min. run of ", runlength, " days, ", k, ", ", gsub("_", "-", yearSpan))
          if (m =="humans")  mainrl = paste0(m, ", PWC ", logicDirection, " , ", stressValue, ", \nlongest no of days in a min. run of ", runlength, " days, ", k, ", ", gsub("_", "-", yearSpan))
          mainct = paste0(m, ", THI gt ", stressValue, ", \nrun minimum length is ", runlength, " days, ", k, ", ", yearSpan)
          if (m =="humans")  mainct = paste0(m, ", PWC  ", logicDirection, " , ", stressValue, ", \nrun minimum length is ", runlength, " days, ", k, ", ", gsub("_", "-", yearSpan))
          
          plot(r_runs$lyr.1, main = mainct)
          plot(r_runs$lyr.2, main = mainrl)
        }
      }
    }
  }
}

#runs, historical -----
k = "historical"
l = 1991
for (runlength in runlengthChoices) {
  for (m in speciesChoice) {
    for (s in "extremeStress") {
      stressValue <- f_getStressValue(m, s) -1
      logicDirection <- ">"
      if (m == "humans") logicDirection <- "<" # pwc less than thiStressVal is bad
      
      if (logicDirection == ">") ldtext <-"gt"
      if (logicDirection == "<") ldtext <-"lt"
      test_logic <- paste0("x ", logicDirection, " ", stressValue)
      yearSpan <- paste0(l, "_", l + yearRange)
      fileName_in <- paste0("data/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
      r <- rast(fileName_in)
      fileName_out <- paste0("data/cmip6/THI/run_", runlength, "_lim_", logicDirection, stressValue, "_ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
      print(system.time(r_runs <- app(r, f_runs, filename = fileName_out,  overwrite = TRUE, wopt= woptList)))
      #   print(system.time(writeRaster(r_runs, filename = fileName_out,  overwrite = TRUE, wopt= woptList))); flush.console()
      mainrl = paste0(m, ", THI gt ", stressValue, ", \nlongest no. of days in a min. run of ", runlength, " days, ", k, ", ", gsub("_", "-", yearSpan))
      if (m =="humans")  mainrl = paste0(m, ", PWC ", logicDirection, " , ", stressValue, ", \nlongest no of days in a min. run of ", runlength, " days, ", k, ", ", gsub("_", "-", yearSpan))
      mainct = paste0(m, ", THI gt ", stressValue, ", \nrun minimum length is ", runlength, " days, ", k, ", ", yearSpan)
      if (m =="humans")  mainct = paste0(m, ", PWC  ", logicDirection, " , ", stressValue, ", \nrun minimum length is ", runlength, " days, ", k, ", ", gsub("_", "-", yearSpan))
      
      plot(r_runs$lyr.1, main = mainct)
      plot(r_runs$lyr.2, main = mainrl)
    }
  }
}


#  Calculate the number of growing degrees per day 
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- paste0(locOfCMIP6ncFiles, "original/")
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9

#gdd functions from the package called pollen, based on the paper Baskerville, G. L., and Emin, P. (1969). 
# Rapid Estimation of Heat Accumulation from Maximum and Minimum Temperatures. Ecology 50, 514–517. doi:10.2307/1933912.
# note that these are linearized versions. If tmax and tmin are vectors of data over a period of time the function returns t
# the cumulative degree days.
f.growdegree <- function(tmax, tmin, tbase, tbase_max, type = "C") 
{
  if (!type %in% c("A", "B", "C", "D")) {
    stop("The case argument must be either \"A\", \"B\", \"C\", or \"D\"", 
         call. = FALSE)
  }
  if (type %in% c("A", "B", "C", "D")) {
    tmax <- adjust_for_tbase(tmax, tbase)
    tmin <- adjust_for_tbase(tmin, tbase)
  }
  if (type %in% c("C")) {
    tmax <- adjust_for_tbase_max(tmax, tbase_max)
    tmin <- adjust_for_tbase_max(tmin, tbase_max)
  }
  if (type == "D") {
    gdd_temp <- ifelse(tmax > tbase_max, yes = 0, no = (tmax + 
                                                          tmin)/2 - tbase)
  }
  else if (type %in% c("A", "B", "C")) {
    gdd_temp <- (tmax + tmin)/2 - tbase
  }
  return(cumsum(gdd_temp))
}

adjust_for_tbase <- function(x, tbase) {
  ifelse(test = x < tbase, yes = tbase, no = x)
}

adjust_for_tbase_max <- function(x, tbase_max) {
  ifelse(test = x > tbase_max, yes = tbase_max, no = x)
}

# ---- end of GDD formulas
#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2021
northernHemWinter <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
#northernHemWinter.num <- c(11, 12, 1, 2, 3, 4)
southernHemWinter <- c("May", "Jun", "Jul", "Aug", "Sep", "Oct")
#southernHemWinter.num <- c(5, 6, 7, 8, 9, 10)
useCores <- detectCores() - 2 # max number of cores
#useCores <- 2 # better for memory intensive activities

varList <- c("startyearChoices", "sspChoices", "modelChoices", "locOfFiles")
libList <- c("raster", "ncdf4")

cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R
foreach(l = startyearChoices) %:%
  foreach(i = modelChoices) %:%
  #  foreach(j = variableChoices) %:%
  foreach(k = sspChoices) %dopar% {
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
    
    j <- "tasmin"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    temp <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    print(paste0("Working on : ", temp, " pid: ", Sys.getpid()))
    tmin <- brick(temp)
    
    startTime <-  Sys.time()
    #    tmin <- readAll((tmin))
    # tmin <- fixUnits(var = "tmin", ncin.brick = tmin) # fixes temp and precip units; assumes ncin.brick values are raw units
    
    print("Done with tmin readIn")
    tminTime <- Sys.time()
    print(paste0(tminTime - startTime, " pid: ", Sys.getpid()))
    #    tmax <- readAll((tmax))
   # tmax <- fixUnits(var = "tmax", ncin.brick = tmax) # fixes temp and precip units; assumes ncin.brick values are raw units
    
    tmaxTime <- Sys.time()
    print("Done with tmax readIn")
    tmaxTime - tminTime
    
    testout <- f.growdegree(tmax, tmin, tbase = 10, tbase_max = 35, type = "C")
    
    
    chillHrs <- overlay(tmin, tmax, fun = f.chillhrs)
    names(chillHrs) <- names(tmax) # put the date info back into the names
    
    # do several count days in a month
    # first days with temp below zero
    print(paste0("Done with chillHrs function", " pid: ", Sys.getpid()))
    endTime <-  Sys.time()
    #  print(endTime - startTime)
    print(difftime(endTime, startTime, units = "mins"))
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
    
    #tmax > 35
    f.tmaxLimit(tmax, tmaxLimit = 35)
    #tmax > 38
    f.tmaxLimit(tmax, tmaxLimit = 38)
    #tmax > 45
    f.tmaxLimit(tmax, tmaxLimit = 45)
    #tmax > 48
    f.tmaxLimit(tmax, tmaxLimit = 48)
    
    rm(list = c("tmax", "tmin"))
    chillHrs.sumMonth <- stackApply(chillHrs, indices, fun = sum, na.rm = TRUE)
    chillHrs.sumMonth <- chillHrs.sumMonth/10 # to get to the monthly average over 10 years
    names(chillHrs.sumMonth) <- month.abb
    chillHrsNorthernHem <- dropLayer(chillHrs.sumMonth, southernHemWinter) # note dropping layers for southern hemisphere winter
    chillHrsSouthernHem <- dropLayer(chillHrs.sumMonth, northernHemWinter) # note dropping layers for northern hemisphere winter
    chillHrsNorthernHem <- sum(chillHrsNorthernHem)
    chillHrsSouthernHem <- sum(chillHrsSouthernHem)
    endCompleteLoop <- Sys.time()
    print("complete loop time")
    print(difftime(Sys.time(), startTime, units = "mins"))
    
    #print(endCompleteLoop - startTime)
    
    fileNameNH <- paste0("chillHrsNorthernHem_", modelName.lower, "_", k, "_", yearSpan, ".tif")
    fileNameSH <- paste0("chillHrsSouthernHem_", modelName.lower, "_", k, "_", yearSpan, ".tif")
    
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

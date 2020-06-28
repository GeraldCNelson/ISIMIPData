#  chilling hours calculations
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c("IPSL-CM6A-LR")# 

modelChoices <- c("IPSL-CM6A-LR", "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2021, 2051, 2091) #, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9

f.chillhrs <- function(tmin, tmax) {
  ch <- (7 - tmin)/(tmax - tmin)
  ch[tmin > 7] <- 0
  ch[tmax < 7 & tmin <= 7] <- 24
  ch
}
#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2091
northernHemWinter <- c("Nov", "Dec", "Jan", "Feb") #, "Mar", "Apr")
northernHemSummer <- c("May", "Jun", "Jul", "Aug", "Sep", "Oct")
southernHemWinter <- c("May", "Jun", "Jul", "Aug") #, "Sep", "Oct")
southernHemSummer <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
useCores <- detectCores() - 2 # max number of cores
useCores <- 2 # better for memory intensive activities

varList <- c("startyearChoices", "sspChoices", "modelChoices", "locOfFiles")
libList <- c("rast", "ncdf4")

cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R
foreach(l = startyearChoices) %:%
  foreach(i = modelChoices) %:%
  #  foreach(j = variableChoices) %:%
  foreach(k = sspChoices) %dopar% {
    print(paste0("start year: ", l, " ssp: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
     
    modelName.lower <- tolower(i)
    startTime <-  Sys.time()
    yearSpan <- paste0(l, "_", l + yearRange)
    j <- "tasmax"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    tmaxIn <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    #      tmax <- readAll(rasttemp))
    # endTime <- Sys.time()
    # print(paste0("tmax brick created, ", tmaxIn, ", creation time: ",  round(difftime(endTime, startTime, units = "mins"), digits = 2),  " min.,  pid: ", Sys.getpid()))
    
    startTime <-  Sys.time()
    j <- "tasmin"
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    tminIn <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
    #      <- readAll(rasttemp))
    startTime <-  Sys.time()
    tmaxTminIn(tmaxIn, tminIn) # function to read in tmax and tmin with rast
    terra:::.mem_info(tmax, 1)
    system.time(chillHrs <- f.chillhrs(tmin, tmax))
    names(chillHrs) <- names(tmax) # put the date info back into the names
    # 
    # do several count days in a month
    # first days with temp below zero
    print(paste0("Done with chillHrs function", " pid: ", Sys.getpid()))
    endTime <-  Sys.time()
    #  print(endTime - startTime)
    print(difftime(endTime, startTime, units = "mins"))
    indices <- format(as.Date(names(tmin), format = "X%Y.%m.%d"), format = "%m")
    indices <- as.numeric(indices)
    monthZeroCount <- tapp(tmin, indices, fun = function(x, ...){sum(x <= 0)})
    names(monthZeroCount) <- month.abb
    fileNameOutZero <- paste0("belowZeroCount_", modelName.lower, "_", k, "_", yearSpan, ".tif")
    print(paste0("Writing out ", fileNameOutZero))
    writeRaster(monthZeroCount, filename = paste0("data/cmip6/belowZero/", fileNameOutZero), format = "GTiff", overwrite = TRUE)
 
    #   rm(list = c("tmax", "tmin"))
    chillHrs.sumMonth <- tapp(chillHrs, indices, fun = sum, na.rm = TRUE)
    chillHrs.sumMonth <- chillHrs.sumMonth/10 # to get to the monthly average over 10 years
    names(chillHrs.sumMonth) <- month.abb
    chillHrsNorthernHem <- subset(chillHrs.sumMonth, southernHemWinter) # note dropping layers for southern hemisphere winter
    chillHrsSouthernHem <- subset(chillHrs.sumMonth, northernHemWinter) # note dropping layers for northern hemisphere winter
    chillHrsNorthernHem <- sum(chillHrsNorthernHem)
    chillHrsSouthernHem <- sum(chillHrsSouthernHem)
    endCompleteLoop <- Sys.time()
    print("complete loop time")
    print(difftime(Sys.time(), startTime, units = "mins"))
    
    #print(endCompleteLoop - startTime)
    
    fileNameNH <- paste0("chillHrs_NorthernHem_", modelName.lower, "_", k, "_", yearSpan, ".tif")
    fileNameSH <- paste0("chillHrs_SouthernHem_", modelName.lower, "_", k, "_", yearSpan, ".tif")
    
    writeRaster(chillHrsNorthernHem, filename = paste0("data/cmip6/chillingHours/", fileNameNH), format = "GTiff", overwrite = TRUE)
    writeRaster(chillHrsSouthernHem, filename = paste0("data/cmip6/chillingHours/", fileNameSH), format = "GTiff", overwrite = TRUE)
    unlink(tmpDirName, recursive = TRUE)
    gc(reset = FALSE, full = TRUE)
  }
stopCluster(cl)

# do same calculations on observed data
tmin <- tasmin.observed
tmax <- tasmax.observed
yearSpan <- "2001_2010"

chillHrs <- overlay(tmin, tmax, fun = f.chillhrs)
names(chillHrs) <- names(tmax) # put the date info back into the names

# do several count days in a month

# first count the number of days with temp below zero
print("Done with chillHrs function")
indices <- format(as.Date(names(tmin), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
monthZeroCount <- tapp(tmin, indices, fun = function(x, ...){sum(x <= 0)}) 
names(monthZeroCount) <- month.abb
fileNameOutZero <- paste0("belowZeroCount", "_observed_", yearSpan, ".tif")
writeRaster(monthZeroCount, filename = paste0("data/cmip6/belowZero/", fileNameOutZero), format = "GTiff", overwrite = TRUE)

# # now do count above tmax limit
# f.tmaxLimit <- function(tmax, tmaxLimit, indices) {
#   tmaxSum <- tapp(tmax, indices, fun = function(x, ...){sum(x >= tmaxLimit)}) 
#   names(tmaxSum) <- month.abb
#   fileNameOut <- paste0("tmaxGT_", tmaxLimit, "_observed_", yearSpan, ".tif")
#   writeRaster(tmaxSum, filename = paste0("data/cmip6/tmaxMonthlySums/", fileNameOut), format = "GTiff", overwrite = TRUE)
# }
# tmaxfunctionStart <- Sys.time()
# #tmax > 31
# f.tmaxLimit(tmax, tmaxLimit = 31, indices)
# print(paste("Completed tmaxlimit for 31C"))
# #tmax > 35
# f.tmaxLimit(tmax, tmaxLimit = 35, indices)
# #tmax > 38
# f.tmaxLimit(tmax, tmaxLimit = 38, indices)
# #tmax > 45
# f.tmaxLimit(tmax, tmaxLimit = 45, indices)
# #tmax > 48
# f.tmaxLimit(tmax, tmaxLimit = 48, indices)
# print(paste("Completed tmaxlimit for 48C"))

rm(list = c("tmax", "tmin"))
chillHrs.sumMonth <- tapp(chillHrs, indices, fun = sum, na.rm = TRUE)
chillHrs.sumMonth <- chillHrs.sumMonth/10 # to get to the monthly average over 10 years
names(chillHrs.sumMonth) <- month.abb
chillHrsNorthernHem <- dropLayer(chillHrs.sumMonth, southernHemWinter) # note dropping layers for southern hemisphere winter
chillHrsSouthernHem <- dropLayer(chillHrs.sumMonth, northernHemWinter) # note dropping layers for northern hemisphere winter
chillHrsNorthernHem <- sum(chillHrsNorthernHem)
chillHrsSouthernHem <- sum(chillHrsSouthernHem)

fileNameNH <- paste0("chillHrs_NorthernHem", "_observed_", yearSpan, ".tif")
fileNameSH <- paste0("chillHrs_SouthernHem", "_observed_", yearSpan, ".tif")

writeRaster(chillHrsNorthernHem, filename = paste0("data/cmip6/chillingHours/", fileNameNH), format = "GTiff", overwrite = TRUE)
writeRaster(chillHrsSouthernHem, filename = paste0("data/cmip6/chillingHours/", fileNameSH), format = "GTiff", overwrite = TRUE)

gc(reset = FALSE, full = TRUE)

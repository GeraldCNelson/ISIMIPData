#  Calculate the number of growing degrees per day 
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, 
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2051, 2091) #, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
hemisphereList <- c("Northern", "Southern")
northerHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-  c( -180, 180, -90, 0)

yearRange <- 9
tmaxList <- c(31, 35, 38, 45, 48)

#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
m <- "Barley"

# function to do count above tmax limit
f.tmaxLimit <- function(tmax, tmaxLimit, indices) {
  tmaxSum <- stackApply(tmax, indices, fun = function(x, ...){(sum(x >= tmaxLimit))/10})  # divide by 10 to get the annual average number of days above tmaxLimit
  names(tmaxSum) <- month.abb
  fileNameOut <- paste0("tmaxGT_", tmaxLimit, "_", modelName.lower, "_", k, "_", yearSpan, ".tif")
  writeRaster(tmaxSum, filename = paste0("data/cmip6/tmaxMonthlySums/", fileNameOut), format = "GTiff", overwrite = TRUE)
}

for (k in sspChoices)  {
  for (i in modelChoices)  {
    for (l in startyearChoices) {
      modelName.lower <- tolower(i)
      startTime <-  Sys.time()
      yearSpan <- paste0(l, "_", l + yearRange)
      j <- "tasmax"
      fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
      fileNameIn <- paste0(fileNameIn, ".nc")
      
      temp <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
      print(paste0("Working on : ", temp, " pid: ", Sys.getpid()))
      tmax <- readAll(brick(temp))
      endTime <- Sys.time()
      print(paste0("tmax brick created, ", temp, ", creation time: ", endTime -startTime,  " pid: ", Sys.getpid()))
      indices <- format(as.Date(names(tmax), format = "X%Y.%m.%d"), format = "%m")
      indices <- as.numeric(indices)
      
      for (n in tmaxList) {
        f.tmaxLimit(tmax, tmaxLimit = n, indices)
        print(paste("Completed tmaxlimit for ", n, "C"))
      }
      print(paste("Completed tmaxlimit list for start year ", l))
    }
  }
}

# observed results
# now do count above tmax limit for observed period

fileNameIn <- paste0(locOfFiles, "gswp3-w5e5_obsclim_tasmax_global_daily_2001_2010.nc")
tmax <- readAll(brick(fileNameIn))
indices <- format(as.Date(names(tmax), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)

f.tmaxLimit.observed <- function(tmax, tmaxLimit, indices) {
  tmaxSum <- stackApply(tmax, indices, fun = function(x, ...){sum(x >= tmaxLimit)}) 
  names(tmaxSum) <- month.abb
  fileNameOut <- paste0("tmaxGT_", tmaxLimit, "_observed_", yearSpan, ".tif")
  writeRaster(tmaxSum, filename = paste0("data/cmip6/tmaxMonthlySums/", fileNameOut), format = "GTiff", overwrite = TRUE)
}

for (n in tmaxList) {
  f.tmaxLimit.observed(tmax, tmaxLimit = n, indices)
  print(paste("Completed tmaxlimit for ", n, "C"))
}
print("Completed tmaxlimit list for observed data")
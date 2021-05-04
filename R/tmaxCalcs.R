#  Calculate the number of days per month were tmax is above the damage tmax level 
source("R/globallyUsed.R")

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL") #, "IPSL-CM6A-LR") #, 
# modelChoices <- c("IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startYearChoices <-  c(2021, 2051, 2091) #, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9

tmaxList <- sort(unique(cropCharacteristics_annual$tdamage.mean)) #get all the damaging temperature levels for the annual crops

#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051

# function to do count above tmax limit
f.tmaxLimit <- function(tmax, tmaxLimit, indices) {
  tmaxSum <- tapp(tmax, indices, fun = function(x, ...){(mean(x >= tmaxLimit))})  # divide by 10 to get the annual average number of days above tmaxLimit
  names(tmaxSum) <- month.abb
  fileName_out <- paste0("tmaxGT_", tmaxLimit, "_", modelName.lower, "_", k, "_", yearSpan, ".tif")
  writeRaster(tmaxSum, filename = paste0("data/cmip6/tmaxMonthlySums/", fileName_out), format = "GTiff", overwrite = TRUE)
}

# this for loop and the one below it for observed data generate the tmax damage monthly counts for all the values in the crop spreadsheet for the whole world
for (k in sspChoices)  {
  for (i in modelChoices)  {
    for (l in startYearChoices) {
      modelName.lower <- tolower(i)
      startTime <-  Sys.time()
      yearSpan <- paste0(l, "_", l + yearRange)
      j <- "tasmax"
      fileName_in <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
      fileName_in <- paste0(fileName_in, ".tif")
      
      temp <- paste0(locOfFiles, k,"/", i, "/", fileName_in)
      print(paste0("Working on: ", temp, ", pid: ", Sys.getpid()))
      tmax <- rast(temp)
      endTime <- Sys.time()
      print(paste0("tmax brick created, ", temp, ", creation time: ", round(difftime(endTime, startTime, units = "mins"), digits = 2),  " pid: ", Sys.getpid()))
      indices <- format(as.Date(names(tmax), format = "X%Y.%m.%d"), format = "%m")
      indices <- as.numeric(indices)
      
      for (n in tmaxList) {
        f.tmaxLimit(tmax, tmaxLimit = n, indices)
        print(paste("Completed tmax damage count for ", n, "C"))
      }
      print(paste("Completed tmax damage count for start year ", l))
    }
  }
}

#now create crop specific (ie masked) maps
# the overlay function needs a user defined function on the relationship between the two rasters
overlayfunction <- function(x,y) {
  return(x * y)
}
for (k in sspChoices)  {
  for (i in modelChoices)  {
    for (l in startYearChoices) {
      for (o in 1:length(cropChoices)) {
        for (m in get(cropChoices[o])) {
          print(paste0("crop: ", m))  
          modelName.lower <- tolower(i)
          yearSpan <- paste0(l, "_", l + yearRange)
          tmaxLimit <- cropCharacteristics_annual[crop %in% m, tdamage.mean]
          
          fileName_in <- paste0("tmaxGT_", tmaxLimit, "_", modelName.lower, "_", k, "_", yearSpan, ".tif")
          tdamage <- rast(paste0("data/cmip6/tmaxMonthlySums/", fileName_in))
          fileNameMask.in <- paste0("data/crops/rasterMask_", m, ".tif")
          print(paste0("fileNameMaskIn: ", fileNameMask.in))
          mask <- rast(fileNameMask.in)
          tdamage.masked <- mask(tdamage, mask)
          fileName_out <- paste0("tmaxGT_masked_", m, "_", tmaxLimit, "_", modelName.lower, "_", k, "_", yearSpan, ".tif")
          writeRaster(tdamage.masked, filename = paste0("data/cmip6/tmaxMonthlySums/", fileName_out), format = "GTiff", overwrite = TRUE)
        }
      }
    }
  }
}

# observed results
# # now do count above tmax limit for observed period
# yearSpan <- paste0(l, "_", l + yearRange)
# fileName_in <- paste0(locOfFiles, "observed/gswp3-w5e5_obsclim_tasmax_global_daily_2001_2010.tif")
# tmax <- rast(rastfileName_in)
# indices <- format(as.Date(names(tmax), format = "X%Y.%m.%d"), format = "%m")
# indices <- as.numeric(indices)
# 
# f.tmaxLimit.observed <- function(tmax, tmaxLimit, indices) {
#   tmaxSum <- tapp(tmax, indices, fun = function(x, ...){sum(x >= tmaxLimit)}) 
#   names(tmaxSum) <- month.abb
#   fileName_out <- paste0("tmaxGT_", tmaxLimit, "_observed_", yearSpan, ".tif")
#   writeRaster(tmaxSum, filename = paste0("data/cmip6/tmaxMonthlySums/", fileName_out), format = "GTiff", overwrite = TRUE)
# }
# 
# for (n in tmaxList) {
#   f.tmaxLimit.observed(tmax, tmaxLimit = n, indices)
#   print(paste("Completed tmax count for ", n, "C"))
# }

# observed results
# now do masked versions of the count above tmax limit for observed period

for (o in 1:length(cropChoices)) {
  for (m in get(cropChoices[o])) {
    print(paste0("crop: ", m))  
    modelName.lower <- tolower(i)
    tmaxLimit <- cropCharacteristics_annual[crop %in% m, tdamage.mean]
    
    fileName_in <- paste0("tmaxGT_", tmaxLimit, "_observed_", yearSpan, ".tif")
    tdamage <- rast(paste0("data/cmip6/tmaxMonthlySums/", fileName_in))
    fileNameMask.in <- paste0("data/crops/rasterMask_", m, ".tif")
    print(paste0("fileNameMaskIn: ", fileNameMask.in))
    mask <- rast(fileNameMask.in)
    tdamage.masked <- overlay(tdamage, mask, fun = overlayfunction)
    fileName_out <- paste0("tmaxGT_masked", m, "_", tmaxLimit, "_observed_", yearSpan, ".tif")
    writeRaster(tdamage.masked, filename = paste0("data/cmip6/tmaxMonthlySums/", fileName_out), format = "GTiff", overwrite = TRUE)
  }
}

# combine 10 year rasters across models to get ensemble means and coeffients of variation.

source("R/globallyUsed.R")
library(raster)
library(doParallel) #Foreach Parallel Adaptor 
library(foreach) #Provides foreach looping construct

startyearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

yearRange <- 9
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
k <- "ssp585"
l <- 2021
j = 1

hemisphereList <- c("Northern", "Southern")

# for (k in sspChoices) {
#   for (l in startyearChoices_ensemble) {
#     
varList <- c("modelChoices", "hemisphereList",  "startyearChoices_ensemble", "sspChoices", "tmpDirName")
libList <- c("raster", "data.table")

# UseCores <- detectCores() - 1 # max number of cores
# useCores <- 3 # better for memory intensive activities
# 
# cl <- clusterSetup(varList, libList, useCores = useCores)
start_time <- Sys.time()
# x <- foreach(l = startyearChoices_ensemble, .combine = rbind) %:%
#   foreach(k = sspChoices, .combine = rbind)  %dopar% {
# require(data.table)
# require(raster)
# rasterOptions(tmpdir = tmpDirName)
# dir.create(tmpDirName)
for (l in startyearChoices_ensemble) {
  for (k in sspChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l, ", pid number: ", Sys.getpid()))
    
    for (j in 1:length(hemisphereList)) {
      hemisphereName <- paste0(hemisphereList[j], "Hem")
      yearRange <- 9
      yearSpan <- paste0(l, "_", l + yearRange)
      print(paste0("hemisphere name ", hemisphereName, ", start year: ", l,  ", pid number: ", Sys.getpid()))
      rasterList <- vector(mode = "list", length = length(modelChoices))
      for (m in 1:length(modelChoices)) {
        fileNameIn <- paste0("data/cmip6/chillingHours/chillHrs", hemisphereName, "_", modelChoices.lower[m], "_", k,  "_", yearSpan, ".tif")
        print(paste0("raster file name in: ", fileNameIn,  ", pid number: ", Sys.getpid()))
        rasterList[[m]] <- brick(fileNameIn)
      }
      ras.test <- stack(rasterList)
      ras.test[ras.test < 0] <- 0 # set all negative  values to 0
      print(paste0( "Done setting negative values to 0 for hemisphere name: ", hemisphereName, ", start year: ", l))
      #    ras.test.mean <- raster::stackApply(ras.test, indices, fun = mean, na.rm = TRUE)
      ras.test.mean <- raster::calc(ras.test, fun = mean, na.rm = TRUE)
      ras.test.cv <- raster::calc(ras.test, fun = cv, na.rm = TRUE)
      fileNameMean <- paste0("data/cmip6/chillingHours/chillHrs_ensembleMean_", hemisphereName,  "_",  yearSpan, "_", k, ".tif") 
      fileNameCV <- paste0("data/cmip6/chillingHours/chillHrs_ensembleCV_", hemisphereName,  "_",  yearSpan, "_", k, ".tif")
      writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
      writeRaster(ras.test.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE)
      print(paste("fileNameMeanOut: ", fileNameMean))
      print(paste0( "Done writing out files for hemisphere: ", hemisphereName, ", start year: ", l, ", pid number: ", Sys.getpid()))
    }
    #    unlink(tmpDirName, recursive = TRUE)
    gc(TRUE) 
  }
}
stopCluster(cl)

end_time <- Sys.time()
end_time - start_time

fruits <-  c("apple", "apricot", "avocado", "berrynes", "blueberry", 
             "cherry", "cranberry", "currant", "grape", 
             "grapefruitetc", "lemonlime", "orange", "peachetc", "persimmon", "rasberry", "sourcherry", 
             "stonefruitnes", "walnut")

# the overlay function needs a user defined function on the relationship between the two rasters
overlayfunction <- function(x,y) {
  return(x * y)
}

for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    for (m in hemisphereList) {
      hemisphereName <- paste0(m, "Hem")
      
      yearSpan <- paste0(l, "_", l + yearRange)
      print(paste0("ssp choice: ", k, ", start year: ", l, " hemisphere: ", hemisphereName))
      
      for (j in 1:length(fruits)) {
        speciesName <- fruits[j]
        fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(speciesName), ".tif")
        print(paste0("fileNameMaskIn: ", fileNameMask.in))
        fileNameMean.in <- paste0("data/cmip6/chillingHours/chillHrs_ensembleMean_", hemisphereName, "_",  yearSpan, "_", k, ".tif")
        print(paste0("fileNameMean.in: ", fileNameMean.in))
        fileNameCV.in <- paste0("data/cmip6/chillingHours/chillHrs_ensembleCV_", hemisphereName, "_", yearSpan, "_", k, ".tif")
        print(paste0("fileNameCV.in: ", fileNameCV.in))
        mask <- raster(fileNameMask.in)
        meanData <- brick(fileNameMean.in)
        meanData[meanData < 0] <- 0 # set all negative values to 0
        CVData <- brick(fileNameCV.in)
        mean.masked <- overlay(meanData, mask, fun = overlayfunction)
        CV.masked <- overlay(CVData, mask, fun = overlayfunction)
        fileNameMean.masked <- paste0("data/cmip6/chillingHours/chillHrs_ensembleMean_masked_", hemisphereName, "_", speciesName, "_",  yearSpan, "_", k, ".tif")
        fileNameCV.masked <- paste0("data/cmip6/chillingHours/chillHrs_ensembleCV_masked_", hemisphereName, "_", speciesName, "_",  yearSpan, "_", k, ".tif")
        print(paste("fileNameMean.masked: ", fileNameMean.masked))
        writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
        writeRaster(CV.masked, filename = fileNameCV.masked, format = "GTiff", overwrite = TRUE)
      }
    }
  }
}

# do observed data
l <- 2001
yearSpan <- paste0(l, "_", l + yearRange)
print(paste0("ssp choice: ", k, "start year: ", l))
for (j in 1:length(fruits)) {
  cropName <- fruits[j]
  fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(cropName), ".tif")
  for (m in hemisphereList) {
    hemisphereName <- paste0(m, "Hem")
    
  print(paste0("fileNameMaskIn: ", fileNameMask.in))
  fileNameMean.in <- paste0("data/cmip6/chillingHours/chillHrs", hemisphereName, "_observed_", yearSpan, ".tif")
  
  print(paste("fileNameMean.in: ", fileNameMean.in))
  mask <- raster(fileNameMask.in)
  meanData <- brick(fileNameMean.in)
  meanData[meanData < 0] <- 0 # set all negative  values to 0
  mean.masked <- overlay(meanData, mask, fun = overlayfunction)
  fileNameMean.masked <- paste0("data/cmip6/chillingHours/chillHrs_masked_", hemisphereName, "_", cropName,  "_observed_", yearSpan, ".tif")
  print(fileNameMean.masked)
  writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
  }
}

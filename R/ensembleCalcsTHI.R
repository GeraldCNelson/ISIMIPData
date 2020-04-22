# combine 10 year rasters across models to get ensemble means and standard deviations .

source("R/globallyUsed.R")
library(raster)
library(doParallel) #Foreach Parallel Adaptor 
library(foreach) #Provides foreach looping construct

startyearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
k <- "ssp585"
l <- 2021

thiList <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.swine")

# for (k in sspChoices) {
#   for (l in startyearChoices) {
#     
varList <- c("modelChoices", "thiList",  "startyearChoices", "sspChoices", "tmpDirName")
libList <- c("raster", "data.table")

UseCores <- detectCores() - 1 # max number of cores
useCores <- 3 # better for memory intensive activities

cl <- clusterSetup(varList, libList, useCores = useCores)
start_time <- Sys.time()
# x <- foreach(l = startyearChoices, .combine = rbind) %:%
#   foreach(k = sspChoices, .combine = rbind)  %dopar% {
# require(data.table)
# require(raster)
# rasterOptions(tmpdir = tmpDirName)
# dir.create(tmpDirName)
for (l in startyearChoices) {
  for (k in sspChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l, ", pid number: ", Sys.getpid()))
    
    for (j in 1:length(thiList)) {
      speciesName <- gsub("thi.", "", thiList[j])
      yearRange <- 9
      print(paste0("species names: ", speciesName, ", start year: ", l,  ", pid number: ", Sys.getpid()))
      rasterList <- vector(mode = "list", length = length(modelChoices))
      for (m in 1:length(modelChoices)) {
        fileNameIn <- paste0("data/cmip6/THI/", thiList[j], "_", modelChoices[m],  "_", l, "_", k, ".tif")
        print(paste0("raster file name in: ", fileNameIn,  ", pid number: ", Sys.getpid()))
        rasterList[[m]] <- brick(fileNameIn)
        names(rasterList[[m]]) <- month.abb
      }
      ras.test <- stack(rasterList)
      ras.test[ras.test < 0] <- 0 # set all negative THI values to 0
      print(paste0( "Done setting negative THI values to 0 for species names: ", speciesName, ", start year: ", l))
      indices <- format(as.Date(names(ras.test), format = "%b.%d"), format = "%m")
      indices <- as.numeric(indices)
      print(paste0( "Done setting doing raster indices for ras.test stack for species: ", speciesName, ", start year: ", l))
      ras.test.mean <- raster::stackApply(ras.test, indices, fun = mean, na.rm = TRUE)
      ras.test.sd <- raster::stackApply(ras.test, indices, fun = sd, na.rm = TRUE)
      names(ras.test.mean) <- month.abb
      names(ras.test.sd) <- month.abb
      print(paste0( "Done updating raster names with month.abb for species: ", speciesName, ", start year: ", l))
      fileNameMean <- paste0("data/cmip6/THI/THI_ensembleMean_", speciesName,  "_",  yearSpan, "_", k, ".tif") 
      fileNameSD <- paste0("data/cmip6/THI/THI_ensembleSD_", speciesName,  "_",  yearSpan, "_", k, ".tif")
      writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
      writeRaster(ras.test.sd, filename = fileNameSD, format = "GTiff", overwrite = TRUE)
      print(paste("fileNameMeanOut: ", fileNameMean))
      print(paste0( "Done writing out files for species: ", speciesName, ", start year: ", l, ", pid number: ", Sys.getpid()))
    }
    #    unlink(tmpDirName, recursive = TRUE)
    gc(TRUE) 
  }
}

stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

# apply masks, can only do this to animals we have in THIlist and that have area mask raster
thiListReduced <- thiList[!thiList %in% c("thi.yak", "thi.broiler", "thi.layer")]

# the overlay function needs a user defined function on the relationship between the two rasters
overlayfunction <- function(x,y) {
  return(x * y)
}
for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l))
    
    for (j in 1:length(thiListReduced)) {
      speciesName <- gsub("thi.", "", thiListReduced[j])
      fileNameMask.in <- paste0("data/animals/rasterMask_", tolower(speciesName), ".tif")
      print(paste0("fileNameMaskIn: ", fileNameMask.in))
      fileNameMean.in <- paste0("data/cmip6/THI/THI_ensembleMean_", speciesName, "_",  yearSpan, "_", k, ".tif")
      print(paste0("fileNameMean.in: ", fileNameMean.in))
      fileNameSD.in <- paste0("data/cmip6/THI/THI_ensembleSD_", speciesName, "_", yearSpan, "_", k, ".tif")
      print(paste0("fileNameSD.in: ", fileNameSD.in))
      mask <- raster(fileNameMask.in)
      meanData <- brick(fileNameMean.in)
      SDData <- brick(fileNameSD.in)
      mean.masked <- overlay(meanData, mask, fun = overlayfunction)
      names(mean.masked) <- month.abb
      SD.masked <- overlay(SDData, mask, fun = overlayfunction)
      names(SD.masked) <- month.abb
      fileNameMean.masked <- paste0("data/cmip6/THI/THI_ensembleMean_masked_",speciesName, "_",  l, "_", k, ".tif")
      fileNameSD.masked <- paste0("data/cmip6/THI/THI_ensembleSD_masked_", speciesName, "_",  l, "_", k, ".tif")
      print(fileNameMean.masked)
      writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
      writeRaster(SD.masked, filename = fileNameSD.masked, format = "GTiff", overwrite = TRUE)
    }
  }
}

# do observed data
l <- 2001
yearSpan <- paste0(l, "_", l + yearRange)
print(paste0("ssp choice: ", k, "start year: ", l))
for (j in 1:length(thiListReduced)) {
  speciesName <- gsub("thi.", "", thiListReduced[j])
  fileNameMask.in <- paste0("data/animals/rasterMask_", tolower(speciesName), ".tif")
  print(paste0("fileNameMaskIn: ", fileNameMask.in))
  fileNameMean.in <- paste0("data/cmip6/THI/", thiListReduced[j], "_observed_", yearSpan, ".tif")
  print(paste("fileNameMean.in: ", fileNameMean.in))
  mask <- raster(fileNameMask.in)
  meanData <- brick(fileNameMean.in)
  mean.masked <- overlay(meanData, mask, fun = overlayfunction)
  names(mean.masked) <- month.abb
  fileNameMean.masked <- paste0("data/cmip6/THI/THI_masked_",speciesName, "_observed_", yearSpan, ".tif")
  print(fileNameMean.masked)
  writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
}

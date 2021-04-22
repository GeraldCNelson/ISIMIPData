# combine 10 year rasters across models to get ensemble means and coeffients of variation

source("R/globallyUsed.R")
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct

startYearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

yearRange <- 9
sspChoices <- c("ssp126", "ssp585") #"ssp126", 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
k <- "ssp585"
l <- 2021
j = 1 # cattle

thiList <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.pigs")

# for (k in sspChoices) {
#   for (l in startYearChoices_ensemble) {
#     
# varList <- c("modelChoices", "thiList",  "startYearChoices_ensemble", "sspChoices", "tmpDirName")
# libList <- c("rast", "data.table")

# UseCores <- detectCores() - 1 # max number of cores
# useCores <- 3 # better for memory intensive activities
# cl <- clusterSetup(varList, libList, useCores = useCores)

start_time <- Sys.time()
# x <- foreach(l = startYearChoices_ensemble, .combine = rbind) %:%
#   foreach(k = sspChoices, .combine = rbind)  %dopar% {

readRast <- function(m) {
  fileName_in <- paste0("data/cmip6/THI/", thiList[j], "_", m,  "_", yearSpan, "_", k, ".tif")
  r <- rast(fileName_in)
  names(r) <- month.abb
  r
}

for (k in sspChoices) {
  for (l in startYearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l, ", pid number: ", Sys.getpid()))
    
    for (j in 1:length(thiList)) {
      speciesName <- gsub("thi.", "", thiList[j])
      yearRange <- 9
      print(paste0("species names: ", speciesName, ", start year: ", l,  ", pid number: ", Sys.getpid()))
        
      # make a list of SpatRasters
      x <- lapply(modelChoices.lower, readRast)
      r <- rast(x)
      r[r < 0] <- 0 # set all negative THI values to 0
      r.mean <- tapp(r, 1:12, fun = mean)
      r.cv <- tapp(r, 1:12, fun = raster::cv)
      names(r.mean) <- month.abb
      names(r.cv) <- month.abb
      
      fileNameMean <- paste0("data/cmip6/THI/THI_ensembleMean_", speciesName,  "_",  yearSpan, "_", k, ".tif") 
      fileNameCV <- paste0("data/cmip6/THI/THI_ensembleCV_", speciesName,  "_",  yearSpan, "_", k, ".tif")
      writeRaster(r.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      writeRaster(r.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      print(paste("fileNameMeanOut: ", fileNameMean))
      print(paste0( "Done writing out files for species: ", speciesName, ", start year: ", l, ", pid number: ", Sys.getpid()))
    }
    #    unlink(tmpDirName, recursive = TRUE)
    gc(reset = FALSE, full = TRUE) 
  }
}

# stopCluster(cl)


# apply masks, can only do this to animals we have in THIlist and that have area mask raster
thiListReduced <- thiList[!thiList %in% c("thi.yak", "thi.broiler", "thi.layer")]

# # the lapp function needs a user defined function on the relationship between the two rasters
# lappfunction_mask <- function(x,y) {
#   return(x * y)
# }
for (k in sspChoices) {
  for (l in startYearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l))
    
    for (j in 1:length(thiListReduced)) {
      speciesName <- gsub("thi.", "", thiListReduced[j])
      fileNameMask.in <- paste0("data/animals/rasterMask_", tolower(speciesName), ".tif")
      print(paste0("fileNameMaskIn: ", fileNameMask.in))
      fileNameMean.in <- paste0("data/cmip6/THI/THI_ensembleMean_", speciesName, "_",  yearSpan, "_", k, ".tif")
      print(paste0("fileNameMean.in: ", fileNameMean.in))
      fileNameCV.in <- paste0("data/cmip6/THI/THI_ensembleCV_", speciesName, "_", yearSpan, "_", k, ".tif")
      print(paste0("fileNameCV.in: ", fileNameCV.in))
      mask <- rast(fileNameMask.in)
      meanData <- rast(fileNameMean.in)
      meanData[meanData < 0] <- 0 # set all negative THI values to 0
      CVData <- rast(fileNameCV.in)
      mean.masked <- mask(meanData, mask)
      #      mean.masked <- lapp(meanData, mask, fun = lappfunction_mask)
      names(mean.masked) <- month.abb
      CV.masked <- mask(CVData, mask)
      #   CV.masked <- lapp(CVData, mask, fun = lappfunction_mask)
      names(CV.masked) <- month.abb
      fileNameMean.masked <- paste0("data/cmip6/THI/THI_ensembleMean_masked_",speciesName, "_",  yearSpan, "_", k, ".tif")
      fileNameCV.masked <- paste0("data/cmip6/THI/THI_ensembleCV_masked_", speciesName, "_",  yearSpan, "_", k, ".tif")
      print(fileNameMean.masked)
      writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      writeRaster(CV.masked, filename = fileNameCV.masked, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
    }
  }
}

# do historical data
l <- 2001
yearSpan <- paste0(l, "_", l + yearRange)
print(paste0("ssp choice: ", k, "start year: ", l))
for (j in 1:length(thiListReduced)) {
  speciesName <- gsub("thi.", "", thiListReduced[j])
  fileNameMask.in <- paste0("data/animals/rasterMask_", tolower(speciesName), ".tif")
  print(paste0("fileNameMaskIn: ", fileNameMask.in))
  fileNameMean.in <- paste0("data/cmip6/THI/", thiListReduced[j], "_historical_", yearSpan, ".tif")
  print(paste("fileNameMean.in: ", fileNameMean.in))
  mask <- rast(fileNameMask.in)
  meanData <- rast(fileNameMean.in)
  meanData[meanData < 0] <- 0 # set all negative THI values to 0
  mean.masked <- mask(meanData, mask)
  names(mean.masked) <- month.abb
  fileNameMean.masked <- paste0("data/cmip6/THI/THI_masked_",speciesName, "_historical_", yearSpan, ".tif")
  print(fileNameMean.masked)
  writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
}

# # do observed data
# l <- 2001
# yearSpan <- paste0(l, "_", l + yearRange)
# print(paste0("ssp choice: ", k, "start year: ", l))
# for (j in 1:length(thiListReduced)) {
#   speciesName <- gsub("thi.", "", thiListReduced[j])
#   fileNameMask.in <- paste0("data/animals/rasterMask_", tolower(speciesName), ".tif")
#   print(paste0("fileNameMaskIn: ", fileNameMask.in))
#   fileNameMean.in <- paste0("data/cmip6/THI/", thiListReduced[j], "_observed_", yearSpan, ".tif")
#   print(paste("fileNameMean.in: ", fileNameMean.in))
#   mask <- rast(fileNameMask.in)
#   meanData <- rast(fileNameMean.in)
#   meanData[meanData < 0] <- 0 # set all negative THI values to 0
#   mean.masked <- mask(meanData, mask)
#   names(mean.masked) <- month.abb
#   fileNameMean.masked <- paste0("data/cmip6/THI/THI_masked_",speciesName, "_observed_", yearSpan, ".tif")
#   print(fileNameMean.masked)
#   writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
# }

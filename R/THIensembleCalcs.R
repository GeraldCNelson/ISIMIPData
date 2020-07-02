# combine 10 year rasters across models to get ensemble means and coeffients of variation

source("R/globallyUsed.R")
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct

startyearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

yearRange <- 9
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
k <- "ssp585"
l <- 2021
j = 1 # cattle

thiList <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.swine")

# for (k in sspChoices) {
#   for (l in startyearChoices_ensemble) {
#     
# varList <- c("modelChoices", "thiList",  "startyearChoices_ensemble", "sspChoices", "tmpDirName")
# libList <- c("rast", "data.table")

# UseCores <- detectCores() - 1 # max number of cores
# useCores <- 3 # better for memory intensive activities
# cl <- clusterSetup(varList, libList, useCores = useCores)

start_time <- Sys.time()
# x <- foreach(l = startyearChoices_ensemble, .combine = rbind) %:%
#   foreach(k = sspChoices, .combine = rbind)  %dopar% {

readRast <- function(m) {
  fileNameIn <- paste0("data/cmip6/THI/", thiList[j], "_", m,  "_", yearSpan, "_", k, ".tif")
  r <- rast(fileNameIn)
  names(r) <- month.abb
  r
}

for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l, ", pid number: ", Sys.getpid()))
    
    for (j in 1:length(thiList)) {
      speciesName <- gsub("thi.", "", thiList[j])
      yearRange <- 9
      print(paste0("species names: ", speciesName, ", start year: ", l,  ", pid number: ", Sys.getpid()))
      # rasterList <- vector(length = 0)
      #  for (m in 1:length(modelChoices)) {
      #  #   fileNameIn <- paste0("data/cmip6/THI/", thiList[j], "_", modelChoices[m],  "_", yearSpan, "_", k, ".tif")
      #   print(paste0("raster file name in: ", fileNameIn,  ", pid number: ", Sys.getpid()))
      #   rtemp <- rast(fileNameIn)
      #   names(rtemp) <- month.abb
      #          rasterList <- c(rasterList, rtemp)
      # }
      #       rasterList <- rast(rasterList)
      #        rasterList[rasterList < 0] <- 0 # set all negative THI values to 0
      #       print(paste0( "Done setting negative THI values to 0 for species names: ", speciesName, ", start year: ", l))
      #  #     indices <- format(as.Date(names(rasterList), format = "%b"), format = "%m")
      # #      indices <- as.numeric(indices)
      # #      index <- month.abb
      #       index <-c(1,2,3,4,5,6,7,8,9,10,11,12)
      
      
      # make a list of SpatRasters
      x <- lapply(modelChoices, readRast)
      
      # approach 1 (yours)
      startTime <- Sys.time()
      r <- rast(x)
      r[r < 0] <- 0 # set all negative THI values to 0
      r.mean <- tapp(r, 1:12, fun = mean)
      r.cv <- tapp(r, 1:12, fun = raster::cv)
      names(r.mean) <- month.abb
      names(r.cv) <- month.abb
      
      endTime <- Sys.time()
      endTime - startTime
      
      # # approach 2
      # startTime <- Sys.time()
      # rs <- rstk(x)
      # rs.mean2 <- lapp(rs, fun = terra::mean)
      # rs.cv2 <- lapp(rs, fun = raster::cv)
      # endTime <- Sys.time()
      # endTime - startTime
      
      # print(paste0( "Done setting doing raster indices for rasterList stack for species: ", speciesName, ", start year: ", l))
      # rasterList.mean <- tapp(rasterList, index, fun = mean)
      # rasterList.cv <- tapp(rasterList, index, fun = raster::cv) #, na.rm = TRUE)
      # names(rasterList.mean) <- month.abb
      # names(rasterList.cv) <- month.abb
      # print(paste0( "Done updating raster names with month.abb for species: ", speciesName, ", start year: ", l))
      fileNameMean <- paste0("data/cmip6/THI/THI_ensembleMean_", speciesName,  "_",  yearSpan, "_", k, ".tif") 
      fileNameCV <- paste0("data/cmip6/THI/THI_ensembleCV_", speciesName,  "_",  yearSpan, "_", k, ".tif")
      writeRaster(r.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
      writeRaster(r.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE)
      print(paste("fileNameMeanOut: ", fileNameMean))
      print(paste0( "Done writing out files for species: ", speciesName, ", start year: ", l, ", pid number: ", Sys.getpid()))
    }
    #    unlink(tmpDirName, recursive = TRUE)
    gc(reset = FALSE, full = TRUE) 
  }
}

# stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

# apply masks, can only do this to animals we have in THIlist and that have area mask raster
thiListReduced <- thiList[!thiList %in% c("thi.yak", "thi.broiler", "thi.layer")]

# # the lapp function needs a user defined function on the relationship between the two rasters
# lappfunction_mask <- function(x,y) {
#   return(x * y)
# }
for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
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
      writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
      writeRaster(CV.masked, filename = fileNameCV.masked, format = "GTiff", overwrite = TRUE)
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
  mask <- rast(fileNameMask.in)
  meanData <- rast(fileNameMean.in)
  meanData[meanData < 0] <- 0 # set all negative THI values to 0
  mean.masked <- mask(meanData, mask)
  names(mean.masked) <- month.abb
  fileNameMean.masked <- paste0("data/cmip6/THI/THI_masked_",speciesName, "_observed_", yearSpan, ".tif")
  print(fileNameMean.masked)
  writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
}

# combine 10 year rasters across models to get ensemble means and coeffients of variation

source("R/globallyUsed.R")

startyearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

yearRange <- 9
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)

# commented out, now in the globallyUsed.R script
#ann_crop_temp_table <- as.data.table(read_excel("data-raw/crops/ann_crop_temp_table_summary_02052020.xlsx", range = "A1:S26"))
#setnames(ann_crop_temp_table, old = names(ann_crop_temp_table), new = make.names(names(ann_crop_temp_table)))
cropChoices <- unique(ann_crop_temp_table$crop)
#cropChoices <- c("Barley")
#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
m <- "Broadbean"


# UseCores <- detectCores() - 1 # max number of cores
# useCores <- 3 # better for memory intensive activities
# cl <- clusterSetup(varList, libList, useCores = useCores)

start_time <- Sys.time()
# x <- foreach(l = startyearChoices_ensemble, .combine = rbind) %:%
#   foreach(k = sspChoices, .combine = rbind)  %dopar% {
# require(data.table)
for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l, ", pid number: ", Sys.getpid()))
    
    for (j in 1:length(cropChoices)) {
      cropName <- cropChoices[j]
      yearRange <- 9
      print(paste0("crop names: ", cropName, ", start year: ", l,  ", pid number: ", Sys.getpid()))
      rasterList <- vector(mode = "list", length = length(modelChoices.lower))
      for (m in 1:length(modelChoices.lower)) {
        fileNameIn <- paste0("data/cmip6/growingDegreeDays/", modelChoices.lower[m], "_", cropName, "_", k, "_", "gdd_global_daily", "_", yearSpan,  ".tif")
        print(paste0("raster file name in: ", fileNameIn,  ", pid number: ", Sys.getpid()))
        rasterList[[m]] <- rast(fileNameIn)
#        names(rasterList[[m]]) <- month.abb
      }
      ras.test <- rast(rasterList)
  #    ras.test[ras.test < 0] <- 0 # set all negative THI values to 0
   #   print(paste0( "Done setting negative THI values to 0 for species names: ", speciesName, ", start year: ", l))
      indices <- format(as.Date(names(ras.test), format = "%b.%d"), format = "%m")
      indices <- as.numeric(indices)
      print(paste0( "Done setting doing raster indices for ras.test stack for crop: ", cropName, ", start year: ", l))
      ras.test.mean <- tapp(ras.test, indices, fun = mean, na.rm = TRUE)
      ras.test.cv <- tapp(ras.test, indices, fun = cv, na.rm = TRUE)
      names(ras.test.mean) <- month.abb
      names(ras.test.cv) <- month.abb
      print(paste0( "Done updating raster names with month.abb for crop: ", cropName, ", start year: ", l))
      fileNameMean <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleMean_", cropName,  "_",  yearSpan, "_", k, ".tif") 
      fileNameCV <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleCV_", cropName,  "_",  yearSpan, "_", k, ".tif")
      writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
      writeRaster(ras.test.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE)
      print(paste("fileNameMeanOut: ", fileNameMean))
      print(paste0( "Done writing out files for crop: ", cropName, ", start year: ", l, ", pid number: ", Sys.getpid()))
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

# the overlay function needs a user defined function on the relationship between the two rasters
overlayfunction <- function(x,y) {
  return(x * y)
}
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
      mean.masked <- overlay(meanData, mask, fun = overlayfunction)
      names(mean.masked) <- month.abb
      CV.masked <- overlay(CVData, mask, fun = overlayfunction)
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
  mean.masked <- overlay(meanData, mask, fun = overlayfunction)
  names(mean.masked) <- month.abb
  fileNameMean.masked <- paste0("data/cmip6/THI/THI_masked_",speciesName, "_observed_", yearSpan, ".tif")
  print(fileNameMean.masked)
  writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
}

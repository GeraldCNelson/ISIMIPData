# combine 10 year rasters across models to get ensemble means and coefficients of variation

source("R/globallyUsed.R")
# library(raster)
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct

startyearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

yearRange <- 9
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)

tmaxList <- sort(unique(ann_crop_temp_table$tdamage.mean)) #get all the damaging temperature levels for the annual crops

#test values
k <- "ssp585"
l <- 2021

start_time <- Sys.time()
# x <- foreach(l = startyearChoices_ensemble, .combine = rbind) %:%
#   foreach(k = sspChoices, .combine = rbind)  %dopar% {
# require(data.table)
# require(raster)
# rasterOptions(tmpdir = tmpDirName)
# dir.create(tmpDirName)
for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l, ", pid number: ", Sys.getpid()))
    for (o in 1:length(cropChoices)) {
      for (m in get(cropChoices[o])) {
        print(paste0("crop: ", m))  
        tmaxLimit <- ann_crop_temp_table[crop %in% m, tdamage.mean]
        j <- tmaxLimit
        print(paste0("tmax value: ", tmaxLimit, ", start year: ", l,  ", pid number: ", Sys.getpid()))
        rasterList <- vector(mode = "list", length = length(modelChoices))
        for (n in 1:length(modelChoices)) {
          modelName.lower <- tolower(modelChoices[n])
          fileName <- paste0("tmaxGT_masked", m, "_", tmaxLimit, "_", modelName.lower, "_", k, "_", yearSpan, ".tif")
          
          fileNameIn <- paste0("data/cmip6/tmaxMonthlySums/", fileName)
          print(paste0("raster file name in: ", fileNameIn,  ", pid number: ", Sys.getpid()))
          rasterList[[n]] <- brick(fileNameIn)
          names(rasterList[[n]]) <- month.abb
        }
        ras.test <- stack(rasterList)
        indices <- format(as.Date(names(ras.test), format = "%b.%d"), format = "%m")
        indices <- as.numeric(indices)
        print(paste0( "Done setting doing raster indices for ras.test stack for tmax: ", j, " for crop ", m, ", start year: ", l))
        ras.test.mean <- raster::stackApply(ras.test, indices, fun = mean, na.rm = TRUE)
        ras.test.cv <- raster::stackApply(ras.test, indices, fun = cv, na.rm = TRUE)
        names(ras.test.mean) <- month.abb
        names(ras.test.cv) <- month.abb
        fileNameMean <- paste0("data/cmip6/tmaxMonthlySums/tmaxMonthlySums_ensembleMean_", m, "_", j,  "_", k, "_",  yearSpan, ".tif") 
        fileNameCV <- paste0("data/cmip6/tmaxMonthlySums/tmaxMonthlySums_ensembleCV_", m, "_", j,  "_", k, "_",  yearSpan, ".tif") 
        writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
        writeRaster(ras.test.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE)
        print(paste("fileNameMeanOut: ", fileNameMean))
        print(paste0( "Done writing out files for max temp: ", j, "for crop ", m, ", start year: ", l, ", pid number: ", Sys.getpid()))
      }
      gc(reset = FALSE, full = TRUE) 
    }
  }
}

# stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

# do observed data
l <- 2001
yearSpan <- paste0(l, "_", l + yearRange)
for (o in 1:length(cropChoices)) {
  for (m in get(cropChoices[o])) {
    print(paste0("crop: ", m))  
    tmaxLimit <- ann_crop_temp_table[crop %in% m, tdamage.mean]
    j <- tmaxLimit
    
    rasterList <- vector(mode = "list", length = length(modelChoices))
    for (n in 1:length(modelChoices)) {
      modelName.lower <- tolower(modelChoices[n])
      fileName <- paste0("tmaxGT_masked", m, "_", tmaxLimit, "_observed_", yearSpan, ".tif")
      
      fileNameIn <- paste0("data/cmip6/tmaxMonthlySums/", fileName)
      print(paste0("raster file name in: ", fileNameIn,  ", pid number: ", Sys.getpid()))
      rasterList[[n]] <- brick(fileNameIn)
      names(rasterList[[n]]) <- month.abb
    }
    ras.test <- stack(rasterList)
    indices <- format(as.Date(names(ras.test), format = "%b.%d"), format = "%m")
    indices <- as.numeric(indices)
    print(paste0( "Done setting doing raster indices for ras.test stack for tmax: ", j, " for crop ", m, ", start year: ", l))
    ras.test.mean <- raster::stackApply(ras.test, indices, fun = mean, na.rm = TRUE)
    ras.test.cv <- raster::stackApply(ras.test, indices, fun = cv, na.rm = TRUE)
    names(ras.test.mean) <- month.abb
    names(ras.test.cv) <- month.abb
    fileNameMean <- paste0("data/cmip6/tmaxMonthlySums/tmaxMonthlySums_ensembleMean_", m, "_", j, "_observed_",  yearSpan, ".tif") 
    fileNameCV <- paste0("data/cmip6/tmaxMonthlySums/tmaxMonthlySums_ensembleCV_", m, "_", j,  "_observed_",  yearSpan, ".tif") 
    writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
    writeRaster(ras.test.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE)
    print(paste("fileNameMeanOut: ", fileNameMean))
    print(paste0( "Done writing out files for max temp: ", j, "for crop ", m, ", start year: ", l, ", pid number: ", Sys.getpid()))
  }
}

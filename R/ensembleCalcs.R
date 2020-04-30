# combine 10 year rasters across models to get ensemble means and standard deviations .

source("R/globallyUsed.R")
library(raster)
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct

startyearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
k <- "ssp585"
l <- 2021

IPCC_WG2_Ch5_crop_temperature_table <- read_excel("data-raw/crops/Crop_temperature_table_summary_29042020.xlsx")

for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, "start year: ", l))
    
    for (m in 1:nrow(IPCC_WG2_Ch5_crop_temperature_table)) {
      cropName <- as.character(IPCC_WG2_Ch5_crop_temperature_table[m, "crop"])
      tdamage_mean <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "tdamage mean"])
      yearRange <- 9
      print(paste0(cropName, ", damage temp: ", tdamage_mean, " start year: ", l))
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      rasterList <- vector(mode = "list", length = length(modelChoices))
      for (j in 1:length(modelChoices)) {
        fileNameIn <- paste0("data/cmip6/damageTemp/tdamage_mean_", cropName, "_", tdamage_mean, "C_",  modelChoices.lower[j], "_", k,  "_", yearSpan, ".tif")
        print(paste("fileNameIn: ", fileNameIn))
        rasterList[[j]] <- brick(fileNameIn)
        names(rasterList[[j]]) <- month.abb
      }
      ras.test <- stack(rasterList)
      indices <- format(as.Date(names(ras.test), format = "%b.%d"), format = "%m")
      indices <- as.numeric(indices)
      ras.test.mean <- raster::stackApply(ras.test, indices, fun = mean, na.rm = TRUE)
      ras.test.sd <- raster::stackApply(ras.test, indices, fun = sd, na.rm = TRUE)
      names(ras.test.mean) <- month.abb
      names(ras.test.sd) <- month.abb
      fileNameMean <- paste0("data/cmip6/damageTemp/tdamage_ensembleMean_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      fileNameSD <- paste0("data/cmip6/damageTemp/tdamage_ensembleSD_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
      writeRaster(ras.test.sd, filename = fileNameSD, format = "GTiff", overwrite = TRUE)
    }
  }
}

# apply masks, can only do this to crops we have in the temperature lookup table
# the overlay function needs a user defined function on the relationship between the two rasters
overlayfunction <- function(x,y) {
  return(x * y)
}
for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, "start year: ", l))
    for (m in 1:nrow(IPCC_WG2_Ch5_crop_temperature_table)) {
      cropName <- as.character(IPCC_WG2_Ch5_crop_temperature_table[m, "crop"])
      tdamage_mean <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "tdamage mean"])
      fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(cropName), ".tif")
      print(paste("fileNameMaskIn: ", fileNameMask.in))
      fileNameMean.in <- paste0("data/cmip6/damageTemp/tdamage_ensembleMean_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      print(paste("fileNameMean.in: ", fileNameMean.in))
      fileNameSD.in <- paste0("data/cmip6/damageTemp/tdamage_ensembleSD_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      print(paste("fileNameSD.in: ", fileNameSD.in))
      mask <- raster(fileNameMask.in)
      meanData <- brick(fileNameMean.in)
      SDData <- brick(fileNameSD.in)
      mean.masked <- overlay(meanData, mask, fun = overlayfunction)
      names(mean.masked) <- month.abb
      SD.masked <- overlay(SDData, mask, fun = overlayfunction)
      names(SD.masked) <- month.abb
      fileNameMean.masked <- paste0("data/cmip6/damageTemp/tdamage_ensembleMean_masked_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      fileNameSD.masked <- paste0("data/cmip6/damageTemp/tdamage_ensembleSD_masked_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      print(fileNameMean.masked)
      writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
      writeRaster(SD.masked, filename = fileNameSD.masked, format = "GTiff", overwrite = TRUE)
    }
  }
  # do observed data
  l <- 2001
  yearSpan <- paste0(l, "_", l + yearRange)
  print(paste0("ssp choice: ", k, "start year: ", l))
  for (m in 1:nrow(IPCC_WG2_Ch5_crop_temperature_table)) {
    cropName <- as.character(IPCC_WG2_Ch5_crop_temperature_table[m, "crop"])
    tdamage_mean <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "tdamage mean"])
    fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(cropName), ".tif")
    print(paste("fileNameMaskIn: ", fileNameMask.in))
    fileNameMean.in <- paste0("data/cmip6/damageTemp/tdamage_mean_", cropName, "_", tdamage_mean, "C",  "_observed_", yearSpan, ".tif")
    print(paste("fileNameMean.in: ", fileNameMean.in))
    mask <- raster(fileNameMask.in)
    meanData <- brick(fileNameMean.in)
    mean.masked <- overlay(meanData, mask, fun = overlayfunction)
    names(mean.masked) <- month.abb
    fileNameMean.masked <- paste0("data/cmip6/damageTemp/tdamage_mean_masked_", cropName, "_", tdamage_mean, "C",  "_observed_", yearSpan, ".tif")
    writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
  }
}


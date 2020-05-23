# combine 10 year rasters across models to get ensemble means and coeffients of variation

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

# commented out, now in the globallyUsed.R script
#IPCC_WG2_Ch5_crop_temperature_table <- as.data.table(read_excel("data-raw/crops/Crop_temperature_table_summary_02052020.xlsx", range = "A1:S26"))
#setnames(IPCC_WG2_Ch5_crop_temperature_table, old = names(IPCC_WG2_Ch5_crop_temperature_table), new = make.names(names(IPCC_WG2_Ch5_crop_temperature_table)))

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
        print("fileNameIn: ", fileNameIn)
        rasterList[[j]] <- brick(fileNameIn)
        names(rasterList[[j]]) <- month.abb
      }
      ras.test <- stack(rasterList)
      indices <- format(as.Date(names(ras.test), format = "%b.%d"), format = "%m")
      indices <- as.numeric(indices)
      ras.test.mean <- raster::stackApply(ras.test, indices, fun = mean, na.rm = TRUE)
      ras.test.cv <- raster::stackApply(ras.test, indices, fun = cv, na.rm = TRUE)
      names(ras.test.mean) <- month.abb
      names(ras.test.cv) <- month.abb
      fileNameMean <- paste0("data/cmip6/damageTemp/tdamage_ensembleMean_", cropName, "_", tdamage_mean, "C_",  modelName.lower, "_", k,  "_", yearSpan, ".tif")
      fileNameCV <- paste0("data/cmip6/damageTemp/tdamage_ensembleCV_", cropName, "_", tdamage_mean, "C_",  modelName.lower, "_", k,  "_", yearSpan, ".tif")
      writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
      writeRaster(ras.test.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE)
    }
  }
}


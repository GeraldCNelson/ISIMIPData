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
      filler <- fixFiller(i)
      yearRange <- 9
      print(paste0(cropName, ", damage temp: ", tdamage_mean, " start year: ", l))
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      rasterList <- vector(mode = "list", length = length(modelChoices))
      for (j in 1:length(modelChoices)) {
        filler <- fixFiller(modelChoices[j])
        fileNameIn <- paste0("data/cmip6/damageTemp/tdamage_mean_", cropName, "_", tdamage_mean, "C_",  modelChoices.lower[j], "_", k, "_", filler,  "_", yearSpan, ".tif")
        print("fileNameIn: ", fileNameIn)
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
      fileNameMean <- paste0("data/cmip6/damageTemp/tdamage_ensembleMean_", cropName, "_", tdamage_mean, "C_",  modelName.lower, "_", k, "_", filler,  "_", yearSpan, ".tif")
      fileNameSD <- paste0("data/cmip6/damageTemp/tdamage_ensembleSD_", cropName, "_", tdamage_mean, "C_",  modelName.lower, "_", k, "_", filler,  "_", yearSpan, ".tif")
      writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
      writeRaster(ras.test.sd, filename = fileNameSD, format = "GTiff", overwrite = TRUE)
    }
  }
}


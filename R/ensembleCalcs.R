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
#ann_crop_temp_table <- as.data.table(read_excel("data-raw/crops/ann_crop_temp_table_summary_02052020.xlsx", range = "A1:S26"))
#setnames(ann_crop_temp_table, old = names(ann_crop_temp_table), new = make.names(names(ann_crop_temp_table)))

for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, "start year: ", l))
    
    for (m in 1:nrow(ann_crop_temp_table)) {
      cropName <- as.character(ann_crop_temp_table[m, "crop"])
      tdamage_mean <- as.numeric(ann_crop_temp_table[m, "tdamage mean"])
      yearRange <- 9
      print(paste0(cropName, ", damage temp: ", tdamage_mean, " start year: ", l))
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      rasterList <- vector(mode = "list", length = length(modelChoices))
      for (j in 1:length(modelChoices)) {
        fileNameIn <- paste0("data/cmip6/damageTemp/tdamage_mean_", cropName, "_", tdamage_mean, "C_",  modelChoices.lower[j], "_", k,  "_", yearSpan, ".tif")
        print(paste("fileNameIn: ", fileNameIn))
        rasterList[[j]] <- rastfileNameIn)
        names(rasterList[[j]]) <- month.abb
      }
      ras.test <- stack(rasterList)
      indices <- format(as.Date(names(ras.test), format = "%b.%d"), format = "%m")
      indices <- as.numeric(indices)
      ras.test.mean <- tapp(ras.test, indices, fun = mean, na.rm = TRUE)
      ras.test.cv <- tapp(ras.test, indices, fun = cv, na.rm = TRUE)
      names(ras.test.mean) <- month.abb
      names(ras.test.cv) <- month.abb
      fileNameMean <- paste0("data/cmip6/damageTemp/tdamage_ensembleMean_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      fileNameCV <- paste0("data/cmip6/damageTemp/tdamage_ensembleCV_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
      writeRaster(ras.test.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE)
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
    for (m in 1:nrow(ann_crop_temp_table)) {
      cropName <- as.character(ann_crop_temp_table[m, "crop"])
      tdamage_mean <- as.numeric(ann_crop_temp_table[m, "tdamage mean"])
      fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(cropName), ".tif")
      print(paste("fileNameMaskIn: ", fileNameMask.in))
      fileNameMean.in <- paste0("data/cmip6/damageTemp/tdamage_ensembleMean_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      print(paste("fileNameMean.in: ", fileNameMean.in))
      fileNameCV.in <- paste0("data/cmip6/damageTemp/tdamage_ensembleCV_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      print(paste("fileNameCV.in: ", fileNameCV.in))
      mask <- rast(fileNameMask.in)
      meanData <- rastfileNameMean.in)
      CVData <- rastfileNameCV.in)
      mean.masked <- overlay(meanData, mask, fun = overlayfunction)
      names(mean.masked) <- month.abb
      CV.masked <- overlay(CVData, mask, fun = overlayfunction)
      names(CV.masked) <- month.abb
      fileNameMean.masked <- paste0("data/cmip6/damageTemp/tdamage_ensembleMean_masked_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      fileNameCV.masked <- paste0("data/cmip6/damageTemp/tdamage_ensembleCV_masked_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      print(fileNameMean.masked)
      writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
      writeRaster(CV.masked, filename = fileNameCV.masked, format = "GTiff", overwrite = TRUE)
    }
  }
  # do observed data
  l <- 2001
  yearSpan <- paste0(l, "_", l + yearRange)
  print(paste0("ssp choice: ", k, "start year: ", l))
  for (m in 1:nrow(ann_crop_temp_table)) {
    cropName <- as.character(ann_crop_temp_table[m, "crop"])
    tdamage_mean <- as.numeric(ann_crop_temp_table[m, "tdamage mean"])
    fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(cropName), ".tif")
    print(paste("fileNameMaskIn: ", fileNameMask.in))
    fileNameMean.in <- paste0("data/cmip6/damageTemp/tdamage_mean_", cropName, "_", tdamage_mean, "C",  "_observed_", yearSpan, ".tif")
    print(paste("fileNameMean.in: ", fileNameMean.in))
    mask <- rast(fileNameMask.in)
    meanData <- rastfileNameMean.in)
    mean.masked <- overlay(meanData, mask, fun = overlayfunction)
    names(mean.masked) <- month.abb
    fileNameMean.masked <- paste0("data/cmip6/damageTemp/tdamage_mean_masked_", cropName, "_", tdamage_mean, "C",  "_observed_", yearSpan, ".tif")
    writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
  }
}


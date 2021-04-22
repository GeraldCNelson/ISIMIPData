# convert Maize to maize in all file names

fileNamesToCheck <- list.files(paste0(gddDaysFilesLoc, "yearly/"), full.names = TRUE)
fileNamesToCheck <- fileNamesToCheck[!grepl("aux.xml", fileNamesToCheck, fixed = TRUE)]
fileNamesToCheck <- gsub("//", "/", fileNamesToCheck)

for (i in fileNamesToCheck) {
  inew <- gsub("Maize", "maize", i)
  file.rename(from = i, to = inew)
}

modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") 
modelChoices <- c( "IPSL-CM6A-LR") 
library(terra)
woptList <- list(gdal=c("COMPRESS = DEFLATE", "PREDICTOR = 3", "ZLEVEL = 6"))

f_croppingtaxminNmax <- function() {
  extent_NH <- c( -180, 180, 0, 90)
  extent_SH <-c( -180, 180, -60, 0) #-60 gets rid of Antarctica
  hemispheres <- c("NH", "SH")
  
  yearRange <- 19
  for (modelChoice in modelChoices) {
    # for (k in sspChoices){
    #   for (l in startYearChoices){
    #     yearSpan <- paste0(l, "_", l + yearRange)
    #     fileName_in_tmax <- paste0("data/bigFiles/", modelChoice, "_", k, "_", "tasmax", "_global_daily_", yearSpan, ".tif") # needed for heat stress
    #     fileName_in_tmin <- paste0("data/bigFiles/", modelChoice, "_", k, "_", "tasmin", "_global_daily_", yearSpan, ".tif")# needed for cold stress
    #     for (hem in hemispheres) {
    #       fileName_out_hem_tmax <- paste0("data/bigFiles/", tolower(modelChoice), "_", k, "_", "tasmax", "_", hem, "_global_daily_", yearSpan, ".tif") # needed for heat stress
    #        = _out_hem_tmin <- paste0("data/bigFiles/", tolower(modelChoice), "_", k, "_", "tasmin", "_", hem, "_global_daily_", yearSpan, ".tif") # needed for heat stress
    #       rastIn_tasmax <- rast(fileName_in_tmax)
    #       rastIn_tasmin <- rast(fileName_in_tmin)
    #       print("crop tasmax")
    #       print(system.time(rastIn_tasmax_hem <- crop(rastIn_tasmax, get(paste0("extent_", hem)), filename = fileName_out_hem_tmax, overwrite = TRUE, wopt = woptList)))
    #       print(paste0("filename out: ", fileName_out_hem_tmax))
    #       print("crop tasmin")
    #       print(system.time(rastIn_tasmin_hem <- crop(rastIn_tasmin, get(paste0("extent_", hem)), filename = fileName_out_hem_tmin, overwrite = TRUE, wopt = woptList)))
    #       print(paste0("filename out: ", fileName_out_hem_tmin))
    #     }
    #   }
    # }
    k <- "historical"
    l <- 1991
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_in_tmax <- paste0("data/bigFiles/", modelChoice, "_", k, "_", "tasmax", "_global_daily_", yearSpan, ".tif") # needed for heat stress
    fileName_in_tmin <- paste0("data/bigFiles/", modelChoice, "_", k, "_", "tasmin", "_global_daily_", yearSpan, ".tif")# needed for cold stress
    for (hem in hemispheres) {
      fileName_out_hem_tmax <- paste0("data/bigFiles/", tolower(modelChoice), "_", k, "_", "tasmax", "_", hem, "_global_daily_", yearSpan, ".tif") # needed for heat stress
      fileName_out_hem_tmin <- paste0("data/bigFiles/", tolower(modelChoice), "_", k, "_", "tasmin", "_", hem, "_global_daily_", yearSpan, ".tif") # needed for heat stress
      rastIn_tasmax <- rast(fileName_in_tmax)
      rastIn_tasmin <- rast(fileName_in_tmin)
      print("crop tasmax")
      print(system.time(rastIn_tasmax_hem <- crop(rastIn_tasmax, get(paste0("extent_", hem)), filename = fileName_out_hem_tmax, overwrite = TRUE, wopt = woptList)))
      print(paste0("filename out: ", fileName_out_hem_tmax))
      print("crop tasmin")
      print(system.time(rastIn_tasmin_hem <- crop(rastIn_tasmin, get(paste0("extent_", hem)), filename = fileName_out_hem_tmin, overwrite = TRUE, wopt = woptList)))
      print(paste0("filename out: ", fileName_out_hem_tmin))
    }
  }
}
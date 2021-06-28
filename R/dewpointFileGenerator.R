# create dewpoint files
library(terra)
library(Rcpp)
sourceCpp("R/cpp/dewpoint.cpp")
source("R/ISIMIPconstants.R")

#test data
l <- 2041
k <- "ssp585"
yearRange <- 19
yearSpan <- paste0(l, "_", l + yearRange)
modelChoice <- "GFDL-ESM4"
modelChoice_lower <- tolower(modelChoice)
climFiles  <- list.files(locOfClimFiles)
climFiles <- climFiles[!grepl("aux.xml", climFiles, fixed = TRUE)]
climFiles <- paste0("climData/", climFiles)

for (k in sspChoices) {
  for (l in startYearChoices) {
    for (modelChoice in modelChoices) {
      fileName_out <- paste0(locOfClimFiles,  modelChoice_lower, "_dwp_", k, "_", yearSpan, ".tif")
      if (!fileName_out %in% climFiles) {
      fileName_tas <- paste0(locOfClimFiles,  modelChoice_lower, "_tas_", k, "_", yearSpan, ".tif")
      fileName_hurs <-  paste0(locOfClimFiles,  modelChoice_lower, "_hurs_", k, "_", yearSpan, ".tif")
      tas <- rast(fileName_tas)
      hurs <- rast(fileName_hurs)
      
      cellVector_dewpoint <- sds(hurs, tas)
      print(system.time(dewp <- lapp(cellVector_dewpoint, f_tDewp)))
      fileName_out <- paste0(locOfClimFiles,  modelChoice_lower, "_dwp_", k, "_", yearSpan, ".tif")
      print(system.time(writeRaster(dewp, filename = fileName_out,  overwrite = TRUE,  wopt= woptList)))
      print(paste0("dewpoint file name: ", fileName_out))
      }
    }
  }
}

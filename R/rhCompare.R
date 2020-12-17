library(terra)

sourceDir <- "data/aveRHfiles/test-rh/"
  fileList_in <- list.files(sourceDir)
  for (i in fileList_in) {
    rh <- rast(paste0(sourceDir, i), format = "ascii")
    setMinMax(rh)
    print(rh)
  }
  

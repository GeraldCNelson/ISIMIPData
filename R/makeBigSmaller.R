# reduce file sizes of big files by rounding to 3 places after decimal.
library(terra)
locOfFiles <- "data/bigFiles/"
woptList <- list(gdal=c("COMPRESS=LZW"))
terraOptions( progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # memfrac = 2.0, 
bigfilesIn <- list.files(locOfFiles)
bigfilesIn <- bigfilesIn[!grepl("aux.xml", bigfilesIn, fixed = TRUE)]

smallfilesIn <- list.files(paste0(locOfFiles, "small"))
smallfilesIn <- smallfilesIn[!grepl("aux.xml", smallfilesIn, fixed = TRUE)]
bigfilesIn <- bigfilesIn[!bigfilesIn %in% smallfilesIn]
bigfilesIn <- bigfilesIn[!bigfilesIn %in% c("small", "unused")]

bigToSmall <- function(rIn) {
  rOut <- round(rIn, 3)
}

for (i in bigfilesIn) {
  r = rast(paste0(locOfFiles, i))
  fileOut <- paste0(locOfFiles, "small/", i)
  print(paste0("file out: ", fileOut))
  system.time(writeRaster(bigToSmall(r), fileOut, overwrite = TRUE, format = "GTiff", wopt = woptList))
}


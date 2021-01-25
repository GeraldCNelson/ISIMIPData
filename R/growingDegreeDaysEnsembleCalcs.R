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
j <- 1

readRast <- function(m, yearSpan) {
  fileName_in <- paste0("data/cmip6/growingDegreeDays/", m , "_", cropChoices[j], "_", k,  "_gdd_global_daily_", yearSpan, ".tif")
  r <- rast(fileName_in)
  r
}

for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l, ", pid number: ", Sys.getpid()))
    
    for (j in 1:length(cropChoices)) {
      cropName <- cropChoices[j]
      print(paste0("crop names: ", cropName, ", start year: ", l,  ", pid number: ", Sys.getpid()))
      x <- lapply(modelChoices.lower, readRast, yearSpan)
      startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- format(indices, format = "%j")
      indices <- as.numeric(indices)
      print(paste0( "Done setting raster indices for ras.test stack for crop: ", cropName, ", start year: ", l))
      x.mean <- tapp(x, indices, fun = mean, na.rm = TRUE)
      x.cv <- tapp(x, indices, fun = cv, na.rm = TRUE)
      names(ras.test.mean) <- month.abb
      names(ras.test.cv) <- month.abb
      print(paste0( "Done updating raster names with month.abb for crop: ", cropName, ", start year: ", l))
      fileNameMean <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleMean_", cropName,  "_",  yearSpan, "_", k, ".tif") 
      fileNameCV <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleCV_", cropName,  "_",  yearSpan, "_", k, ".tif")
      writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      writeRaster(ras.test.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      print(paste("fileNameMeanOut: ", fileNameMean))
      print(paste0( "Done writing out files for crop: ", cropName, ", start year: ", l, ", pid number: ", Sys.getpid()))
    }
    #    unlink(tmpDirName, recursive = TRUE)
    gc(reset = FALSE, full = TRUE) 
  }
}

# stopCluster(cl)



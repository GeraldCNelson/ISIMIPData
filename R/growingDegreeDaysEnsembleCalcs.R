# combine 10 year rasters across models to get ensemble means and coefficients of variation
source("R/globallyUsed.R")

startYearChoices <-  c( 2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 19
sspChoices <- c("ssp126", "ssp585") 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)

# commented out, now in the globallyUsed.R script
#cropCharacteristics_annual <- as.data.table(read_excel("data-raw/crops/cropCharacteristics_annual_summary_02052020.xlsx", range = "A1:S26"))
#setnames(cropCharacteristics_annual, old = names(cropCharacteristics_annual), new = make.names(names(cropCharacteristics_annual)))
cropChoices <- unique(cropCharacteristics_annual$crop)
#cropChoices <- c("Barley")
#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
j <- 1

f_readRast <- function(m, yearSpan) {
  fileName_in <- paste0("data/cmip6/growingDegreeDays/", m , "_", cropChoices[j], "_", k,  "_gdd_global_daily_", yearSpan, ".tif")
  r <- rast(fileName_in)
  r
}

f_ensembleCalcs <- function(k, l, j) {
  cropName <- cropChoices[j]
  print(paste0("crop names: ", cropName, ", start year: ", l,  ", pid number: ", Sys.getpid()))
  x <- lapply(modelChoices.lower, f_readRast, yearSpan)
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
  fileName_out_mean <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleMean_", cropName,  "_",  yearSpan, "_", k, ".tif") 
  fileName_out_cv <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleCV_", cropName,  "_",  yearSpan, "_", k, ".tif")
  writeRaster(ras.test.mean, filename = fileName_out_mean,  overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
  writeRaster(ras.test.cv, filename = fileName_out_cv,  overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
  print(paste("fileName out mean: ", fileName_out_mean))
  print(paste("fileName out cv: ", fileName_out_cv))
  print(paste0( "Done writing out files for crop: ", cropName, ", start year: ", l))
}

# ensemble calcs, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l))
    for (j in 1:length(cropChoices)) {
      f_ensembleCalcs(k, l, j) 
    }
  }
}

# ensemble calcs, historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
print(paste0("ssp choice: ", k, ", start year: ", l))
for (j in 1:length(cropChoices)) {
  f_ensembleCalcs(k, l, j) 
}




# combine 10 year rasters across models to get ensemble means and coefficients of the heat index by month

source("R/globallyUsed.R")

startyearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

yearRange <- 9
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)


#test values
k <- "ssp585"
l <- 2051


# function to read in the results from all the models
readRast <- function(m) {
  fileNameIn <-    paste0("data/cmip6/heatIndex/", m, "_heatIndexSimple_", "global_daily_", yearSpan, ".tif")
  r <- rast(fileNameIn)
  r
}

# x <- foreach(l = startyearChoices_ensemble, .combine = rbind) %:%
#   foreach(k = sspChoices, .combine = rbind)  %dopar% {
# require(data.table)
for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l, ", pid number: ", Sys.getpid()))
    x <- lapply(modelChoices.lower, readRast)
    r <- rast(x)
    
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices <- format(indices, format = "%j") # this gets the monthly averages
    indices <- as.numeric(indices)
    print(system.time(r.mean <- tapp(r, indices, fun = mean, na.rm = TRUE)))
    print(system.time( r.cv <- tapp(r, indices, fun = cv, na.rm = TRUE)))
    # names(r.mean) <- month.abb
    # names(r.cv) <- month.abb
     fileNameMean <- paste0("data/cmip6/heatIndex/HI_ensembleMean_", yearSpan, "_", k, ".tif") 
    fileNameCV <- paste0("data/cmip6//heatIndex/HI_ensembleCV_", yearSpan, "_", k, ".tif") 
    writeRaster(r.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
    writeRaster(r.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
    print(paste("fileNameMeanOut: ", fileNameMean))
  }
  gc(reset = FALSE, full = TRUE) 
}


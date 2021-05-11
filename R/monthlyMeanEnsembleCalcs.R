# combine 10 year rasters across models to get ensemble means and coefficients of variation; 20 years is below

source("R/globallyUsed.R")
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping constructThese 
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

startYearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data
climateVars <- c( "tasmax", "tasmin", "pr", "hurs",  "rsds", "sfcwind") 
climateVars <- c(  "rsds") 

yearRange <- 9
sspChoices <- c("ssp126","ssp585") 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
k <- "ssp585"
l <- 2051
j <- "hurs"

# varList <- c("modelChoices", "thiList",  "startYearChoices_ensemble", "sspChoices", "tmpDirName")
# libList <- c("rast", "data.table")

# UseCores <- detectCores() - 1 # max number of cores
# useCores <- 3 # better for memory intensive activities
# cl <- clusterSetup(varList, libList, useCores = useCores)

# x <- foreach(l = startYearChoices_ensemble, .combine = rbind) %:%
#   foreach(k = sspChoices, .combine = rbind)  %dopar% {
readRast <- function(m) {
  fileName_in <- paste0("data/cmip6/monthlyMean/monthlyMean_", j, "_", m, "_", k,  "_", yearSpan, ".tif")
  r <- rast(fileName_in)
  names(r) <- month.abb
  r
}

for (k in sspChoices) {
  for (l in startYearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    
    for (j in climateVars) {
      print(paste0("scenario: ", k, ", climate var.: ", j, ", start year: ", l,  ", pid number: ", Sys.getpid()))
      
      # make a list of SpatRasters
      x <- lapply(modelChoices.lower, readRast)
      r <- rast(x)
      system.time(r.mean <- tapp(r, 1:12, fun = mean))
      names(r.mean) <- month.abb
      print(r.mean)
      if (j %in% c("tasmin", "tasmax", "tave")) {
        rKel <- r + 273.15 # convert temp to Kelvin for cv calc on temps
        system.time(r.cv <- tapp(rKel, 1:12, fun = raster::cv))
      }
      if (j %in% "pr") {
       rPrecip <- r + 100 # add 10 for cv calc on rain
        system.time(r.cv <- tapp(rPrecip, 1:12, fun = raster::cv))
      }
      if (j %in% "hurs") {
        system.time(r.cv <- tapp(r, 1:12, fun = raster::cv))
      }
      
      print(r.cv)
      
      # system.time(r.sd <- tapp(r, 1:12, fun = sd))
      # print(r.sd)
       names(r.mean) <- names(r.cv) <-month.abb
      
      # print(paste0( "Done setting doing raster indices for rasterList stack for species: ", speciesName, ", start year: ", l))
      # rasterList.mean <- tapp(rasterList, index, fun = mean)
      # rasterList.cv <- tapp(rasterList, index, fun = raster::cv) #, na.rm = TRUE)
      # names(rasterList.mean) <- month.abb
      # names(rasterList.cv) <- month.abb
      # print(paste0( "Done updating raster names with month.abb for species: ", speciesName, ", start year: ", l))
      fileNameMean <- paste0("data/cmip6/monthlyMean/ensembleMonthlyMean_", j,  "_",  yearSpan, "_", k, ".tif") 
      fileNameCV <-   paste0("data/cmip6/monthlyMean/ensembleMonthlyCV_", j,  "_",  yearSpan, "_", k, ".tif")
      writeRaster(r.mean, filename = fileNameMean,  overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      writeRaster(r.cv, filename = fileNameCV,  overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      print(paste("fileNameMeanOut: ", fileNameMean))
      print(paste0( "Done writing out files for variable: ", j, ", start year: ", l, ", pid number: ", Sys.getpid()))
    }
    #    unlink(tmpDirName, recursive = TRUE)
    gc(reset = FALSE, full = TRUE) 
  }
}

# stopCluster(cl)

# 20 year monthly means

source("R/globallyUsed.R")
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping constructThese 
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

startYearChoices <-  c(2081) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices_ensemble <-  c(2081) # no multimodel results for observed data
climateVars <- c("sfcwind")# "tasmax", "tasmin") #,  "hurs", "rsds", "sfcwind") #, "tave") "pr",
#climateVars <- c("rsds") 

yearRange <- 19
sspChoices <- c("ssp585") 
modelChoices <- c( "MPI-ESM1-2-HR") #, "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
k <- "ssp585"
l <- 2081
j <- "tasmax"

# varList <- c("modelChoices", "thiList",  "startYearChoices_ensemble", "sspChoices", "tmpDirName")
# libList <- c("rast", "data.table")

# UseCores <- detectCores() - 1 # max number of cores
# useCores <- 3 # better for memory intensive activities
# cl <- clusterSetup(varList, libList, useCores = useCores)

# x <- foreach(l = startYearChoices_ensemble, .combine = rbind) %:%
#   foreach(k = sspChoices, .combine = rbind)  %dopar% {
readRast <- function(m) {
  fileName_in <- paste0("data/cmip6/monthlyMean/monthlyMean_", j, "_", m, "_", k,  "_", yearSpan, ".tif")
  r <- rast(fileName_in)
  names(r) <- month.abb
  r
}

for (k in sspChoices) {
  for (l in startYearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    
    for (j in climateVars) {
      print(paste0("scenario: ", k, ", climate var.: ", j, ", start year: ", l,  ", pid number: ", Sys.getpid()))
      
      # make a list of SpatRasters
      x <- lapply(modelChoices.lower, readRast)
      r <- rast(x)
      system.time(r.mean <- tapp(r, 1:12, fun = mean))
      names(r.mean) <- month.abb
      print(r.mean)
      if (j %in% c("tasmin", "tasmax", "tave")) {
        rKel <- r + 273.15 # convert temp to Kelvin for cv calc on temps
        system.time(r.cv <- tapp(rKel, 1:12, fun = raster::cv))
      }
      if (j %in% "pr") {
        rPrecip <- r + 100 # add 10 for cv calc on rain
        system.time(r.cv <- tapp(rPrecip, 1:12, fun = raster::cv))
      }
      if (j %in% "hurs") {
        system.time(r.cv <- tapp(r, 1:12, fun = raster::cv))
      }
      
      print(r.cv)
      
      # system.time(r.sd <- tapp(r, 1:12, fun = sd))
      # print(r.sd)
      names(r.mean) <- names(r.cv) <-month.abb
      
      # print(paste0( "Done setting doing raster indices for rasterList stack for species: ", speciesName, ", start year: ", l))
      # rasterList.mean <- tapp(rasterList, index, fun = mean)
      # rasterList.cv <- tapp(rasterList, index, fun = raster::cv) #, na.rm = TRUE)
      # names(rasterList.mean) <- month.abb
      # names(rasterList.cv) <- month.abb
      # print(paste0( "Done updating raster names with month.abb for species: ", speciesName, ", start year: ", l))
      fileNameMean <- paste0("data/cmip6/monthlyMean/ensembleMonthlyMean_", j,  "_",  yearSpan, "_", k, ".tif") 
      fileNameCV <-   paste0("data/cmip6/monthlyMean/ensembleMonthlyCV_", j,  "_",  yearSpan, "_", k, ".tif")
      writeRaster(r.mean, filename = fileNameMean,  overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      writeRaster(r.cv, filename = fileNameCV,  overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      print(paste("fileNameMeanOut: ", fileNameMean))
      print(paste0( "Done writing out files for variable: ", j, ", start year: ", l, ", pid number: ", Sys.getpid()))
    }
    #    unlink(tmpDirName, recursive = TRUE)
    gc(reset = FALSE, full = TRUE) 
  }
}


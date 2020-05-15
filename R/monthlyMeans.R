# climate data monthly mean and coeffients of variation
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
library(foreach) #Provides foreach looping construct
library(stringr)

sspChoices <- c("ssp585") #"ssp126", "ssp585"
modelChoices <- c("GFDL-ESM4", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
#modelChoices <- c( "MRI-ESM2-0")
variableChoices <- c( "hurs", "tasmax", "tasmin", "pr") # "tasmin", tasmax
startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
locOfFiles <- locOfCMIP6ncFiles
yearRange <- 9

# test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2021
j <- "hurs"

# info on managing raster disk use - https://stackoverflow.com/questions/25426405/raster-package-taking-all-hard-drive

#Sys.setenv(PROJ_LIB = "/usr/local/Cellar/proj/6.2.1/share/proj") # use until the sf and gdal issues get sorted out. See https://github.com/r-spatial/sf/issues/1060

useCores <- detectCores() - 1 # max number of cores
useCores <- 3 # better for memory intensive activities

varList <- c("startyearChoices", "sspChoices", "modelChoices", "wrld_land",  "locOfFiles")
libList <- c("raster", "ncdf4", "stringr")

cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R

foreach(k = sspChoices) %:%
  foreach(i = modelChoices) %:%
  foreach(l = startyearChoices) %:%
  foreach(j = variableChoices) %dopar% {
    require(raster)
    print(paste0("working on start year: ", l, " variable: ", j, " ssp choice: ", k, " model: ", " pid: ", Sys.getpid(), " systime: ", Sys.time()))
    tmpDirName <- paste0(locOfFiles, "rasterTmp_", Sys.getpid(), "/")
    rasterOptions(tmpdir = tmpDirName)
    dir.create(tmpDirName)
    
    modelName.lower <- tolower(i)
    startTime <-  Sys.time()
    yearSpan <- paste0(l, "_", l + yearRange)
    
    # fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    # fileNameIn <- paste0(fileNameIn, ".nc")
    #        fileNameIn <- paste(spatialCoverageChoices, modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    
    #        fileNameIn <- paste0(fileNameIn, ".RDS")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    temp <- paste(locOfFiles, k, "/", i, "/", fileNameIn, sep = "")
    print(paste0("Working on : ", temp))
    ncin.brick <- brick(temp, varname = j) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
    ncin.brick <- readAll(ncin.brick) # seems to speed up processing if ncin.brick is an nc file
    indices <- format(as.Date(names(ncin.brick), format = "X%Y.%m.%d"), format = "%m")
    indices <- as.numeric(indices)
    # ncin.brick <- fixUnits(var = j, ncin.brick = ncin.brick) # fixes temp and precip units; assumes ncin.brick values are raw units
    ncin.brick.mean <- raster::stackApply(ncin.brick, indices, fun = mean, na.rm = TRUE)
    ncin.brick.cv <- raster::stackApply(ncin.brick, indices, fun = cv, na.rm = TRUE)
    names(ncin.brick.mean) <- month.abb
    names(ncin.brick.cv) <- month.abb
    
    fileNameOut_monthMean <- paste0("monthMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
    fileNameOut_monthCV <- paste0("monthCV_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
    
    writeRaster(ncin.brick.mean, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthMean), format = "GTiff", overwrite = TRUE)
    writeRaster(ncin.brick.cv, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthCV), format = "GTiff", overwrite = TRUE)
    print(paste("time to do one brick: ", difftime(Sys.time(), startTime, units = "mins")))
    unlink(tmpDirName, recursive = TRUE)
    rm(ncin.brick.mean)
    rm(ncin.brick.cv)
    gc(TRUE)
  }
stopCluster(cl)

# do same calculations on observed data
tasmax <- tasmax.observed
tasmin <- tasmin.observed
pr <- pr.observed
hurs <- hurs.observed
observedlist <- c("hurs", "tasmax", "tasmin", "pr")

for (j in observedlist) {
  filenameIn <- get(j)
  print(paste0("Working on : ", filenameIn))
  
  #in the hurs file, the variable name is rhs
  ncin.brick <- brick(filenameIn) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
  ncin.brick <- readAll(ncin.brick) # seems to speed up processing if ncin.brick is an nc file
  # ncin.brick <- fixUnits(var = j, ncin.brick = ncin.brick) # fixes temp and precip units; assumes ncin.brick values are raw units
  
  indices <- format(as.Date(names(ncin.brick), format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)
  ncin.brick.mean <- raster::stackApply(ncin.brick, indices, fun = mean, na.rm = TRUE)
  ncin.brick.cv <- raster::stackApply(ncin.brick, indices, fun = cv, na.rm = TRUE)
  names(ncin.brick.mean) <- month.abb
  names(ncin.brick.cv) <- month.abb
  
  yearSpan <- "2001_2010"
  
  fileNameOut_monthMean <- paste0("monthMean_", j, "_", "observed_", yearSpan, ".tif")
  fileNameOut_monthCV <- paste0("monthCV_", j, "_", "observed_", yearSpan, ".tif")
  
  writeRaster(ncin.brick.mean, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthMean), format = "GTiff", overwrite = TRUE)
  writeRaster(ncin.brick.cv, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthCV), format = "GTiff", overwrite = TRUE)
  
  gc(TRUE)
}


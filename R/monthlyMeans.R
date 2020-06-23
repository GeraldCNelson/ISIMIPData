# climate data monthly mean and coeffients of variation
source("R/globallyUsed.R")
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
# library(stringr)

sspChoices <- c("ssp585") #"ssp126", "ssp585"
modelChoices <- c("GFDL-ESM4", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
modelChoices <- c( "IPSL-CM6A-LR", "UKESM1-0-LL")
variableChoices <- c( "hurs", "tasmax", "tasmin", "pr") # "tasmin", tasmax
startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
locOfFiles <- locOfCMIP6ncFiles
yearRange <- 9

# test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
j <- "hurs"

# info on managing raster disk use - https://stackoverflow.com/questions/25426405/raster-package-taking-all-hard-drive

#Sys.setenv(PROJ_LIB = "/usr/local/Cellar/proj/6.2.1/share/proj") # use until the sf and gdal issues get sorted out. See https://github.com/r-spatial/sf/issues/1060

# useCores <- detectCores() - 1 # max number of cores
# useCores <- 3 # better for memory intensive activities

# varList <- c("startyearChoices", "sspChoices", "modelChoices", "wrld_land",  "locOfFiles")
# libList <- c("terra", "ncdf4", "stringr")
# 
# cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R

# foreach(k = sspChoices) %:%
#   foreach(i = modelChoices) %:%
#   foreach(l = startyearChoices) %:%
#   foreach(j = variableChoices) %dopar% {

for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      for (j in variableChoices) {
        
        print(paste0("working on start year: ", l, " variable: ", j, " ssp choice: ", k, " model: ", " pid: ", Sys.getpid(), " systime: ", Sys.time()))
        # tmpDirName <- paste0(locOfFiles, "rasterTmp_", Sys.getpid(), "/")
        # rasterOptions(tmpdir = tmpDirName)
        # dir.create(tmpDirName)
        # 
        modelName.lower <- tolower(i)
        #    startTime <-  Sys.time()
        yearSpan <- paste0(l, "_", l + yearRange)
        layerNames <- readRDS(paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))
        
        fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
        fileNameOut_monthMean <- paste0("monthMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        
        #        fileNameIn <- paste0(fileNameIn, ".RDS")
        fileNameIn <- paste0(fileNameIn, ".nc")
        
        temp <- paste(locOfFiles, k, "/", i, "/", fileNameIn, sep = "")
        print(paste0("Working on : ", temp))
        ncin <- rast(temp) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
        names(ncin) <- layerNames
        indices <- layerNames
        indices <- format(as.Date(indices, format = "X%Y.%m.%d"), format = "%m")
        indices <- as.numeric(indices)
        ncin
        system.time(ncin.mean <- tapp(ncin, indices, fun = mean))
        # ncin.cv <- tapp(ncin, indices, fun = cv, na.rm = TRUE)
        names(ncin.mean) <- month.abb
        # names(ncin.cv) <- month.abb
        
        fileNameOut_monthMean <- paste0("monthMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        #        fileNameOut_monthCV <- paste0("monthCV_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        
        writeRaster(ncin.mean, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthMean), format = "GTiff", overwrite = TRUE)
        print(paste0("writing ncin.mean ", fileNameOut_monthMean))
        #  writeRaster(ncin.cv, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthCV), format = "GTiff", overwrite = TRUE)
        print(paste("time to do one brick: ", difftime(Sys.time(), startTime, units = "mins")))
        #    unlink(tmpDirName, recursive = TRUE)
        rm(ncin.mean)
        #   rm(ncin.cv)
      }
    }
  }
}
#stopCluster(cl)

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
  ncin <- rast(filenameIn) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
  # ncin <- readAll(ncin) # seems to speed up processing if ncin is an nc file
  # ncin <- fixUnits(var = j, ncin = ncin) # fixes temp and precip units; assumes ncin values are raw units
  
  indices <- format(as.Date(names(ncin), format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)
  ncin.mean <- tapp(ncin, indices, fun = mean, na.rm = TRUE)
  ncin.cv <- tapp(ncin, indices, fun = cv, na.rm = TRUE)
  names(ncin.mean) <- month.abb
  names(ncin.cv) <- month.abb
  
  yearSpan <- "2001_2010"
  
  fileNameOut_monthMean <- paste0("monthMean_", j, "_", "observed_", yearSpan, ".tif")
  fileNameOut_monthCV <- paste0("monthCV_", j, "_", "observed_", yearSpan, ".tif")
  
  writeRaster(ncin.mean, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthMean), format = "GTiff", overwrite = TRUE)
  writeRaster(ncin.cv, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthCV), format = "GTiff", overwrite = TRUE)
  
  gc(reset = FALSE, full = TRUE)
}


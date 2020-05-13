# analysis of precipitation data

source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
library(foreach) #Provides foreach looping construct
library(stringr)

sspChoices <- c("ssp585") #"ssp126", "ssp585"
modelChoices <- c("GFDL-ESM4", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
#modelChoices <- c( "MRI-ESM2-0")
variableChoices <- c( "pr") # "tasmax", "pr", "hurs") # "tasmin", tasmax
startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
locOfFiles <- locOfCMIP6ncFiles
yearRange <- 9

# info on managing raster disk use - https://stackoverflow.com/questions/25426405/raster-package-taking-all-hard-drive

#Sys.setenv(PROJ_LIB = "/usr/local/Cellar/proj/6.2.1/share/proj") # use until the sf and gdal issues get sorted out. See https://github.com/r-spatial/sf/issues/1060

useCores <- detectCores() - 1 # max number of cores
useCores <- 3 # better for memory intensive activities

varList <- c("startyearChoices", "sspChoices", "modelChoices", "wrld_land",  "locOfFiles")
libList <- c("raster", "ncdf4", "stringr")

cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R

foreach(i = modelChoices) %:%
  foreach(l = startyearChoices) %:%
  foreach(j = variableChoices) %:%
  foreach(k = sspChoices) %dopar% {
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
    
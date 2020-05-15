# animal extreme THI analysis
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
library(foreach) #Provides foreach looping construct
library(stringr)

sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
variableChoices <- c("tasmax", "tasmin", "pr", "tmpDirName") # "tasmax", "pr" "tasmin"
locOfFiles <- locOfCMIP6ncFiles

# startday <- "0101"
# endday <- "1231"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9
thiList <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.swine")
thiList.animalCount <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.chicken", "thi.swine") # these are the only animals with counts by 1/2 degree
largeAnimals <- c("cattle", "sheep", "goat", "swine")
#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
j = "thi.cattle"

breakPoints <- as.data.table(read.csv("data-raw/animals/breakpointslistRaw.csv", stringsAsFactors = FALSE))

foreach(l = startyearChoices) %:%
  foreach(i = modelChoices) %:%
  foreach(j = thiList.animalCount ) %:%
  foreach(k = sspChoices) %dopar% {
    rasterOptions(tmpdir = tmpDirName)
    dir.create(tmpDirName)
    
    modelName.lower <- tolower(i)
    startTime <-  Sys.time()
    yearSpan <- paste0(l, "_", l + yearRange)
    
    speciesName <- gsub("thi.", "", j)
    extremeStress <- breakPoints[species %in% speciesName, extremeStress]
    fileNameIn <- paste0("data/cmip6/THI/", j, "_", i, "_", yearSpan, "_", k, ".tif")
    temp <- readAll(brick(fileNameIn))
    
    temp[temp < extremeStress] <- NA
    
    # raster.animalcount <- raster(paste0("data/animals/raster_", speciesName, ".tif"))
    # if (speciesName %in% "chicken") 
    # {cutoff <- 100000
    # }else{cutoff <- 1000}
    # 
    # raster.animalcount[raster.animalcount < cutoff] <- NA
    
  }
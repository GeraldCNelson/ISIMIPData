# this script reads in the monthly means of tmax, tmin and rh from data/cmip6/monthMean/ calculates the THI values for each animal type. These are 
# written out to files in data/cmip6/THI/
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
library(foreach) #Provides foreach looping construct

locOfFiles <- "data/cmip6/monthMean/"
startyearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "IPSL-CM6A-LR", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL", "GFDL-ESM4") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
#modelChoices <- c(  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"

#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2021

# create data table to hold all the breakpoints
# bpListRaw <- as.data.table(read.csv("data-raw/animals/breakpointslistRaw.csv", stringsAsFactors = FALSE))
# bpListRaw[, (c("max", "model", "period", "rcp")) := as.character(max, model, period, rcp)]
# bpTemp <- copy(bpListRaw)
# bpListFinal <- bpList <- data.table(species = character(), zeroLevel = numeric(), noStress = numeric(), moderateStress = numeric(), 
#                                     extremeStress = numeric(), max = numeric(), model = character(), period = character(), rcp = character())

thiList <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.swine")
varList <- c("modelChoices", "thiList",  "startyearChoices", "sspChoices", "tmpDirName")
libList <- c("rast", "data.table")

UseCores <- detectCores() - 1 # max number of cores
useCores <- 2 # better for memory intensive activities

cl <- clusterSetup(varList, libList, useCores = useCores)
start_time <- Sys.time()
x <- foreach(i = modelChoices, .combine = rbind) %:%
  foreach(l = startyearChoices, .combine = rbind) %:%
  foreach(k = sspChoices, .combine = rbind)  %dopar% {
    require(data.table)
    require(terra)
    
    #    tmpDirName <- paste0(locOfFiles, "/rasterTmp_", Sys.getpid(), "/")
    #    yearRange <- 9
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("model: ", i, " start year: ", l, " ssp: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
    
    modelName.lower <- tolower(i)
    yearSpan <- paste0(l, "_", l + yearRange)
    
    filePrefix.tmax <- "monthMean_tasmax_"
    filePrefix.tmin <- "monthMean_tasmin_"
    filePrefix.rh <- "monthMean_hurs_"
    
    fileSuffix <- paste0("_", k, "_", yearSpan, ".tif")
    fileName.tmax <- paste0(locOfFiles, filePrefix.tmax, modelName.lower, fileSuffix)
    fileName.tmin <- paste0(locOfFiles, filePrefix.tmin, modelName.lower, fileSuffix)
    fileName.rh <- paste0(locOfFiles, filePrefix.rh, modelName.lower, fileSuffix)
    print(fileName.tmax)
    tmax <- rast(fileName.tmax)
    print(fileName.tmin)
    tmin <- rast(fileName.tmin)
    print(fileName.rh)
    rh <- rast(fileName.rh)
    names(tmax) <- names(tmin) <- names(rh) <- month.abb
    # # THI equations
    # mostly from Lallo
    # note: The Dry Bulb temperature, usually referred to as "air temperature", is the air property that is most commonly used. 
    # When people refer to the temperature of the air they are normally referring to the dry bulb temperature.
    # formulas are stored in globallyUsed.R
    
    thi.cattle <- eval(parse(text = formula.thi.cattle))
    thi.sheep <- eval(parse(text = formula.thi.sheep))
    thi.goat <- eval(parse(text = formula.thi.goat))
    thi.broiler <- eval(parse(text = formula.thi.broiler))
    thi.layer <- eval(parse(text = formula.thi.layer))
    thi.chicken <- eval(parse(text = formula.thi.chicken))
    thi.swine <- eval(parse(text = formula.thi.swine))
    thi.yak <- eval(parse(text = formula.thi.yak))
    names(thi.cattle) <- names(thi.sheep) <- names(thi.goat) <- names(thi.yak) <- names(thi.broiler) <- names(thi.broiler) <- names(thi.layer) <- names(thi.swine) <- month.abb
    
    for (m in thiList) {
      fileName <- paste0("data/cmip6/THI/", m, "_", i, "_", yearSpan, "_", m, ".tif")
      print(fileName)
      writeRaster(get(m), filename = fileName, format = "GTiff", overwrite = TRUE)
      
      # # THI breakpoints by species, four are needed - No stress, moderate stress, severe stress, extreme stress, max value in thi is given by the ceiling xxx code. Stored in data-raw//breakpointslistRaw.csv
      # speciesName <- gsub("thi.", "", k)
      # maxVal <- ceiling(max(maxValue(get(k))))
      # temp <- bpTemp[species %in% speciesName, ]
      # temp <- temp[, (c("max", "model", "period", "rcp")) := list(maxVal, i, l, j)]
      # bpList <- rbind(bpList, temp)
      # 
      # bp.cattle <- list("cattle",     0, 74, 78, 83, ceiling(max(maxValue(thi.cattle))), i, l, j)
      # bp.sheep <- list("sheep",       0, 22.2, 23.3, 25.6,  ceiling(max(maxValue(thi.sheep))), i, l, j)
      # bp.goat <- list("goat",         0, 70, 79, 89,  ceiling(max(maxValue(thi.goat))), i, l, j)
      # bp.yak <- list("yak",           0, 52, 57, 62,  ceiling(max(maxValue(thi.yak))), i, l, j)
      # bp.swine <- list("swine",       0, 27.8, 28.8, 29.9,  ceiling(max(maxValue(thi.swine))), i, l, j)
      # bp.layer <- list("layer",       0, 27.8, 28.8, 29.9,  ceiling(max(maxValue(thi.layer))), i, l, j)
      # bp.broiler <- list("broiler",   0, 27.8, 28.8, 29.9,  ceiling(max(maxValue(thi.broiler))), i, l, j)
      # bp.chicken <- list("chicken",   0, 27.8, 28.8, 29.9,  ceiling(max(maxValue(thi.broiler))), i, l, j)
      # 
      # bpList <- rbind(bpList, bp.cattle, bp.sheep, bp.goat, bp.yak, bp.swine, bp.layer, bp.broiler)
    }
    
    gc(reset = FALSE, full = TRUE) 
  }
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

# observed data calcs

# load observed data and calculate THI values
fileName.rh <- paste0(locOfFiles, "monthMean_hurs", "_observed_", "2001_2010.tif")
print(fileName.rh)
fileName.tmin <- paste0(locOfFiles, "monthMean_tasmin", "_observed_", "2001_2010.tif")
print(fileName.tmin)
fileName.tmax <- paste0(locOfFiles, "monthMean_tasmax", "_observed_", "2001_2010.tif")
print(fileName.tmax)
tmax <- rast(rastfileName.tmax)
tmin <- rast(rastfileName.tmin)
rh <- rast(rastfileName.rh)
names(tmax) <- names(tmin) <- names(rh) <- month.abb

thi.cattle <- eval(parse(text = formula.thi.cattle))
thi.sheep <- eval(parse(text = formula.thi.sheep))
thi.goat <- eval(parse(text = formula.thi.goat))
thi.broiler <- eval(parse(text = formula.thi.broiler))
thi.layer <- eval(parse(text = formula.thi.layer))
thi.chicken <- eval(parse(text = formula.thi.chicken))
thi.swine <- eval(parse(text = formula.thi.swine))
thi.yak <- eval(parse(text = formula.thi.yak))
names(thi.cattle) <- names(thi.sheep) <- names(thi.goat) <- names(thi.yak) <- names(thi.broiler) <- names(thi.broiler) <- names(thi.layer) <- names(thi.swine) <- month.abb

for (k in thiList) {
  fileName <- paste0("data/cmip6/THI/", k, "_observed_", "2001_2010.tif")
  print(fileName)
  writeRaster(get(k), filename = fileName, format = "GTiff", overwrite = TRUE)
}


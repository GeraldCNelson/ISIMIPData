# this script reads in the monthly means of tmax, tmin and rh from data/cmip6/monthlyMean/ calculates the THI values for each animal type. These are 
# written out to files in data/cmip6/THI/
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
library(foreach) #Provides foreach looping construct

locOfFiles <- "data/cmip6/monthlyMean/"
startYearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9
sspChoices <- c("ssp126","ssp585") #"ssp126", 
modelChoices <- c( "IPSL-CM6A-LR", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL", "GFDL-ESM4") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
#modelChoices <- c(  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"

#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2021
m <-  "thi.cattle" 

# create data table to hold all the breakpoints
# bpListRaw <- as.data.table(read.csv("data-raw/animals/breakpointslistRaw.csv", stringsAsFactors = FALSE))
# bpListRaw[, (c("max", "model", "period", "rcp")) := as.character(max, model, period, rcp)]
# bpTemp <- copy(bpListRaw)
# bpListFinal <- bpList <- data.table(species = character(), zeroLevel = numeric(), noStress = numeric(), moderateStress = numeric(), 
#                                     extremeStress = numeric(), max = numeric(), model = character(), period = character(), rcp = character())

thiList <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.swine")
varList <- c("modelChoices", "thiList",  "startYearChoices", "sspChoices", "tmpDirName")
libList <- c("terra", "data.table")

UseCores <- detectCores() - 1 # max number of cores
useCores <- 2 # better for memory intensive activities

cl <- clusterSetup(varList, libList, useCores = useCores)
start_time <- Sys.time()
x <- foreach(i = modelChoices, .combine = rbind) %:%
  foreach(l = startYearChoices, .combine = rbind) %:%
  foreach(k = sspChoices, .combine = rbind)  %dopar% {
    # require(data.table)
    # require(terra)
    
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("model: ", i, " start year: ", l, " ssp: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
    
    modelName.lower <- tolower(i)
    yearSpan <- paste0(l, "_", l + yearRange)
    
    filePrefix.tmax <- "monthlyMean_tasmax_"
    filePrefix.tmin <- "monthlyMean_tasmin_"
    filePrefix.rh <- "monthlyMean_hurs_"
    
    fileSuffix <- paste0("_", k, "_", yearSpan, ".tif")
    fileName.tmax <- paste0(locOfFiles, filePrefix.tmax, modelName.lower, fileSuffix)
    fileName.tmin <- paste0(locOfFiles, filePrefix.tmin, modelName.lower, fileSuffix)
    fileName.rh <- paste0(locOfFiles, filePrefix.rh, modelName.lower, fileSuffix)
#    print(fileName.tmax)
    tmax <- rast(fileName.tmax)
    tmin <- rast(fileName.tmin)
    rh <- rast(fileName.rh)
    # # THI equations
    # mostly from Lallo
    # note: The Dry Bulb temperature, usually referred to as "air temperature", is the air property that is most commonly used. 
    # When people refer to the temperature of the air they are normally referring to the dry bulb temperature.
    # formulas are stored in globallyUsed.R
    
    thi.cattle <- eval(parse(text = formula.thi.cattle))
    print("thi.cattle created")
    thi.sheep <- eval(parse(text = formula.thi.sheep))
    thi.goat <- eval(parse(text = formula.thi.goat))
    thi.broiler <- eval(parse(text = formula.thi.broiler))
    thi.layer <- eval(parse(text = formula.thi.layer))
    thi.chicken <- eval(parse(text = formula.thi.chicken))
    thi.swine <- eval(parse(text = formula.thi.swine))
    thi.yak <- eval(parse(text = formula.thi.yak))
#    names(thi.cattle) <- names(thi.sheep) <- names(thi.goat) <- names(thi.yak) <- names(thi.broiler) <- names(thi.broiler) <- names(thi.layer) <- names(thi.swine) <- month.abb
    
    for (m in thiList) {
      fileName <- paste0("data/cmip6/THI/", m, "_", i, "_", yearSpan, "_", k, ".tif")
      print(paste0("fileName out: ", fileName))
      writeRaster(get(m), filename = fileName, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
      
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
    
    gc() 
  }
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

# observed data calcs

# load observed data and calculate THI values
# fileName.rh <- paste0(locOfFiles, "monthlyMean_hurs", "_observed_", "2001_2010.tif")
# print(fileName.rh)
# fileName.tmin <- paste0(locOfFiles, "monthlyMean_tasmin", "_observed_", "2001_2010.tif")
# print(fileName.tmin)
# fileName.tmax <- paste0(locOfFiles, "monthlyMean_tasmax", "_observed_", "2001_2010.tif")
# print(fileName.tmax)
# tmax <- rast(fileName.tmax)
# tmin <- rast(fileName.tmin)
# rh <- rast(fileName.rh)
# names(tmax) <- names(tmin) <- names(rh) <- month.abb
# 
# thi.cattle <- eval(parse(text = formula.thi.cattle))
# thi.sheep <- eval(parse(text = formula.thi.sheep))
# thi.goat <- eval(parse(text = formula.thi.goat))
# thi.broiler <- eval(parse(text = formula.thi.broiler))
# thi.layer <- eval(parse(text = formula.thi.layer))
# thi.chicken <- eval(parse(text = formula.thi.chicken))
# thi.swine <- eval(parse(text = formula.thi.swine))
# thi.yak <- eval(parse(text = formula.thi.yak))
# names(thi.cattle) <- names(thi.sheep) <- names(thi.goat) <- names(thi.yak) <- names(thi.broiler) <- names(thi.broiler) <- names(thi.layer) <- names(thi.swine) <- month.abb
# 
# for (m in thiList) {
#   fileName <- paste0("data/cmip6/THI/", m, "_observed_", "2001_2010.tif")
#   print(fileName)
#   writeRaster(get(m), filename = fileName, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
# }


# do historical instead of 'observed'
locOfFiles <- "data/cmip6/monthlyMean/"
fileName.rh <- paste0(locOfFiles, "ensembleMonthlyMean_hurs", "_historical_", "2001_2010.tif")
print(fileName.rh)
fileName.tmin <- paste0(locOfFiles, "ensembleMonthlyMean_tasmin", "_historical_", "2001_2010.tif")
print(fileName.tmin)
fileName.tmax <- paste0(locOfFiles, "ensembleMonthlyMean_tasmax", "_historical_", "2001_2010.tif")
print(fileName.tmax)
tmax <- rast(fileName.tmax)
tmin <- rast(fileName.tmin)
rh <- rast(fileName.rh)

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
  fileName <- paste0("data/cmip6/THI/", m, "_historical_", "2001_2010.tif")
  print(fileName)
  writeRaster(get(m), filename = fileName, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
}


# crop min and max temp analysis
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
library(foreach) #Provides foreach looping construct
library(stringr)

sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
variableChoices <- c("tasmax", "tasmin", "pr") # "tasmax", "pr" "tasmin"
locOfFiles <- locOfCMIP6ncFiles
locOfFiles <- "data-raw/ISIMIP/cmip6/unitsCorrected"

startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9

# commented out, now in the globallyUsed.R script
#ann_crop_temp_table <- as.data.table(read_excel("data-raw/crops/ann_crop_temp_table_summary_02052020.xlsx", range = "A1:S26"))
#setnames(ann_crop_temp_table, old = names(ann_crop_temp_table), new = make.names(names(ann_crop_temp_table)))

useCores <- detectCores() - 1 # max number of cores
useCores <- 3 # better for memory intensive activities

varList <- c("locOfFiles", "startyearChoices", "sspChoices", "modelChoices", "ann_crop_temp_table", "tmpDirName")
libList <- c("raster", "ncdf4", "stringr")

#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R

foreach(l = startyearChoices) %:%
  foreach(i = modelChoices) %:%
  #  foreach(j = variableChoices) %:%
  foreach(k = sspChoices) %dopar% {
    rasterOptions(tmpdir = tmpDirName)
    dir.create(tmpDirName)
    
    modelName.lower <- tolower(i)
    startTime <-  Sys.time()
    yearSpan <- paste0(l, "_", l + yearRange)
   filename.tmax <- paste0(locOfFiles, "/", k, "/", i, "/", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".nc")
    
    tmax <- brick(filename.tmax, varname = "tasmax") 
    tmax <- readAll(tmax)
    # tmax <- fixUnits(var = "tasmax", ncin.brick = tmax) # fixes temp and precip units; assumes ncin.brick values are raw units
    
    # tmin <- brick(filename.tmin, varname = "tasmin") 
    # tmin <- readAll(tmin)
    ## tmin <- fixUnits(var = "tasmin", ncin.brick = tmin) # fixes temp and precip units; assumes ncin.brick values are raw units
    # 
    indices <- format(as.Date(names(tmax), format = "X%Y.%m.%d"), format = "%m")
    indices <- as.numeric(indices)
    
    for (m in 1:nrow(ann_crop_temp_table)) {
      cropName <- as.character(ann_crop_temp_table[m, "crop"])
      tdamage_mean <- as.numeric(ann_crop_temp_table[m, "tdamage mean"])
      #     tLethal_min <- as.numeric(ann_crop_temp_table[m, "Tlethal_min"])
      
      upperOpt <- as.numeric(ann_crop_temp_table[m, "topt mean"])
      lowerOpt <- as.numeric(ann_crop_temp_table[m, "topt lower"])
      print(paste0(cropName, ", lower optimum: ", lowerOpt, ", upper optimum: ", upperOpt, ", damage temp: ", tdamage_mean, "\n"))
      
      fileNameOut_damage <- paste0("tdamage_mean_", cropName, "_", tdamage_mean, "C_", modelName.lower, "_", k, "_", yearSpan, ".tif")
      fileNameOut_optTemp <- paste0("optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_", modelName.lower, "_", k, "_", yearSpan, ".tif")
      startTime <-  Sys.time()
      numYears <- 10
      monthDamageCount <- raster::stackApply(tmax, indices, fun = function(x, ...){sum(x > tdamage_mean)/numYears}, progress = "text") 
      names(monthDamageCount) <- month.abb
      print(paste0("time to calc monthly damage: ", difftime(Sys.time(), startTime, units = "mins")))
      startTime <-  Sys.time()
      monthOptTempRangeCount <- stackApply(tmax, indices, fun = function(x, ...){sum(x < upperOpt & x >  lowerOpt)/numYears}, progress = "text") 
      names(monthOptTempRangeCount) <- month.abb
      print(paste0("time to calc optimum range: ", difftime(Sys.time(), startTime, units = "mins")))
      print(paste0("damage  file name out: ", fileNameOut_damage))
      print(paste0("optTempRange file name out: ", fileNameOut_optTemp))
      writeRaster(monthDamageCount, filename = paste0("data/cmip6/damageTemp/", fileNameOut_damage), format = "GTiff", overwrite = TRUE)
      writeRaster(monthOptTempRangeCount, filename = paste0("data/cmip6/optTempRange/", fileNameOut_optTemp), format = "GTiff", overwrite = TRUE)
    }
    unlink(tmpDirName, recursive = TRUE)
    gc(TRUE) 
  }
stopCluster(cl)

# do same calculations on observed data
startTime <-  Sys.time()
yearSpan <- "2001_2010"
tmax <- tasmax.observed
tmax <- readAll(brick(tmax))
# tmax <- fixUnits(var = "tasmax", ncin.brick = tmax) # fixes temp and precip units; assumes ncin.brick values are raw units
indices <- format(as.Date(names(tmax), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)

for (m in 1:nrow(ann_crop_temp_table)) {
  cropName <- as.character(ann_crop_temp_table[m, "crop"])
  tdamage_mean <- as.numeric(ann_crop_temp_table[m, "tdamage mean"])

  upperOpt <- as.numeric(ann_crop_temp_table[m, "topt mean"])
  lowerOpt <- as.numeric(ann_crop_temp_table[m, "topt lower"])
  #         print(paste0("years covered:", yearSpan)), 
  print(paste0(cropName, ", lower optimum: ", lowerOpt, ", upper optimum: ", upperOpt, ", damage temp: ", tdamage_mean))
  
  fileNameOut_damage <- paste0("tdamage_mean_", cropName, "_", tdamage_mean, "C", "_observed_", yearSpan, ".tif")
  fileNameOut_optTemp <- paste0("optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_observed_", yearSpan, ".tif")
  startTime <-  Sys.time()
  numYears <- 10
  monthDamageCount <- raster::stackApply(tmax, indices, fun = function(x, ...){sum(x > tdamage_mean)/numYears}, progress = "text") 
  names(monthDamageCount) <- month.abb
  print(paste0("time to calc monthly damage: ", difftime(Sys.time(), startTime, units = "mins")))
  startTime <-  Sys.time()
  monthOptTempRangeCount <- stackApply(tmax, indices, fun = function(x, ...){sum(x < upperOpt & x >  lowerOpt)/numYears}, progress = "text") 
  names(monthOptTempRangeCount) <- month.abb
  #          raster::endCluster()
  print(paste0("time to calc monthly opt temp range: ", difftime(Sys.time(), startTime, units = "mins")))
  print(paste0("damage  file name out: ", fileNameOut_damage))
  print(paste0("optTempRange file name out: ", fileNameOut_optTemp))
  writeRaster(monthDamageCount, filename = paste0("data/cmip6/damageTemp/", fileNameOut_damage), format = "GTiff", overwrite = TRUE)
  writeRaster(monthOptTempRangeCount, filename = paste0("data/cmip6/optTempRange/", fileNameOut_optTemp), format = "GTiff", overwrite = TRUE)
}


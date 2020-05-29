#  Calculate the number of growing degrees per day for specific crops in specific areas. Main function is globallyUsed.R
source("R/globallyUsed.R")
#library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, 
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2021) #, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
hemisphereList <- c("Northern", "Southern")
northerHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-  c( -180, 180, -90, 0)

yearRange <- 9

# commented out, now in the globallyUsed.R script
#ann_crop_temp_table <- as.data.table(read_excel("data-raw/crops/ann_crop_temp_table_summary_02052020.xlsx", range = "A1:S26"))

# cropChoices <- c("Cassava", "Chickpea", "Cotton")
# cropChoices <- c( "Rye"  , "Sorghum" , "Soybean",  "Sugarbeet" , "Sunflower")
#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
m <- "Barley"
# useCores <- detectCores() - 2 # max number of cores
# useCores <- 2 # better for memory intensive activities
# 
# varList <- c("startyearChoices", "sspChoices", "modelChoices", "locOfFiles", "ann_crop_temp_table", "cropChoices")
# libList <- c("raster", "ncdf4", "data.table")

# cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R
# foreach(k = sspChoices)  %:%
#   foreach(l = startyearChoices) %:%
#   foreach(i = modelChoices) %dopar% {

for (k in sspChoices)  {
  for (l in startyearChoices) {
    for (i in modelChoices)  {
      
      print(paste0("start year: ", l, " ssp: ", k,  " model: ", i, " start year: ", l, " ssp choice: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
      tmpDirName <- paste0(locOfFiles, "rasterTmp_", Sys.getpid(), "/")
      
      rasterOptions(tmpdir = tmpDirName)
      dir.create(tmpDirName)
      
      modelName.lower <- tolower(i)
      startTime <-  Sys.time()
      yearSpan <- paste0(l, "_", l + yearRange)
      j <- "tasmax"
      fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
      fileNameIn <- paste0(fileNameIn, ".nc")
      
      temp <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
      print(paste0("Working on : ", temp, " pid: ", Sys.getpid()))
      tmax <- readAll(brick(temp))
      endTime <- Sys.time()
      print(paste0("tmax brick created, ", temp, ", creation time: ", endTime -startTime,  " pid: ", Sys.getpid()))
      
      j <- "tasmin"
      fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
      fileNameIn <- paste0(fileNameIn, ".nc")
      temp <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
      tmin <- readAll(brick(temp))
      endTime <- Sys.time()
      print(paste0("tmin brick created, ", temp, ", creation time: ", endTime -startTime,  ", pid: ", Sys.getpid()))
      
      for (m in cropChoices) {
        Tbase <- ann_crop_temp_table[(crop %in% m), Tbase]
        Tbase_max <- ann_crop_temp_table[(crop %in% m), Tbase_max]
        startTime <-  Sys.time()
        gdd <- f.gdd(tmax = tmax, tmin = tmin, tbase = Tbase, tbase_max = Tbase_max, crop = m)
        endTime <-  Sys.time()
        print(paste0("gdd created, ", "creation time: ", endTime -startTime,  ", pid: ", Sys.getpid()))
        gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"
        fileNameOut <-    paste(modelName.lower, m, k, "gdd", "global_daily", yearSpan, sep = "_")
        print(paste0("gdd file out name: ", gddsfileOutLoc, fileNameOut, ".tif"))
        writeRaster(gdd, filename = paste0(gddsfileOutLoc, fileNameOut, ".tif"), format = "GTiff", overwrite = TRUE)  
      }
      unlink(tmpDirName, recursive = TRUE)
      gc()
      
    }
  }
}
#    stopCluster(cl)


}
stopCluster(cl)

# do same calculations on observed data
tmax <- tasmax.observed
tmin <- tasmin.observed
# tmin <- readAll(brick(tmin))
# tmax <- readAll(brick(tmax))
print("done with readAll tmin and tmax")
# tmin <- fixUnits(var = "tmin", ncin.brick = tmin) # fixes temp and precip units; assumes ncin.brick values are raw units
# tmax <- fixUnits(var = "tmax", ncin.brick = tmin) # fixes temp and precip units; assumes ncin.brick values are raw units
yearSpan <- "2001_2010"

chillHrs <- overlay(tmin, tmax, fun = f.chillhrs)
names(chillHrs) <- names(tmax) # put the date info back into the names

# do several count days in a month
# first days with temp below zero
print("Done with chillHrs function")
indices <- format(as.Date(names(tmin), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
monthZeroCount <- stackApply(tmin, indices, fun = function(x, ...){sum(x <= 0)}) 
names(monthZeroCount) <- month.abb
fileNameOutZero <- paste0("belowZeroCount", "_observed_", yearSpan, ".tif")
writeRaster(monthZeroCount, filename = paste0("data/cmip6/belowZero/", fileNameOutZero), format = "GTiff", overwrite = TRUE)


rm(list = c("tmax", "tmin"))
chillHrs.sumMonth <- stackApply(chillHrs, indices, fun = sum, na.rm = TRUE)
chillHrs.sumMonth <- chillHrs.sumMonth/10 # to get to the monthly average over 10 years
names(chillHrs.sumMonth) <- month.abb
chillHrsNorthernHem <- dropLayer(chillHrs.sumMonth, southernHemWinter) # note dropping layers for southern hemisphere winter
chillHrsSouthernHem <- dropLayer(chillHrs.sumMonth, northernHemWinter) # note dropping layers for northern hemisphere winter
chillHrsNorthernHem <- sum(chillHrsNorthernHem)
chillHrsSouthernHem <- sum(chillHrsSouthernHem)
endCompleteLoop <- Sys.time()

#print(endCompleteLoop - startTime)

fileNameNH <- paste0("chillHrsNorthernHem", "_observed_", yearSpan, ".tif")
fileNameSH <- paste0("chillHrsSouthernHem", "_observed_", yearSpan, ".tif")

writeRaster(chillHrsNorthernHem, filename = paste0("data/cmip6/chillingHours/", fileNameNH), format = "GTiff", overwrite = TRUE)
writeRaster(chillHrsSouthernHem, filename = paste0("data/cmip6/chillingHours/", fileNameSH), format = "GTiff", overwrite = TRUE)

gc(TRUE)

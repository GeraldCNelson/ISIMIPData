#  Calculate the number of growing degrees per day for specific crops in specific areas. Main function is globallyUsed.R
source("R/globallyUsed.R")
#library(doParallel) #Foreach Parallel Adapter 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "UKESM1-0-LL") #"MPI-ESM1-2-HR", "MRI-ESM2-0")# "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, 
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c( 2021, 2051, 2091) #, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
# hemisphereList <- c("Northern", "Southern")
# northerHemExtent <- c( -180, 180, 0, 90)
# southernHemExtent <-  c( -180, 180, -90, 0)
# 
yearRange <- 9
gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"

# commented out, now in the globallyUsed.R script
#ann_crop_temp_table <- as.data.table(read_excel("data-raw/crops/ann_crop_temp_table_summary_02052020.xlsx", range = "A1:S26"))

# cropChoices <- c("Cassava", "Chickpea", "Cotton")
# cropChoices <- c( "Rye"  , "Sorghum" , "Soybean",  "Sugarbeet" , "Sunflower")
#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2051
m <- "Wheat"
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
  for (i in modelChoices)  {
    for (l in startyearChoices) {
      
      print(paste0("start year: ", l, " ssp: ", k,  " model: ", i, " start year: ", l, " ssp choice: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
      #      tmpDirName <- paste0(locOfFiles, "rasterTmp_", Sys.getpid(), "/")
      
      #      rasterOptions(tmpdir =  "data/ISIMIP/") # need to use a relative path
      # dir.create(tmpDirName)
      # 
      modelName.lower <- tolower(i)
      startTime <-  Sys.time()
      yearSpan <- paste0(l, "_", l + yearRange)
      j <- "tasmax"
      fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
      fileNameIn <- paste0(fileNameIn, ".nc")
      
      tmaxIn <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
      #      tmax <- readAll(rasttemp))
      # endTime <- Sys.time()
      # print(paste0("tmax brick created, ", tmaxIn, ", creation time: ",  round(difftime(endTime, startTime, units = "mins"), digits = 2),  " min.,  pid: ", Sys.getpid()))
      
      startTime <-  Sys.time()
      j <- "tasmin"
      fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
      fileNameIn <- paste0(fileNameIn, ".nc")
      tminIn <- paste0(locOfFiles, k,"/", i, "/", fileNameIn)
      #      <- readAll(rasttemp))
      startTime <-  Sys.time()
      tmaxTminIn(tmaxIn, tminIn) # function to read in tmax and tmin with rast
      terra:::.mem_info(tmin, 1) 
      endTime <- Sys.time()
      endTime - startTime    
      print(paste0("tmin and tmax loaded, ", ",  creation time: ",  round(difftime(endTime, startTime, units = "mins"), digits = 2),  " min.,  pid: ", Sys.getpid()))
      gddFilesCompleted <- list.files(gddsfileOutLoc)
      gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]
      print("starting calculation of tavg")
      system.time(tavg <- (tmax + tmin) / 2) # do this calc once for each period and then adjust below to get to gdds
      terra:::.mem_info(tmin, 1) 
      tmin <- tmax <- NULL
      print("mem_info for tavg")
      terra:::.mem_info(tavg, 1) 
      
      for (o in 1:length(cropChoices)) {
        for (m in get(cropChoices[o])) {
          print(paste0("crop: ", m))
          fileNameOut <-    paste(modelName.lower, m, k, "gdd", "global_daily", yearSpan, sep = "_")
          print(paste0("Working on: ", fileNameOut))
          if (!paste0(fileNameOut, ".tif") %in% gddFilesCompleted) {
            tbase <- ann_crop_temp_table[(crop %in% m), Tbase]
            tbase_max <- ann_crop_temp_table[(crop %in% m), Tbase_max]
            fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(m), ".tif")
            cropMask <- rast(fileNameMask.in)
            startTime <-  Sys.time()
            #            system.time(gdd <- setValues(tmin, f.gdd(cropMask = values(cropMask), tmin = values(tmin), tmax = values(tmax), tbase = Tbase, tbase_max = Tbase_max)))
            #          gdd <- f.gdd(cropMask = cropMask, tmin = tmin, tmax = tmax, tbase = tbase, tbase_max = tbase_max)
            #     function(cropMask, tmin, tmax, tbase, tbase_max) 
            #         #system.time(gdd <- overlay(cropMask, tmin, tmax, fun=function(x, y, z) gdd.f1(x, y, z, tb = Tbase, tbm = Tbase_max)))
            terra:::.mem_info(tavg, 1) 
            print(system.time(gdd <- tavg - tbase))
            print("Working on mask")
            system.time(gdd[is.na(cropMask), ] <- NA)
            terra:::.mem_info(tavg, 1) 
            endTime <- Sys.time()
            print(paste0("gdd created, ", "creation time: ", round(difftime(endTime, startTime, units = "mins"), digits = 2),  " min., pid: ", Sys.getpid()))
            print(paste0("gdd file out name: ", gddsfileOutLoc, fileNameOut, ".tif"))
            writeRaster(gdd, filename = paste0(gddsfileOutLoc, fileNameOut, ".tif"), format = "GTiff", overwrite = TRUE)  
            cropMask <- NULL
            gdd <- NULL
            # gc(reset = FALSE, full = TRUE)
            
          }else{
            print(paste("This file has already been created: ", fileNameOut))
          }
          removeTmpFiles(h = 1)
        }
      }
      #      unlink(tmpDirName, recursive = TRUE)
      
    }
  }
}
#    stopCluster(cl)

# do same calculations on observed data
tmax <- tasmax.observed
tmin <- tasmin.observed

yearSpan <- "2001_2010"

for (o in 1:length(cropChoices)) {
  for (m in get(cropChoices[o])) {
    print(paste0("crop: ", m))
    fileNameOut <-    paste("observed", m, "gdd", "global_daily", yearSpan, sep = "_")
    print(paste0("Working on: ", fileNameOut))
    if (!paste0(fileNameOut, ".tif") %in% gddFilesCompleted) {
      Tbase <- ann_crop_temp_table[(crop %in% m), Tbase]
      Tbase_max <- ann_crop_temp_table[(crop %in% m), Tbase_max]
      startTime <-  Sys.time()
      #      gdd <- f.gdd(tmax = tmax, tmin = tmin, tbase = Tbase, tbase_max = Tbase_max, crop = m)
      system.time(gdd <- setValues(tmin, gdd.f3(values(mask), values(tmin), values(tmax), tbase = Tbase, tbase_max = Tbase_max)))
      endTime <-  Sys.time()
      print(paste0("gdd created, ", "creation time: ", round(difftime(endTime, startTime, units = "mins"), digits = 2),  " min., pid: ", Sys.getpid()))
      print(paste0("gdd file out name: ", gddsfileOutLoc, fileNameOut, ".tif"))
      writeRaster(gdd, filename = paste0(gddsfileOutLoc, fileNameOut, ".tif"), format = "GTiff", overwrite = TRUE)  
    }else{
      print(paste("This file has already been created: ", fileNameOut))
    }
    #   gc(reset = FALSE, full = TRUE)
  }
}


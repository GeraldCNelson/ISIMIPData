#  Calculate the number of growing degrees per day for specific crops in specific areas. Main function is globallyUsed.R
source("R/globallyUsed.R")
library(terra)
library(data.table)
#library(doParallel) #Foreach Parallel Adapter 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c(   "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "IPSL-CM6A-LR",

startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9
gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"

# commented out, now in the globallyUsed.R script
library(readxl)
ann_crop_temp_table <- as.data.table(read_excel("data-raw/crops/ann_crop_temp_table_summary_0506052020.xlsx", range = "A1:S26"))
data.table::setnames(ann_crop_temp_table, old = names(ann_crop_temp_table), new = make.names(names(ann_crop_temp_table)))
cropChoice_cereals <- ann_crop_temp_table[ICC.crop.classification %in% "Cereal", crop]

#cropChoices <- c("cropChoice_cereals")
cropChoices <- cropChoice_cereals
#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2051
m <- "Wheat"

for (k in sspChoices)  {
  for (i in modelChoices)  {
    for (l in startyearChoices) {
      
      print(paste0("start year: ", l, " ssp: ", k,  " model: ", i, ", start year: ", l, ", ssp choice: ", k, ", pid: ", Sys.getpid(), " systime: ", Sys.time()))
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      
      gddFilesCompleted <- list.files(gddsfileOutLoc)
      gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]
      
      fileIn.tave <- paste0("data-raw/ISIMIP/cmip6/unitsCorrected/", k, "/", i, "/", modelName.lower, "_", k, "_tave_global_daily_", yearSpan, ".tif")
      tave <- rast(fileIn.tave)
      print(system.time(tave <- tave * 1)); flush.console
      
      print("mem_info for tave")
      terra:::.mem_info(tave, 1) 
      
      #      for (o in 1:length(cropChoices)) {
#      for (m in get(cropChoices[o])) {
      for (m in cropChoices) {
        print(paste("start time: ", Sys.time()))
        
        print(paste0("crop: ", m))
        fileNameOut <-    paste(modelName.lower, m, k, "gdd", "global_daily", yearSpan, sep = "_")
        if (!paste0(fileNameOut, ".tif") %in% gddFilesCompleted) {
          print(paste0("Working on: ", fileNameOut))
          tbase <- ann_crop_temp_table[(crop %in% m), Tbase]
          tbase_max <- ann_crop_temp_table[(crop %in% m), Tbase_max]
          print(system.time(gdd <- app(tave, fun=function(x){ 
            x[x > tbase_max] <- tbase_max
            y <- x - tbase
            y[y < 0] <- 0
            return(y)} ))); flush.console
          print(paste0("gdd file out name: ", gddsfileOutLoc, fileNameOut, ".tif"))
          writeRaster(round(gdd, 1), filename = paste0(gddsfileOutLoc, fileNameOut, ".tif"), format = "GTiff", overwrite = TRUE, wopt=list(gdal=c("COMPRESS=LZW"))  )  
          gdd <- NULL
          gc(reset = FALSE, full = TRUE)
          
        }else{
          print(paste("This file has already been created: ", fileNameOut))
        }
        gc(reset = FALSE, full = TRUE)
      }
    }
  }
}
#}
#    stopCluster(cl)

# do same calculations on observed data
l <- 2001
yearSpan <- paste0(l, "_", l + yearRange)
fileIn.tave <- paste0("data-raw/ISIMIP/cmip6/unitsCorrected/", k, "/", i, "/", modelName.lower, k, "_tave_global_daily_", yearSpan, "tif")
tave <- rast("data-raw/ISIMIP/cmip6/unitsCorrected/ssp585/UKESM1-0-LL/ukesm1-0-ll_ssp585_tave_global_daily_2051_2060.tif")
tave <- tave * 1

print("mem_info for tave")
terra:::.mem_info(tave, 1) 

gddFilesCompleted <- list.files(gddsfileOutLoc)
gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]

for (o in 1:length(cropChoices)) {
  for (m in get(cropChoices[o])) {
    print(paste0("crop: ", m))
    fileNameOut <-    paste("observed", m, "gdd", "global_daily", yearSpan, sep = "_")
    if (!paste0(fileNameOut, ".tif") %in% gddFilesCompleted) {
      print(paste0("Working on: ", fileNameOut))
      print(paste("start time: ", Sys.time()))
      
      Tbase <- ann_crop_temp_table[(crop %in% m), Tbase]
      Tbase_max <- ann_crop_temp_table[(crop %in% m), Tbase_max]
      system.time(gdd <- tave - tbase)
      print(system.time(gdd <- app(tave, fun=function(x){ 
        x[x > tbase_max] <- tbase_max
        y <- x - tbase
        y[y < 0] <- 0
        return(y)} ))); flush.console
      print(paste0("gdd file out name: ", gddsfileOutLoc, fileNameOut, ".tif"))
      writeRaster(round(gdd, 1), filename = paste0(gddsfileOutLoc, fileNameOut, ".tif"), format = "GTiff", overwrite = TRUE, wopt=list(gdal=c("COMPRESS=LZW"))  
      gdd <- NULL
    }else{
      print(paste("This file has already been created: ", fileNameOut))
    }
    gc(reset = FALSE, full = TRUE)
  }
}


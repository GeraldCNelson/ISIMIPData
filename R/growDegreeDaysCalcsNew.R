#  Calculate the number of growing degrees per day for specific crops in specific areas. Main function is globallyUsed.R
#source("R/globallyUsed.R")
library(terra)
library(data.table)
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else {## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
if (get_os() %in% "osx") {
  terraOptions(memfrac = 2,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
  gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"
  
}else{
  terraOptions(progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
  locOfFiles <- "/Volumes/ExtremeSSD3/bigFiles/"
  gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"
}
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") 
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") #
startyearChoices <-  c( 2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
#startyearChoices <-  c(2081) #2011, 2041, 2051, 2081) 
ann_crop_temp_table <- as.data.table(read_excel("data-raw/crops/ann_crop_temp_table_summary_22082020.xlsx", range = "A1:T26"))
data.table::setnames(ann_crop_temp_table, old = names(ann_crop_temp_table), new = make.names(names(ann_crop_temp_table)))
cropChoice_cereals <- ann_crop_temp_table[ICC.crop.classification %in% "Cereal", crop]

yearRange <- 19

library(readxl)

#cropChoices <- c("cropChoice_cereals")
cropChoices <- cropChoice_cereals
#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2041
m <- "Wheat"

f_gdd = function(cellVector, tbase, tbase_max){
  if (is.nan(cellVector[1])) {return(cellVector)}
#  y <- clamp(cellVector, lower = tbase, upper = tbase_max)
  y <- sapply(cellVector, FUN = f_ycalc) # from Toshi email May 3, 2020
  return(y)
}

f_ycalc <- function(cellVector){max(0, min(cellVector, tbase_max)-tbase)}

gddFilesCompleted <- list.files(paste0(getwd(), gddsfileOutLoc),  full.names = TRUE)
gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]

for (k in sspChoices)  {
  for (i in modelChoices)  {
    for (l in startyearChoices) {
      
      print(paste0("start year: ", l, " ssp: ", k,  " model: ", i, ", start year: ", l, ", ssp choice: ", k, ", pid: ", Sys.getpid(), " systime: ", Sys.time()))
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      
      
      fileIn.tas <- paste0(locOfFiles, modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
      tas <- rast(fileIn.tas)
      print("mem_info for tas")
      terra:::.mem_info(tas, 1) 
      
      if (get_os() %in% "osx") system.time(tas <- tas * 1 )
      
      for (m in cropChoices) {
        print(paste0("crop: ", m))
        fileName_out <- paste0(gddsfileOutLoc, modelName.lower, "_", "gdd", "_", tolower(m), "_", k, "_", "global_daily", "_", yearSpan, ".tif")
        if (!fileName_out %in% gddFilesCompleted) {
          print(paste0("Working on: ", fileName_out))
          tbase <- ann_crop_temp_table[crop == m, Tbase]
          tbase_max <- ann_crop_temp_table[crop == m, Tbase_max]
          print(Sys.time())
         print(system.time(gdd <- app(tas, fun=f_gdd, tbase, tbase_max, filename = fileName_out, overwrite=TRUE, wopt = woptList)))
          print(Sys.time())
          print(paste0("gdd file out name: ", fileName_out))
          #         writeRaster(round(gdd, 1), filename = paste0(gddsfileOutLoc, fileName_out, ".tif"), format = "GTiff", overwrite = TRUE, wopt = woptList)
          gdd <- NULL
          gc(reset = FALSE, full = TRUE)
        }else{
          print(paste("This file has already been created: ", fileName_out))
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
fileIn.tas <- paste0("data-raw/ISIMIP/cmip6/unitsCorrected/", k, "/", i, "/", modelName.lower, k, "_tas_global_daily_", yearSpan, "tif")
tas <- rast("data-raw/ISIMIP/cmip6/unitsCorrected/ssp585/UKESM1-0-LL/ukesm1-0-ll_ssp585_tas_global_daily_2051_2060.tif")
tas <- tas * 1

print("mem_info for tas")
terra:::.mem_info(tas, 1) 

gddFilesCompleted <- list.files(gddsfileOutLoc)
gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]

for (o in 1:length(cropChoices)) {
  for (m in get(cropChoices[o])) {
    print(paste0("crop: ", m))
    fileName_out <-    paste("observed", m, "gdd", "global_daily", yearSpan, sep = "_")
    if (!paste0(fileName_out, ".tif") %in% gddFilesCompleted) {
      print(paste0("Working on: ", fileName_out))
      print(paste("start time: ", Sys.time()))
      
      Tbase <- ann_crop_temp_table[(crop %in% m), Tbase]
      Tbase_max <- ann_crop_temp_table[(crop %in% m), Tbase_max]
      system.time(gdd <- tas - tbase)
      print(system.time(gdd <- app(tas, fun=function(x){ 
        x[x > tbase_max] <- tbase_max
        y <- x - tbase
        y[y < 0] <- 0
        return(y)} ))); flush.console
      print(paste0("gdd file out name: ", gddsfileOutLoc, fileName_out, ".tif"))
      writeRaster(round(gdd, 1), filename = paste0(gddsfileOutLoc, fileName_out, ".tif"), format = "GTiff", overwrite = TRUE, wopt=list(gdal=c("COMPRESS=LZW"))  )  
      gdd <- NULL
    }else{
      print(paste("This file has already been created: ", fileName_out))
    }
    gc(reset = FALSE, full = TRUE)
  }
}


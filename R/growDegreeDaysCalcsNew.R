#  Calculate the number of growing degrees per day for specific crops in specific areas. Main function is globallyUsed.R
#source("R/globallyUsed.R")
{library(terra)
  library(data.table)
  library(readxl)
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
    locOfFiles <- "/Volumes/ExtremeSSD3/bigFiles/"
    gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"
    
  }else{
    terraOptions(progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
    locOfFiles <- "data/tas/"
    gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"
  }
  woptList <- list(gdal=c("COMPRESS=LZW"))
  woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))
  
  #   library(Rcpp)
  #   
  #   cppFunction("
  #   NumericVector min_vec(NumericVector vec1, NumericVector vec2) {
  #     int n = vec1.size();
  #     if(n != vec2.size()) return 0;
  #     else {
  #       NumericVector out(n);
  #       for(int i = 0; i < n; i++) {
  #         out[i] = std::min(vec1[i], vec2[i]);
  #       }
  #       return out;
  #     }
  #   }
  # ")
  #   
  #   cppFunction("
  #   NumericVector max_vec(NumericVector vec1, NumericVector vec2) {
  #     int n = vec1.size();
  #     if(n != vec2.size()) return 0;
  #     else {
  #       NumericVector out(n);
  #       for(int i = 0; i < n; i++) {
  #         out[i] = std::max(vec1[i], vec2[i]);
  #       }
  #       return out;
  #     }
  #   }
  # ")
  
  sspChoices <- c("ssp126", "ssp585") 
  sspChoices <- c("ssp126") 
  modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") 
  # modelChoices <- c("UKESM1-0-LL", "IPSL-CM6A-LR") 
  modelChoices.lower <- tolower(modelChoices)
  startYearChoices <-  c( 2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
  #startYearChoices <-  c(2081) #2011, 2041, 2051, 2081) 
  ann_crop_temp_table <- as.data.table(read_excel("data-raw/crops/ann_crop_temp_table_summary_22082020.xlsx", range = "A1:T26"))
  data.table::setnames(ann_crop_temp_table, old = names(ann_crop_temp_table), new = make.names(names(ann_crop_temp_table)))
  cropChoice_cereals <- ann_crop_temp_table[ICC.crop.classification %in% "Cereal", crop]
  
  yearRange <- 19
  
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
  
  # f_gdd = function(cellVector, tbase, tbase_max){
  #   if (is.nan(cellVector[1])) {return(cellVector)}
  #   y <- pmax(0, pmin(cellVector, tbase_max)-tbase)
  #   return(y)
  # }
  gddFilesCompleted <- list.files(gddsfileOutLoc,  full.names = TRUE)
  gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]
  gddFilesCompleted <- gsub("//", "/", gddFilesCompleted)
}

for (k in sspChoices)  {
  for (i in modelChoices)  {
    for (l in startYearChoices) {
      print(paste0("start year: ", l, " ssp: ", k,  " model: ", i, ", start year: ", l, ", ssp choice: ", k, ", pid: ", Sys.getpid(), " systime: ", Sys.time()))
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      fileIn.tas <- paste0("data/bigFiles/", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
            tas <- rast(fileIn.tas)
      print("mem_info for tas")
      terra:::.mem_info(tas, 1) 
      if (get_os() %in% "osx") print(system.time(tas <- tas * 1))
      
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

# historical -----
k <- "historical"
l <- 1991
for (i in modelChoices)  {
  print(paste0("start year: ", l, " ssp: ", k,  " model: ", i, ", start year: ", l, ", ssp choice: ", k, ", pid: ", Sys.getpid(), " systime: ", Sys.time()))
  modelName.lower <- tolower(i)
  yearSpan <- paste0(l, "_", l + yearRange)
  fileIn.tas <- paste0("data/bigFiles/", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
  tas <- rast(fileIn.tas)
  print("mem_info for tas")
  terra:::.mem_info(tas, 1) 
  if (get_os() %in% "osx") print(system.time(tas <- tas * 1))
  
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

# ensemble calcs -----
# do ensemble calcs by hemisphere, 20 years in the northern hemisphere, 19 in the southern hemisphere
#First do NH, then SH below that

#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2041
m <- "Wheat"

hemisphere <- c("NH", "SH")
extent_NH <- c( -180, 180, 0, 90)
extent_SH <-c( -180, 180, -60, 0) #-60 gets rid of Antarctica
# readRast <- function(i) {
#   fileName_in <- paste0(gddsfileOutLoc, i, "_", "gdd", "_", tolower(m), "_", k, "_", "global_daily", "_", yearSpan, ".tif")
#   r <- rast(fileName_in)
#   # names(r) <- indices
#   # r
# }

f_readRast_thi_ensemble <- function(i, m, k) {
  fileName_in <- paste0(gddsfileOutLoc, i, "_", "gdd", "_", tolower(m), "_", k, "_", "global_daily", "_", yearSpan, ".tif")
  print(paste0("m: ", m, ", k: ", k, ", model(i): ", i, ", fileName in: ", fileName_in))
  r <- rast(fileName_in)
  indices <- seq(from = 1, to = nlyr(r), 1)
  indices <- paste0("X", as.character(indices))
  names(r) <- indices
  r
}

# dailyMean <- function() {
#   yearSpan <- paste0(l, "_", l + yearRange)
#   x <- lapply(modelChoices, readRast_climVar_ensemble)
#   r <- rast(x)
#   indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
#   fileName_out <- paste0("data/bigFiles/ensemble_dyMean20yr_", k,  "_", m, "_", yearSpan, ".tif") # note yearSpan here
#   print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
# }

sspChoices <- "ssp126"
for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l))
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
   # indices <- paste0("X", indices)
    indices_day <- format(indices, format = "%j") # days
    indices_day <- as.numeric(indices_day)
    for (m in cropChoices) {
      print(paste0("crop names: ", m, ", start year: ", l))
      x <- lapply(modelChoices.lower, f_readRast_thi_ensemble, m = m, k = k)
      print(x)
#            indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
      r <- rast(x)
      print(r)
      fileName_out <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleMean_daily_", m,  "_",  yearSpan, "_", k, ".tif")
      print(paste0("fileName out: ", fileName_out))
      print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE))) #, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
    }
  }
}

k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
print(paste0("ssp choice: ", k, ", start year: ", l))
startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
indices <- seq(as.Date(startDate), as.Date(endDate), 1)
# indices <- paste0("X", indices)
indices_day <- format(indices, format = "%j") # days
indices_day <- as.numeric(indices_day)
for (m in cropChoices) {
  print(paste0("crop names: ", m, ", start year: ", l))
  x <- lapply(modelChoices.lower, f_readRast_thi_ensemble, m = m, k = k)
  print(x)
  #            indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
  r <- rast(x)
  print(r)
  fileName_out <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleMean_daily_", m,  "_",  yearSpan, "_", k, ".tif")
  print(paste0("fileName out: ", fileName_out))
  print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE))) #, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
}


names(r) <- indices

x_ <- crop(x, get(paste0("extent_", m)))

print(paste0( "Done setting raster indices for ras.test stack for crop: ", m, ", start year: ", l))
x.mean <- tapp(x, indices, fun = mean, na.rm = TRUE)
names(ras.test.mean) <- month.abb
print(paste0( "Done updating raster names with month.abb for crop: ", m, ", start year: ", l))

writeRaster(ras.test.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))
print(paste("fileNameMeanOut: ", fileNameMean))
}
  #    unlink(tmpDirName, recursive = TRUE)
  gc(reset = FALSE, full = TRUE) 
  }
  }
  
  # check names
  for (i in gddFilesCompleted) {
    temp <- rast(i)
    print(paste0("file name in: ", i))
    print(head(names(temp)))
  }
  
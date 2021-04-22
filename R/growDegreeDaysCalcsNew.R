#  Calculate the number of growing degrees per day for specific crops in specific areas. 
# use hemisphere and climate year data
source("R/globallyUsed.R")
{
  library(data.table)
  library(readxl)
  
  if (get_os() %in% "osx") {
    terraOptions(memfrac = 2,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) #ncopies = 1, 
    locOfFiles <- "data/bigFiles/"
    gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"
  }else{
    terraOptions(progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
    locOfFiles <- "data/tas/"
    gddsfileOutLoc <- "data/cmip6/growingDegreeDays/"
  }
  woptList <- list(gdal=c("COMPRESS=LZW"))
  woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))
  # woptList <- list(gdal=c("COMPRESS=ZSTD", "PREDICTOR=3", "ZSTD_LEVEL=6"))
  
  #file locations -----
  runsLoc <- "data/cmip6/runs/"
  rasterMaskLoc <- "data/crops/rasterMask_"
  cropCalFilesLoc <- paste0("data-raw/crops/cropCalendars/ggcmiCropCalendars/")
  gddDaysFilesLoc <- "data/cmip6/growingDegreeDays/"
  
  
  sspChoices <- c("ssp126", "ssp585") 
  #  sspChoices <- c("ssp126") 
  modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") 
   modelChoices <- c("MRI-ESM2-0") 
  modelChoices.lower <- tolower(modelChoices)
  startYearChoices <-  c( 2041, 2081) 
  #startYearChoices <-  c(2081) 
  # source of crop temperature values
  cropCharacteristics_annual <- as.data.table(read_excel("data-raw/crops/GCB_annual_crops_temp_requirements_11032021.xlsx", range = "A1:J21"))
  
  data.table::setnames(cropCharacteristics_annual, old = names(cropCharacteristics_annual), new = make.names(names(cropCharacteristics_annual)))
  cropChoice_cereals <- cropCharacteristics_annual[Crop_type %in% "Cereal", Crop]
  hemispheres <- c("NH", "SH")
  extent_NH <- c( -180, 180, 0, 90)
  extent_SH <-c( -180, 180, -60, 0) #-60 gets rid of Antarctica
  yearRange <- 19
  
  cropChoices <- cropChoice_cereals
  cropChoices <- c("Maize")
  #test values
  i <- "UKESM1-0-LL"
  k <- "ssp585"
  l <- 2041
  m <- "Maize"
  hem <- "NH"
  
  f_gdd = function(cellVector, Topt_min, Topt_max){
    if (is.nan(cellVector[1])) {return(cellVector)}
    y <- sapply(cellVector, FUN = f_ycalc, Topt_max, Topt_min) # from Toshi email May 3, 2020
    return(y)
  }
  
  # f_ycalc <- function(cellVector, tbase_max, tbase){max(0, min(cellVector, tbase_max)-tbase)}
  f_ycalc <- function(cellVector, Topt_max, Topt_min){
    ycalc <- max(0, min(cellVector, Topt_max)-Topt_min)
    #   print(paste0("ycalc: ", ycalc))
    return(ycalc)
  }
  
  f_computeGDDs <- function(k, i, l, hem, cropCharacteristics_annual) {
    print(paste0("start year: ", l, ", ssp: ", k,  " model: ", i, ", start year: ", l, ", hemisphere: ", hem))
    modelName.lower <- tolower(i)
    #    if (hem == "SH") yearRange <- 18
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_in <- paste0("data/cmip6/runs/", modelName.lower, "_", "tas", "_", hem, "_", "summer", "_", k, "_", yearSpan, ".tif")
    tas <- rast(fileName_in)
    print(tas)
    #   print("mem_info for tas")
    #   terra:::.mem_info(tas, 1) 
    #   if (get_os() %in% "osx") print(system.time(tas <- tas * 1)) takes 40+ seconds to do this
    
    for (cropName in cropChoices) {
      cropName_lower <- tolower(cropName)
      fileName_out <- paste0(gddsfileOutLoc, modelName.lower, "_", "gdd", "_", hem, "_", cropName_lower, "_", k, "_", yearSpan, ".tif")
      if (!fileName_out %in% gddFilesCompleted) {
        #      print(paste0("Working on: ", fileName_out))
        Topt_min <- cropCharacteristics_annual[Crop == cropName, Topt_min]
        Topt_max <- cropCharacteristics_annual[Crop == cropName, Topt_max]
        print(paste0("crop: ", cropName, " Topt_min: ", Topt_min, " Topt_max: ", Topt_max, " fileName_out: ", fileName_out))
        print(system.time(gdd <- app(tas, fun=f_gdd, Topt_min, Topt_max, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
        print(paste0("gdd file out name: ", fileName_out))
        return(gdd)
      }else{
        print(paste("This file has already been created: ", fileName_out))
      }
    }
  }
  
  gddFilesCompleted <- list.files(gddsfileOutLoc,  full.names = TRUE)
  gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]
  gddFilesCompleted <- gsub("//", "/", gddFilesCompleted)
}

# calc gdds, scenarios -----
for (k in sspChoices) {
  #    k = "ssp126"
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (i in modelChoices) {
      for (hem in hemispheres) {
        gdds <- f_computeGDDs(k, i, l, hem, cropCharacteristics_annual)
      }
    }
  }
}

# calc gdds, historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (i in modelChoices) {
  for (hem in hemispheres) {
    f_computeGDDs(k, i, l, hem, cropCharacteristics_annual)
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

# readRast <- function(i) {
#   fileName_in <- paste0(gddsfileOutLoc, i, "_", "gdd", "_", tolower(m), "_", k, "_", yearSpan, ".tif")
#   r <- rast(fileName_in)
#   # names(r) <- indices
#   # r
# }

f_readRast_thi_ensemble <- function(i, cropName, k) {
  fileName_in <- paste0(gddsfileOutLoc, i, "_", "gdd", "_", tolower(m), "_", k, "_", yearSpan, ".tif")
  print(paste0("cropName: ", cropName, ", k: ", k, ", model(i): ", i, ", fileName in: ", fileName_in))
  r <- rast(fileName_in)
  indices <- seq(from = 1, to = nlyr(r), 1)
  indices <- paste0("X", as.character(indices))
  names(r) <- indices
  r
}

for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l))
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    # indices <- paste0("X", indices)
    indices_day <- format(indices, format = "%j") # days
    indices_day <- as.numeric(indices_day)
    for (cropName in cropChoices) {
      print(paste0("crop names: ", cropName, ", start year: ", l))
      x <- lapply(modelChoices.lower, f_readRast_thi_ensemble, m = m, k = k)
      print(x)
      r <- rast(x)
      print(r)
      fileName_out <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleMean_daily_", cropName,  "_",  yearSpan, "_", k, ".tif")
      print(paste0("fileName out: ", fileName_out))
      print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
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
for (cropName in cropChoices) {
  print(paste0("crop names: ", cropName, ", start year: ", l))
  x <- lapply(modelChoices.lower, f_readRast_thi_ensemble, cropName = cropName, k = k)
  print(x)
  r <- rast(x)
  print(r)
  fileName_out <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleMean_daily_", m,  "_",  yearSpan, "_", k, ".tif")
  print(paste0("fileName out: ", fileName_out))
  print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
}




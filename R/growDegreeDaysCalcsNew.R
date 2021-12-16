#  Calculate the number of growing degrees per day for specific crops in specific areas. 
# use hemisphere and climate year data
#source("R/globallyUsed.R")
{
  source("R/ISIMIPconstants.R")
  source("R/ISIMIPspatialConstants.R")
  library(data.table)
  library(readxl)
  options(warn = 1)
  require(Rcpp)
  sourceCpp("R/cpp/gdd.cpp")
  
  #file locations -----
  runsLoc <- "data/cmip6/runs/"
  rasterMaskLoc <- "data/crops/rasterMask_"
  locOfgddsFiles <- "data/cmip6/growingDegreeDays/"
  locOfClimFiles <- "/Volumes/ExtremeSSD2/ISIMIP/cmip6/"
  
  cropCharacteristics_annual <- as.data.table(read_excel("data-raw/crops/GCB_annual_crops_temp_requirements_11032021.xlsx", range = "A1:J21"))
  setnames(cropCharacteristics_annual, old = names(cropCharacteristics_annual), new = make.names(names(cropCharacteristics_annual)))
  lowerCases <- c("Crop_type", "Crop", "Species", "Topt_min", "Topt", "Topt_max",  "Tmax", "Tcrit")
  setnames(cropCharacteristics_annual, old = lowerCases, new = tolower(lowerCases))
  cropCharacteristics_annual[, crop_type := tolower(crop_type)]
  cropCharacteristics_annual[, crop := tolower(crop)]
  cropChoice_cereals <- cropCharacteristics_annual[crop_type %in% "cereal", crop]
  
  cropChoices <- tolower(cropChoice_cereals)
  cropChoice <- c("maize")
  
  #test values
  modelChoice <- "UKESM1-0-LL"
  k <- "ssp585"
  l <- 2041
  hem <- "NH"
  
  # # replaced with gdd.cpp
  #  f_gdd = function(cellVector, Topt_min, Topt_max){
  #    if (is.nan(cellVector[1])) {return(cellVector)}
  #    y <- sapply(cellVector, FUN = f_ycalc, Topt_max, Topt_min) # from Toshi email May 3, 2020
  #    return(y)
  #  }
  #  
  #  # replaced with gdd.cpp
  #  f_ycalc <- function(cellVector, topt_max, topt_min){
  #    ycalc <- max(0, min(cellVector, topt_max)-topt_min)
  #    #   print(paste0("ycalc: ", ycalc))
  #    return(ycalc)
  #  }
  
  f_computeGDDs <- function(k, modelChoice, l, hem, cropCharacteristics_annual, cropChoice) {
    print(paste0("start year: ", l, ", ssp: ", k,  " model: ", modelChoice, ", start year: ", l, ", hemisphere: ", hem))
    modelChoice_lower <- tolower(modelChoice)
    #    if (hem == "SH") yearRange <- 18
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_in <- paste0(locOfClimFiles, modelChoice_lower, "_", "tas", "_", k, "_", yearSpan, ".tif")
    tas <- rast(fileName_in)
    print(tas)
    
    #   for (cropChoice in cropChoices) {
    fileName_out <- paste0(locOfgddsFiles, modelChoice_lower, "_", "gdd", "_", cropChoice, "_", k, "_", yearSpan, ".tif")
    if (!fileName_out %in% gddFilesCompleted) {
      #      print(paste0("Working on: ", fileName_out))
      topt_min <- cropCharacteristics_annual[crop == cropChoice, topt_min]
      topt_max <- cropCharacteristics_annual[crop == cropChoice, topt_max]
      print(paste0("crop: ", cropChoice, " topt_min: ", topt_min, " topt_max: ", topt_max, " fileName_out: ", fileName_out))
      print(system.time(gdd <- app(tas, fun = f_gdd, topt_min, topt_max, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
      print(paste0("gdd file out name: ", fileName_out))
      return(gdd)
    }else{
      print(paste("This file has already been created: ", fileName_out))
    }
  }
}


gddFilesCompleted <- list.files(locOfgddsFiles,  full.names = TRUE)
gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]
gddFilesCompleted <- gsub("//", "/", gddFilesCompleted)

# calc gdds, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (modelChoice in modelChoices) {
      for (hem in hemispheres) {
        gdds <- f_computeGDDs(k, modelChoice, l, hem, cropCharacteristics_annual, cropChoice)
      }
    }
  }
}

# calc gdds, historical -----
k <- "historical"
l <- 1991
for (i in modelChoices) {
  for (hem in hemispheres) {
    gdds <- f_computeGDDs(k, modelChoice, l, hem, cropCharacteristics_annual, cropChoice)
  }
}

# ensemble calcs -----
# do ensemble calcs by hemisphere, 20 years in the northern hemisphere, 19 in the southern hemisphere
#First do NH, then SH below that

# readRast <- function(i) {
#   fileName_in <- paste0(locOfgddsFiles, i, "_", "gdd", "_", tolower(m), "_", k, "_", yearSpan, ".tif")
#   r <- rast(fileName_in)
#   # names(r) <- indices
#   # r
# }

f_readRast_thi_ensemble <- function(modelChoice, cropChoice, k, l) {
  yearSpan <- paste0(l, "_", l + yearRange)
  modelChoice_lower <- tolower(modelChoice)
  fileName_in <- paste0(locOfgddsFiles, modelChoice, "_", "gdd", "_", modelChoice_lower, "_", k, "_", yearSpan, ".tif")
  print(paste0("cropChoice: ", cropChoice, ", k: ", k, ", model(i): ", modelChoice, ", fileName in: ", fileName_in))
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
    for (cropChoice in cropChoices) {
      print(paste0("crop names: ", cropChoice, ", start year: ", l))
      x <- lapply(modelChoices.lower, f_readRast_thi_ensemble, cropChoice, k, l)
      print(x)
      r <- rast(x)
      print(r)
      fileName_out <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleMean_daily_", cropChoice,  "_",  yearSpan, "_", k, ".tif")
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
for (cropChoice in cropChoices) {
  print(paste0("crop names: ", cropChoice, ", start year: ", l))
  x <- lapply(modelChoices.lower, f_readRast_thi_ensemble, cropChoice, k, l)
  print(x)
  r <- rast(x)
  print(r)
  fileName_out <- paste0("data/cmip6/growingDegreeDays/GDD_ensembleMean_daily_", cropChoice,  "_",  yearSpan, "_", k, ".tif")
  print(paste0("fileName out: ", fileName_out))
  print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
}




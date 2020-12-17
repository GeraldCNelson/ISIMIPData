# calculate ag worker climate stress
source("R/globallyUsed.R")

locOfFiles <- locOfCMIP6tifFiles

sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
modelChoices <- c("UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
#startyearChoices <-  c(2041) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_historical <- c(1991)
scenarioChoicesEnsemble <- c("historical", sspChoices)
northernHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-c( -180, 180, -90, 0)
hemisphere <- c("NH", "SH")

yearRange <- 19
yearRangeSH <- 18 # one less year because of 6 month offset

woptList <- list(gdal=c("COMPRESS=LZW"))

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2041
yearNumber <- 2043

readRast_hi <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/humanStress/hi_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  r <- rast(fileNameIn)
}

=100/(1+(((-12.28*LN($A)+87.99)/B$1))^(-2.2*LN($A3)+2.63))

readRast_HI_Ensemble <- function(i) {
  fileNameIn <- paste0("data/cmip6/humanStress/hi_", m, "_", i, "_", k,  "_", yearSpan, ".tif") # note yearSpan here
  print(fileNameIn)
  
  
  r <- rast(fileNameIn)
  startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
  indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  indices <- paste0("X", as.character(indices))
  names(r) <- indices
  r
}

# HI index
#HI =  - 42.379 + 2.04901523 * RH - 10.14333127 * RH - 0.22475541 * T * RH - 0.00683783 * T^2 - 0.05481717 * RH^2 + 0.00122874 * T^2 * RH + 0.00085282 * T * RH^2 - 0.00000199 * T^2 * RH^2
# HI = -42.379 + RH*12.192346501014333 - 0.22475541 * T * RH - 0.00683783 * T^2 - 0.05481717 * RH^2 + 0.00122874 * T^2 * RH + 0.00085282 * T * RH^2 - 0.00000199 * T^2 * RH^2
hifun <- function(rh, tmin) {
  hi <- -42.379 + rh * -8.094316 - 0.22475541 * tmin * rh - 0.00683783 * tmin^2 - 0.05481717 * rh^2 + 0.00122874 * tmin^2 * rh + 0.00085282 * tmin * rh^2 - 0.00000199 * tmin^2 * rh^2
}

# HI scenarios ----- 
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange + 1) # the +1 here is to get at the original file names
      modelName.lower <- tolower(i)
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      tmin <- rast(fileName_tasmin)
      fileName_rh <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      rh <- rast(fileName_rh)
      
      for (yearNumber in l:(l + yearRange)) {
        gc()
        
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        
        print(system.time(tmin_yr <- subset(tmin, indicesCharYear)))
        #        names(tmax_yr) <- indicesCharYear
        # system.time(tmax_NH <- crop(tmax, ext(northernHemExtent)))
        # system.time(tmax_yr_NH <- subset(tmax_NH, indicesCharYear))
        
        print(system.time(rh_yr <- subset(rh, indicesCharYear)))
        #      names(rh_yr) <- indicesCharYear
        # system.time(rh_NH <- crop(rh, ext(northernHemExtent)))
        # system.time(rh_yr_NH <- subset(rh_NH, indicesCharYear))
        
        comb <- sds(rh_yr, tmin_yr)
        fileName_out <- paste0("data/cmip6/humanStress/hi_",  i, "_", k, "_", yearNumber, ".tif")
        print(system.time(rrr <- lapp(comb, hifun, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
      }
    }
  }
}

# HI historical ------
k <- "historical"
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    yearSpan <- paste0(l, "_", l + yearRange + 1) # the +1 here is to get at the original file names
    modelName.lower <- tolower(i)
    modelName.lower <- tolower(i)
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
    tmin <- rast(fileName_tasmin)
    fileName_rh <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
    rh <- rast(fileName_rh)
    for (yearNumber in l:(l + yearRange)) {
      gc()
      startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      
      print(system.time(tmin_yr <- subset(tmin, indicesCharYear)))
      print(system.time(rh_yr <- subset(rh, indicesCharYear)))
      
      comb <- sds(rh_yr, tmin_yr)
      fileName_out <- paste0("data/cmip6/humanStress/hi_",  i, "_", k, "_", yearNumber, ".tif")
      print(system.time(rrr <- lapp(comb, hifun, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
    }
  }
}

# 20 year HI results  ------
for (k in sspChoices) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    for (l in startyearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      x <- lapply(yearnumberRange, readRast_hi)
      x_start <- x
      r <- rast(x)
      fileNameOut <- paste0("data/cmip6/humanStress/hi_", i, "_", k,  "_", yearSpan, ".tif")
      print(paste0("fileNameOut: ", fileNameOut))
      print(system.time(writeRaster(r, fileNameOut, overwrite=TRUE, wopt = woptList))); flush.console()
      r_start <- r_end <- NULL
      gc()
    }
  }
}

# 20 year HI results, historical  ------
k <- "historical"
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    yearSpan <- paste0(l, "_", l + yearRange)
    gc()
    x <- lapply(yearnumberRange, readRast_hi)
    r <- rast(x)
    fileNameOut <- paste0("data/cmip6/humanStress/hi_", i, "_", k,  "_", yearSpan, ".tif")
    print(paste0("fileNameOut: ", fileNameOut))
    print(system.time(writeRaster(r, fileNameOut, overwrite=TRUE, wopt = woptList))); flush.console()
    r_start <- r_end <- NULL
    gc()
  }
}

# ensemble results
for (k in scenarioChoicesEnsemble) {
  for (startYear in startyearChoices) {
    l <- startYear
    yearSpan <- paste0(l, "_", l + yearRange)
    yearnumberRange <- seq(l, (l + yearRange), 1)
    if (k %in% "historical") {yearSpan <- "1991_2010"; l <- 1991}
    gc()
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices <- paste0("X", as.character(indices))
    indices_day <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
    indices_day <- as.numeric(indices_day)
 
      x <- lapply(modelChoices, readRast_HI_Ensemble)
      r <- rast(x)
      print(r)
#      fileName_out <- paste0("data/cmip6/humanStress/ensemble_HI_", k, "_", yearSpan, ".tif")
      fileName_out <- paste0("data/cmip6/humanStress/ensemble_hi_", m, "_", k, "_", yearSpan, ".tif")
      print(paste0("fileName out: ", fileName_out))
      # r.mean <- tapp(r, indices_day, fun = mean, na.rm = TRUE)
 #     x.cv <- tapp(x, indices_day, fun = cv, na.rm = TRUE)
      
      system.time(r.mean <- tapp(r, indices_day, fun = mean, na.rm = TRUE, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      title <- paste0("Ensemble mean, HI index ", m, "," , k, ", period ", yearSpan, ", Jan 1.")
      plot(r.mean, 1, main = title))
  }
}

# days with max stress
library(data.table)
funextTHI <- function(cellVector) {
  extremeCt <- c(NA)
  if (is.nan(cellVector[1])) {
    return(extremeCt)
  }
  extremeCt <- sum(cellVector > extremeStress)
  return(extremeCt)
}

extremeStress <- 100
for (k in scenarioChoicesEnsemble) {
  for (startYear in startyearChoices) {
    l <- startYear
    yearSpan <- paste0(l, "_", l + yearRange)
    yearnumberRange <- seq(l, (l + yearRange), 1)
    if (k %in% "historical") {yearSpan <- "1991_2010"; l <- 1991}
    
    
    fileName_in <- paste0("data/cmip6/humanStress/ensemble_hi", m, "_", k, "_", yearSpan, ".tif")
    r <- rast(fileName_in)
    print(system.time(extremeCt <- app(r, funextTHI)))
    fileName_out <- paste0("data/cmip6/humanStress/extemeCt.", m, "_", k, "_", yearSpan, ".tif")
    plot(extremeCt, main = paste0("Days above extreme stress value of ", extremeStress, ", ensemble means, ", k, ", ", yearSpan))
    
  }
}

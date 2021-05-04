# Extreme Heat
# 
# For the periods:
#   
#   Northern Hemisphere 1 July – 15 August
# Southern Hemisphere 1 January – 15 February
# Count the total number of days that Tmax >=35oC with the window above. This is the result (for now). Calculate this for each individual year, GCM, ssp. We then need to do some additional analyses.

# 
# Spring Frost
# For the periods:
#   Northern Hemisphere 1 March – 30 April
# Southern Hemisphere 15 August – 15 October
# Calculate the total number of days that Tmin <=0oC within the window above. This is the result (for now). Calculate this for each individual year, GCM, ssp. We then need to do some additional analyses.

# cold damage
# When tmin < -30C

terraOptions(memfrac = 4, progress = 0, tempdir =  "data/ISIMIP", verbose = TRUE)
locOfFiles <- "data/bigFiles/"

sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
modelChoices <- c("UKESM1-0-LL", "GFDL-ESM4") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startYearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices <-  c(2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices_historical <- c(1991)
scenarioChoicesEnsemble <- c("historical", sspChoices)
northernHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-c( -180, 180, -90, 0)
hemisphere <- c("NH", "SH")

yearRange <- 19
yearRangeSH <- 18 # one less year because of 6 month offset
minimumGrwSeasonLength = 100
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2041
yearNumber <- 2043

# set of functions for the perennial crops limits

# readRastHeat, readRastFrost, readRastHeatEnsemble, and readRastFrostEnsemble are used to assemble sets of spatRasters
readRastHeat <- function(yearNumber) {
  fileName_in <- paste0("data/cmip6/extremeHtCt/extremeHtCt", m, "_", i, "_", k,  "_", yearNumber, ".tif")
  print(yearNumber)
  print(fileName_in)
  r <- rast(fileName_in)
}
readRastFrost <- function(yearNumber) {
  fileName_in <- paste0("data/cmip6/frostCt/frostCt", m, "_", i, "_", k,  "_", yearNumber, ".tif")
  r <- rast(fileName_in)
}

readRastExtremeCld <- function(yearNumber) {
  fileName_in <- paste0("data/cmip6/extremeCldCt/extremeCldCt_", i, "_", k,  "_", yearNumber, ".tif")
  r <- rast(fileName_in)
}
readRastHeatEnsemble <- function(i) {
  fileName_in <- paste0("data/cmip6/extremeHtCt/extremeHtCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
  print(fileName_in)
  r <- rast(fileName_in)
}
readRastFrostEnsemble <- function(i) {
  fileName_in <- paste0("data/cmip6/frostCt/frostCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
  print(fileName_in)
  r <- rast(fileName_in)
}

readRastExtremeCldEnsemble <- function(i) {
  fileName_in <- paste0("data/cmip6/extremeCldCt/extremeCldCt_", i, "_", k,  "_", yearSpan, ".tif")
  print(fileName_in)
  r <- rast(fileName_in)
}

# f_Frost, funExtremeHeat, and funColdKill return counts of the number of days in the cellVector spatRaster that meet the criterion
f_Frost <- function(cellVector) {
  frostCt <- c(NA) 
  if (is.nan(cellVector[1])) {
    return(frostCt)
  }
  frostCt <- sum(cellVector < 0)
  return(frostCt) 
}
funExtremeHeat <- function(cellVector) {
  heatCt <- c(NA) 
  if (is.nan(cellVector[1])) {
    return(heatCt)
  }
  heatCt <- sum(cellVector > 35)
  return(heatCt) 
}
f_ExtremeCld <- function(cellVector) {
  ExtremeCldCt <- c(NA) 
  if (is.nan(cellVector[1])) {
    return(ExtremeCldCt)
  }
  ExtremeCldCt <- sum(cellVector < -35)
  return(ExtremeCldCt) 
}

# f_Crop crops a climate variable to an extent
f_Crop <- function(cvar, extent) {
  if (cvar %in% c("tmax", "tasmax")) cvar <- "tasmax"
  if (cvar %in% c("tmin", "tasmin")) cvar <- "tasmin"
  fileName_cvar <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_", cvar, "_global_daily_", yearSpan, ".tif")
  r_cvar <- rast(fileName_cvar)
  if (extent %in% "NH") extent_local <- ext(northernHemExtent)
  if (extent %in% "SH") extent_local <- ext(southernHemExtent)
  varRegion <- crop(r_cvar, extent_local)
}

# Northern hemisphere -----
# NH heat -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startYearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      system.time(NH <- f_Crop("tmax", "NH"))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber, "-08-15")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmax_yr_NH <- subset(NH, indicesCharYear))
        print(system.time(extremeHtCt <- app(tmax_yr_NH, funExtremeHeat)))
        fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCtNH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, wopt = woptList)
      }
    }
  }
}

# 20 year heat temps, spatRasters by 20 year windows, by ESM and by ssp -----
for (k in sspChoices) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    for (hem in hemisphere)
      for (l in startYearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        print(hem)
        yearnumberRange <- seq(l, (l + yearRange), 1)
        gc()
        x <- lapply(yearnumberRange, readRastHeat)
        r <- rast(x)
        fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
        print(system.time(writeRaster(r, fileName_out, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
      }
  }
}

# 20 year heat temps, historical -----
k <- "historical"
for (i in modelChoices) {
  modelName.lower <- tolower(i)
  for (hem in hemisphere)
    for (l in startYearChoices_historical) {
      yearSpan <- paste0(l, "_", l + yearRange)
      print(hem)
      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      x <- lapply(yearnumberRange, readRastHeat)
      r <- rast(x)
      r
      fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
      print(system.time(writeRaster(r, fileName_out, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
    }
}

# NH frost -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startYearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      tmin <- rast(fileName_tasmin)
      system.time(NH <- crop(tmin, ext(northernHemExtent)))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-03-01"); endDate <- paste0(yearNumber, "-04-30")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmin_yr_NH <- subset(NH, indicesCharYear))
        print(system.time(frostCt <- app(tmin_yr_NH, f_Frost)))
        fileName_out <- paste0("data/cmip6/frostCt/frostCtNH_",  i, "_", k, "_", yearNumber, ".tif")
        frostCt
        writeRaster(frostCt, fileName_out, overwrite = TRUE, wopt = woptList)
      }
    }
  }
}

# NH heat historical -----
k <- "historical"
for (i in modelChoices) {
  for (l in startYearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    system.time(NH <- f_Crop("tmax", "NH"))
    NH
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber, "-08-15")
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmax_yr_NH <- subset(NH, indicesCharYear))
      print(system.time(extremeHtCt <- app(tmax_yr_NH, funExtremeHeat)))
      print(extremeHtCt)
      fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCtNH_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, wopt = woptList)
      #      extremeHtCt <- NULL
      gc()
    }
  }
}

for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startYearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      system.time(NH <- f_Crop("tmax", "NH"))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber, "-08-15")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmax_yr_NH <- subset(NH, indicesCharYear))
        print(system.time(extremeHtCt <- app(tmax_yr_NH, funExtremeHeat)))
        fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCtNH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, wopt = woptList)
      }
    }
  }
}

# NH frost historical -----
k <- "historical"
for (i in modelChoices) {
  for (l in startYearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    system.time(NH <- f_Crop("tmin", "NH"))
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-03-01"); endDate <- paste0(yearNumber, "-04-30")
      # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      # indicesChar <- paste0("X", as.character(indices))
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmin_yr_NH <- subset(NH, indicesCharYear))
      print(system.time(frostCt <- app(tmin_yr_NH, f_Frost)))
      fileName_out <- paste0("data/cmip6/frostCt/frostCtNH_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(frostCt, fileName_out, overwrite = TRUE, wopt = woptList)
      #      frostCt <- NULL
      gc()
    }
  }
}

# 20 year frost temps, both NH and SH -----

for (k in sspChoices) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    for (hem in hemisphere)
      for (l in startYearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        print(hem)
        yearRange <- 19
         yearnumberRange <- seq(l, (l + yearRange), 1)
        gc()
        x <- lapply(yearnumberRange, readRastFrost)
        r <- rast(x)
        fileName_out <- paste0("data/cmip6/frostCt/frostCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
        print(system.time(writeRaster(r, fileName_out, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
      }
  }
}

# 20 year frost temps, historical, both NH and SH -----
k <- "historical"
for (i in modelChoices) {
  modelName.lower <- tolower(i)
  for (hem in hemisphere)
    for (l in startYearChoices_historical) {
      yearSpan <- paste0(l, "_", l + yearRange)
      print(hem)
      yearRange <- 19
      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      x <- lapply(yearnumberRange, readRastFrost)
      print(x)
      r <- rast(x)
      fileName_out <- paste0("data/cmip6/frostCt/frostCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
      print(system.time(writeRaster(r, fileName_out, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
    }
}

# Southern hemisphere -----
# SH heat -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startYearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      system.time(SH <- f_Crop("tmax", "SH"))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-02-15")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmax_yr_SH <- subset(SH, indicesCharYear))
        print(system.time(extremeHtCt <- app(tmax_yr_SH, funExtremeHeat)))
        print(paste0("extremeHtCt: ", summary(extremeHtCt))); flush.console()
        fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCtSH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, wopt = woptList)
      }
    }
  }
}

# SH frost -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startYearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      system.time(SH <- f_Crop("tmin", "SH"))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-03-01"); endDate <- paste0(yearNumber, "-04-30")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmin_yr_SH <- subset(SH, indicesCharYear))
        
        print(system.time(frostCt <- app(tmin_yr_SH, f_Frost)))
        fileName_out <- paste0("data/cmip6/frostCt/frostCtSH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(frostCt, fileName_out, overwrite = TRUE, wopt = woptList)
      }
    }
  }
}

# SH heat historical -----
k = "historical"
for (i in modelChoices) {
  for (l in startYearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    system.time(SH <- f_Crop("tmax", "SH"))
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-02-15")
      # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      # indicesChar <- paste0("X", as.character(indices))
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmax_yr_SH <- subset(SH, indicesCharYear))
      print(system.time(extremeHtCt <- app(tmax_yr_SH, funExtremeHeat)))
      print(paste0("extremeHtCt: ", summary(extremeHtCt))); flush.console()
      fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCtSH_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, wopt = woptList)
    }
  }
}

# SH frost historical -----
for (i in modelChoices) {
  for (l in startYearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    system.time(SH <- f_Crop("tmin", "SH"))
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-03-01"); endDate <- paste0(yearNumber, "-04-30")
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmin_yr_SH <- subset(SH, indicesCharYear))
      
      # then use the function like this
      print(system.time(frostCt <- app(tmin_yr_SH, f_Frost)))
      print(frostCt)
      fileName_out <- paste0("data/cmip6/frostCt/frostCtSH_",  i, "_", k, "_", yearNumber, ".tif")
      print(paste0("frost historical outfile: ", fileName_out))
      writeRaster(frostCt, fileName_out, overwrite = TRUE, wopt = woptList)
    }
  }
}

# extreme cold calcs -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startYearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      fileName_tmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_", "tasmin", "_global_daily_", yearSpan, ".tif")
      tmin <- rast(fileName_tmin)
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(extremeCld_yr <- subset(tmin, indicesCharYear))
        
        print(system.time(extremeCldCt <- app(extremeCld_yr, f_ExtremeCld)))
        fileName_out <- paste0("data/cmip6/extremeCldCt/extremeCldCt_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(extremeCldCt, fileName_out, overwrite = TRUE, wopt = woptList)
      }
    }
  }
}

# extreme cold historical -----
k <- "historical"
for (i in modelChoices) {
  for (l in startYearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    fileName_tmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_", "tasmin", "_global_daily_", yearSpan, ".tif")
    
    tmin <- rast(fileName_tmin)
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(extremeCld_yr <- subset(tmin, indicesCharYear))
      print(system.time(extremeCldCt <- app(extremeCld_yr, f_ExtremeCld)))
      print(extremeCldCt)
      fileName_out <- paste0("data/cmip6/extremeCldCt/extremeCldCt_",  i, "_", k, "_", yearNumber, ".tif")
      print(paste0("fileName out: ", fileName_out))
      writeRaster(extremeCldCt, fileName_out, overwrite = TRUE, wopt = woptList)
    }
  }
}

# 20 year extreme cold -----
for (k in sspChoices) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      yearRange <- 19
      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      x <- lapply(yearnumberRange, readRastExtremeCld)
      r <- rast(x)
      fileName_out <- paste0("data/cmip6/extremeCldCt/extremeCldCt_", i, "_", k,  "_", yearSpan, ".tif")
      print(system.time(writeRaster(r, fileName_out, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
    }
  }
}

# 20 year extreme cold, historical -----
k <- "historical"
for (i in modelChoices) {
  modelName.lower <- tolower(i)
  for (l in startYearChoices_historical) {
    yearSpan <- paste0(l, "_", l + yearRange)
    yearRange <- 19
    yearnumberRange <- seq(l, (l + yearRange), 1)
    gc()
    x <- lapply(yearnumberRange, readRastExtremeCld)
    r <- rast(x)
    fileName_out <- paste0("data/cmip6/extremeCldCt/extremeCldCt_", i, "_", k,  "_", yearSpan, ".tif")
    print(system.time(writeRaster(r, fileName_out, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
  }
}

# ensemble means -----
# combine all the spatrasters by model for the hemisphere, time period, and scenario and then take the mean across that combo
# ensemble mean, extreme heat -----
for (k in scenarioChoicesEnsemble) {
  for (hem in hemisphere)
    for (l in startYearChoices) {
      print(hem)
      yearRange <- 19
      #      if (m %in% "SH") yearRange <- 18
      yearSpan <- paste0(l, "_", l + yearRange)
      if (k %in% "historical") yearSpan <- "1991_2010"
      #      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      x <- lapply(modelChoices, readRastHeatEnsemble)
      r <- rast(x)
      fileName_out <- paste0("data/cmip6/extremeHtCt/ensemble_extremeHtCt", m, "_", k, "_", yearSpan, ".tif")
      system.time(r_mean <- app(r, fun = "mean", filename = fileName_out, overwrite = TRUE, wopt = woptList))
      plot(r_mean, main = paste0("Extreme heat days count ", m, ", ", k, ", period ", yearSpan))
    }
}

# ensemble mean, frost days -----
for (k in scenarioChoicesEnsemble) {
  for (hem in hemisphere)
    for (l in startYearChoices) {
      print(hem)
      yearRange <- 19
      #      if (m %in% "SH") yearRange <- 18
      yearSpan <- paste0(l, "_", l + yearRange)
      if (k %in% "historical") yearSpan <- "1991_2010"
      #      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      print(hem)
      x <- lapply(modelChoices, readRastFrostEnsemble)
      r <- rast(x)
      fileName_out <- paste0("data/cmip6/frostCt/ensemble_frostCt", m, "_", k, "_", yearSpan, ".tif")
      r_mean <- app(r, fun = "mean", filename = fileName_out, overwrite = TRUE, wopt = woptList)
      r_mean
      plot(r_mean, main = paste0("Frost days count ", m, ", ", k, ", period ", yearSpan))
    }
}

# ensemble mean, extreme cold  -----
for (k in scenarioChoicesEnsemble) {
  for (l in startYearChoices) {
    yearRange <- 19
    yearSpan <- paste0(l, "_", l + yearRange)
    if (k %in% "historical") yearSpan <- "1991_2010"
    #      yearnumberRange <- seq(l, (l + yearRange), 1)
    gc()
    x <- lapply(modelChoices, readRastExtremeCldEnsemble)
    r <- rast(x)
    fileName_out <- paste0("data/cmip6/extremeCldCt/ensemble_extremeCldtCt_", k, "_", yearSpan, ".tif")
    r_mean <- app(r, fun = "mean", filename = fileName_out, overwrite = TRUE, wopt = woptList)
    # truncate number of cold days at 3, so the values show up elsewhere besides the arctic
    r_mean_capped <- r_mean
    r_mean_capped[r_mean_capped > 3] <- 3
    plot(r_mean_capped, main = paste0("Extreme cold days (< -30°C) count, ", k, ", period ", yearSpan))
  }
}

# mosaic NH and SH -----
mosaicFileTypes <- c("frostCt", "extremeHtCt")
locofFilesToMosaic <- "data/cmip6/"

for (i in mosaicFileTypes) {
  for (k in scenarioChoicesEnsemble) {
    for (l in startYearChoices) {
      yearRange <- 19
      yearSpan <- paste0(l, "_", l + yearRange)
      if (k %in% "historical") yearSpan <- "1991_2010"
      NH <- paste0(locofFilesToMosaic, i, "/ensemble_", i, "NH", "_", k, "_", yearSpan, ".tif")
      print(NH)
      NH <- raster(NH) # read in as raster to use with mosaic
      SH <- paste0(locofFilesToMosaic, i, "/ensemble_", i, "SH", "_", k, "_", yearSpan, ".tif")
      print(SH)
      SH <- raster(SH)
      test <- mosaic(NH, SH, tolerance=0.05, fun = "mean", filename="")
      if (i %in% "frostCt") titleText <- paste0("Frost day (tmin < 0°C) counts in hemisphere spring ", k, ", period ", yearSpan)
      if (i %in% "extremeHtCt") titleText <- paste0("Max temp greater than 35°C in hemisphere high summer ", k, ", period ", yearSpan)
      
      plot(test, main = titleText)
    }
  }
}


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

source("R/globallyUsed.R")

locOfFiles <- locOfCMIP6tifFiles

sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
modelChoices <- c("MRI-ESM2-0") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_historical <- c(1991)
northernHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-c( -180, 180, -90, 0)
hemisphere <- c("NH", "SH")

yearRange <- 19
woptList <- list(gdal=c("COMPRESS=LZW"))

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2041
yearNumber <- 2043

# set of functions for the perennial crops limits

# readRastHeat, readRastFrost, readRastHeatEnsemble, and readRastFrostEnsemble are used to assemble sets of spatRasters
readRastHeat <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/extremeHtCt/extremeHtCt", m, "_", i, "_", k,  "_", yearNumber, ".tif")
  r <- rast(fileNameIn)
}
readRastFrost <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/frostCt/frostCt", m, "_", i, "_", k,  "_", yearNumber, ".tif")
  r <- rast(fileNameIn)
}

readRastExtremeCld <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/extremeCldCt/extremeCldCt", m, "_", i, "_", k,  "_", yearNumber, ".tif")
  r <- rast(fileNameIn)
}
readRastHeatEnsemble <- function(i) {
  fileNameIn <- paste0("data/cmip6/extremeHtCt/extremeHtCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
  print(fileNameIn)
  r <- rast(fileNameIn)
}
readRastFrostEnsemble <- function(i) {
  fileNameIn <- paste0("data/cmip6/frostCt/frostCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
  print(fileNameIn)
  r <- rast(fileNameIn)
}

readRastExtremeCldEnsemble <- function(i) {
  fileNameIn <- paste0("data/cmip6/extremeCldCt/extremeCldCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
  print(fileNameIn)
  r <- rast(fileNameIn)
}

# funFrost, funExtremeHeat, and funColdKill return counts of the number of days in the cellVector spatRaster that meet the criterion
funFrost <- function(cellVector) {
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
funExtremeCld <- function(cellVector) {
  ExtremeCldCt <- c(NA) 
  if (is.nan(cellVector[1])) {
    return(ExtremeCldCt)
  }
  ExtremeCldCt <- sum(cellVector < -35)
  return(ExtremeCldCt) 
}

# funCrop crops a climate variable to an extent
funCrop <- function(cvar, extent) {
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
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      system.time(NH <- funCrop("tmax", "NH"))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber, "-08-15")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmax_yr_NH <- subset(NH, indicesCharYear))
        
        # then use the function like this
        print(system.time(extremeHtCt <- app(tmax_yr_NH, funExtremeHeat)))
        fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCtNH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# 20 year heat temps, spatRasters by 20 year windows, by ESM and by ssp -----
for (k in sspChoices) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    for (m in hemisphere)
      for (l in startyearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        print(m)
        yearnumberRange <- seq(l, (l + yearRange), 1)
        gc()
        print(m)
        x <- lapply(yearnumberRange, readRastHeat)
        r <- rast(x)
        fileNameOut <- paste0("data/cmip6/extremeHtCt/extremeHtCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
        print(system.time(writeRaster(r, fileNameOut, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
      }
  }
}

# 20 year heat temps, historical -----
k <- "historical"
for (i in modelChoices) {
  modelName.lower <- tolower(i)
  for (m in hemisphere)
    for (l in startyearChoices_historical) {
      yearSpan <- paste0(l, "_", l + yearRange)
      print(m)
      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      x <- lapply(yearnumberRange, readRastHeat)
      r <- rast(x)
      r
      fileNameOut <- paste0("data/cmip6/extremeHtCt/extremeHtCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
      print(system.time(writeRaster(r, fileNameOut, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
    }
}

# NH frost -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
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
        print(system.time(frostCt <- app(tmin_yr_NH, funFrost)))
        fileName_out <- paste0("data/cmip6/frostCt/frostCtNH_",  i, "_", k, "_", yearNumber, ".tif")
        frostCt
        writeRaster(frostCt, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# NH heat historical -----
k <- "historical"
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    system.time(NH <- funCrop("tmax", "NH"))
    NH
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber, "-08-15")
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmax_yr_NH <- subset(NH, indicesCharYear))
      print(system.time(extremeHtCt <- app(tmax_yr_NH, funExtremeHeat)))
      print(extremeHtCt)
      fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCtNH_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, woptList = woptList)
      #      extremeHtCt <- NULL
      gc()
    }
  }
}

for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      system.time(NH <- funCrop("tmax", "NH"))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber, "-08-15")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmax_yr_NH <- subset(NH, indicesCharYear))
        print(system.time(extremeHtCt <- app(tmax_yr_NH, funExtremeHeat)))
        fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCtNH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# NH frost historical -----
k <- "historical"
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    system.time(NH <- funCrop("tmin", "NH"))
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-03-01"); endDate <- paste0(yearNumber, "-04-30")
      # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      # indicesChar <- paste0("X", as.character(indices))
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmin_yr_NH <- subset(NH, indicesCharYear))
      print(system.time(frostCt <- app(tmin_yr_NH, funFrost)))
      fileName_out <- paste0("data/cmip6/frostCt/frostCtNH_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(frostCt, fileName_out, overwrite = TRUE, woptList = woptList)
      #      frostCt <- NULL
      gc()
    }
  }
}

# 20 year frost temps, both NH and SH -----

for (k in sspChoices) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    for (m in hemisphere)
      for (l in startyearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        print(m)
        yearRange <- 19
        #        if (m %in% "SH") yearRange <- 18
        yearnumberRange <- seq(l, (l + yearRange), 1)
        gc()
        print(m)
        x <- lapply(yearnumberRange, readRastFrost)
        r <- rast(x)
        fileNameOut <- paste0("data/cmip6/frostCt/frostCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
        print(system.time(writeRaster(r, fileNameOut, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
      }
  }
}

# 20 year frost temps, historical, both NH and SH -----
k <- "historical"
for (i in modelChoices) {
  modelName.lower <- tolower(i)
  for (m in hemisphere)
    for (l in startyearChoices_historical) {
      yearSpan <- paste0(l, "_", l + yearRange)
      print(m)
      yearRange <- 19
      #     if (m %in% "SH") yearRange <- 18
      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      x <- lapply(yearnumberRange, readRastFrost)
      print(x)
      r <- rast(x)
      fileNameOut <- paste0("data/cmip6/frostCt/frostCt", m, "_", i, "_", k,  "_", yearSpan, ".tif")
      print(system.time(writeRaster(r, fileNameOut, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
    }
}

# Southern hemisphere -----
# SH heat -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      system.time(SH <- funCrop("tmax", "SH"))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-02-15")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmax_yr_SH <- subset(SH, indicesCharYear))
        print(system.time(extremeHtCt <- app(tmax_yr_SH, funExtremeHeat)))
        print(paste0("extremeHtCt: ", summary(extremeHtCt))); flush.console()
        fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCtSH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# SH frost -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      system.time(SH <- funCrop("tmin", "SH"))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-03-01"); endDate <- paste0(yearNumber, "-04-30")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmin_yr_SH <- subset(SH, indicesCharYear))
        
        print(system.time(frostCt <- app(tmin_yr_SH, funFrost)))
        fileName_out <- paste0("data/cmip6/frostCt/frostCtSH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(frostCt, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# SH heat historical -----
k = "historical"
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    system.time(SH <- funCrop("tmax", "SH"))
    
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
      writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, woptList = woptList)
    }
  }
}

# SH frost historical -----
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    system.time(SH <- funCrop("tmin", "SH"))
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-03-01"); endDate <- paste0(yearNumber, "-04-30")
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmin_yr_SH <- subset(SH, indicesCharYear))
      
      # then use the function like this
      print(system.time(frostCt <- app(tmin_yr_SH, funFrost)))
      print(frostCt)
      fileName_out <- paste0("data/cmip6/frostCt/frostCtSH_",  i, "_", k, "_", yearNumber, ".tif")
      print(paste0("frost historical outfile: ", fileName_out))
      writeRaster(frostCt, fileName_out, overwrite = TRUE, woptList = woptList)
    }
  }
}

# extreme cold calcs -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      fileName_tmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_", "tmin", "_global_daily_", yearSpan, ".tif")
      
      tmin <- rast(fileName_tmin)
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(extremeCld_yr <- subset(tmin, indicesCharYear))
        
        print(system.time(extremeCldCt <- app(extremeCld_yr, funExtremeCld)))
        fileName_out <- paste0("data/cmip6/extremeCldCt/extremeCldCt_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(frostCt, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# extreme cold historical -----
k <- "historical"
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    fileName_tmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_", "tmin", "_global_daily_", yearSpan, ".tif")
    
    tmin <- rast(fileName_tmin)
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(extremeCld_yr <- subset(tmin, indicesCharYear))
      print(system.time(extremeCldCt <- app(extremeCld_yr, funExtremeCld)))
      print(extremeCldCt)
      fileName_out <- paste0("data/cmip6/extremeCldCt/extremeCldCt_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(frostCt, fileName_out, overwrite = TRUE, woptList = woptList)
    }
  }
}

# 20 year extreme cold -----
for (k in sspChoices) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    for (l in startyearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      print(m)
      yearRange <- 19
      #        if (m %in% "SH") yearRange <- 18
      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      print(m)
      x <- lapply(yearnumberRange, readRastFrost)
      r <- rast(x)
      fileNameOut <- paste0("data/cmip6/extremeCldCt/extremeCldCt_", i, "_", k,  "_", yearSpan, ".tif")
      print(system.time(writeRaster(r, fileNameOut, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
    }
  }
}

# 20 year extreme cold, historical -----
k <- "historical"
for (i in modelChoices) {
  modelName.lower <- tolower(i)
  for (l in startyearChoices_historical) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(m)
    yearRange <- 19
    #        if (m %in% "SH") yearRange <- 18
    yearnumberRange <- seq(l, (l + yearRange), 1)
    gc()
    print(m)
    x <- lapply(yearnumberRange, readRastFrost)
    r <- rast(x)
    fileNameOut <- paste0("data/cmip6/extremeCldCt/extremeCldCt_", i, "_", k,  "_", yearSpan, ".tif")
    print(system.time(writeRaster(r, fileNameOut, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
  }
}

# ensemble means -----
# combine all the spat rasters by model for the hemisphere, time period, and scenario and then take the mean across that combo
scenarioChoicesEnsemble <- c("historical", sspChoices)
# ensemble mean, extreme heat -----
for (k in scenarioChoicesEnsemble) {
  for (m in hemisphere)
    for (l in startyearChoices) {
      print(m)
      yearRange <- 19
      #      if (m %in% "SH") yearRange <- 18
      yearSpan <- paste0(l, "_", l + yearRange)
      if (k %in% "historical") yearSpan <- "1991_2010"
      #      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      print(m)
      x <- lapply(modelChoices, readRastHeatEnsemble)
      r <- rast(x)
      fileName_out <- paste0("data/cmip6/extremeHtCt/ensemble_extremeHtCt", m, "_", k, "_", yearNumber, ".tif")
      r_mean <- app(r, fun = "mean", filename = fileName_out, overwrite = TRUE, woptList = woptList)
      plot(r_mean, main = paste0("Extreme heat days count ", m, ", ", k, ", period ", yearSpan))
    }
}


for (k in scenarioChoicesEnsemble) {
  for (m in hemisphere)
    for (l in startyearChoices) {
      print(m)
      yearRange <- 19
      #      if (m %in% "SH") yearRange <- 18
      yearSpan <- paste0(l, "_", l + yearRange)
      if (k %in% "historical") yearSpan <- "1991_2010"
      #      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      print(m)
      x <- lapply(modelChoices, readRastFrostEnsemble)
      r <- rast(x)
      fileName_out <- paste0("data/cmip6/frostCt/ensemble_frostCt", m, "_", k, "_", yearSpan, ".tif")
      r_mean <- app(r, fun = "mean", filename = fileName_out, overwrite = TRUE, woptList = woptList)
      r_mean
      plot(r_mean, main = paste0("Frost days count ", m, ", ", k, ", period ", yearSpan))
    }
}

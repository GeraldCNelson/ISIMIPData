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



source("R/globallyUsed.R")

locOfFiles <- locOfCMIP6tifFiles

sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c(  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_historical <- c(1991)
northernHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-c( -180, 180, -90, 0)

yearRange <- 19
woptList <- list(gdal=c("COMPRESS=LZW"))

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2041
yearNumber <- 2043


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

# NH heat
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      fileName_tasmax <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      tmax <- rast(fileName_tasmax)
      system.time(NH <- crop(tmax, ext(northernHemExtent)))
      
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber, "-08-15")
        # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        # indicesChar <- paste0("X", as.character(indices))
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

# NH frost
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
        # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        # indicesChar <- paste0("X", as.character(indices))
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmin_yr_NH <- subset(NH, indicesCharYear))
        
        # then use the function like this
        print(system.time(frostCt <- app(tmin_yr_NH, funFrost)))
        fileName_out <- paste0("data/cmip6/frostCt/frostCtNH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(frostCt, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# ------
# work on northern hemisphere historical -----
k <- "historical"
# NH heat
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    fileName_tasmax <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
    tmax <- rast(fileName_tasmax)
    system.time(NH <- crop(tmax, ext(northernHemExtent)))
    
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber, "-08-15")
      # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      # indicesChar <- paste0("X", as.character(indices))
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


# NH frost
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
    tmin <- rast(fileName_tasmin)
    system.time(NH <- crop(tmin, ext(northernHemExtent)))
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-03-01"); endDate <- paste0(yearNumber, "-04-30")
      # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      # indicesChar <- paste0("X", as.character(indices))
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmin_yr_NH <- subset(NH, indicesCharYear))
      
      # then use the function like this
      print(system.time(frostCt <- app(tmin_yr_NH, funFrost)))
      fileName_out <- paste0("data/cmip6/frostCt/frostCtNH_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(frostCt, fileName_out, overwrite = TRUE, woptList = woptList)
    }
  }
}


# work on southern hemisphere -----

# SH heat -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      fileName_tasmax <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      tmax <- rast(fileName_tasmax)
      system.time(SH <- crop(tmax, ext(southernHemExtent)))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-02-15")
        # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        # indicesChar <- paste0("X", as.character(indices))
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmax_yr_SH <- subset(SH, indicesCharYear))
        
        # then use the function like this
        print(system.time(extremeHtCt <- app(tmax_yr_SH, funExtremeHeat)))
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
      fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      tmin <- rast(fileName_tasmin)
      system.time(SH <- crop(tmin, ext(southernHemExtent)))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-03-01"); endDate <- paste0(yearNumber, "-04-30")
        # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        # indicesChar <- paste0("X", as.character(indices))
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmin_yr_SH <- subset(SH, indicesCharYear))
        
        # then use the function like this
        print(system.time(frostCt <- app(tmin_yr_SH, funFrost)))
        fileName_out <- paste0("data/cmip6/frostCt/frostCtSH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(frostCt, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# ---------
# work on southern hemisphere historical -----

k = "historical"

# SH heat -----
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    fileName_tasmax <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
    tmax <- rast(fileName_tasmax)
    system.time(SH <- crop(tmax, ext(southernHemExtent)))
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-02-15")
      # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      # indicesChar <- paste0("X", as.character(indices))
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmax_yr_SH <- subset(SH, indicesCharYear))
      
      # then use the function like this
      print(system.time(extremeHtCt <- app(tmax_yr_SH, funExtremeHeat)))
      fileName_out <- paste0("data/cmip6/extremeHtCt/extremeHtCtSH_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(extremeHtCt, fileName_out, overwrite = TRUE, woptList = woptList)
    }
  }
}


# SH frost -----
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
    tmin <- rast(fileName_tasmin)
    system.time(SH <- crop(tmin, ext(southernHemExtent)))
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-03-01"); endDate <- paste0(yearNumber, "-04-30")
      # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      # indicesChar <- paste0("X", as.character(indices))
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmin_yr_SH <- subset(SH, indicesCharYear))
      
      # then use the function like this
      print(system.time(frostCt <- app(tmin_yr_SH, funFrost)))
      fileName_out <- paste0("data/cmip6/frostCt/frostCtSH_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(frostCt, fileName_out, overwrite = TRUE, woptList = woptList)
    }
  }
}



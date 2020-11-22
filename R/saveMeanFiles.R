# save .mean files
library("terra")
if (get_os() %in% "osx") {
  terraOptions(memfrac = 4, progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path, memfrac = .9,
}else{
  terraOptions(memfrac = .6,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
}

yearRange <- 19
woptList <- list(gdal=c("COMPRESS=LZW"))

locOfFiles <- "data/bigFiles/"
sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
modelChoices <- c( "UKESM1-0-LL") #, "GFDL-ESM4") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
#startyearChoices <-  c(2041) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_historical <- c(1991)
scenarioChoicesEnsemble <- c("historical", sspChoices)

for (k in sspChoices) {
  #    k = "ssp126"
  for (i in modelChoices) {
    # i = "UKESM1-0-LL"
    for (l in startyearChoices) {
      # l = 2041
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      fileName_rh <- paste0(locOfFiles, modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      fileName_tasmax <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      fileName_tasmin <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      fileName_tas <- paste0(locOfFiles,  modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
      
      rh <- rast(fileName_rh)
      indices_day <- format(as.Date(names(rh), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
      indices_day <- as.numeric(indices_day)
      names(rh) <- indices_day
      print(paste0("rh min: ", round(min(minmax(rh)), 3), " rh max: ", round(max(minmax(rh)), 3), ", rh file: ", fileName_rh ))
      system.time(rh.mean <- tapp(rh, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      print(paste0("rh.mean min: ", round(min(minmax(rh.mean)), 2), ", rh.mean max: ", round(max(minmax(rh.mean)), 2)))
      fileName_rh_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      print(paste0("rh file out: ", fileName_rh_out))
      print(system.time(writeRaster(rh.mean, filename = fileName_rh_out, overwrite=TRUE, wopt = woptList))); flush.console()
      
      tmax <- rast(fileName_tasmax)
      indices_day <- format(as.Date(names(tmax), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
      indices_day <- as.numeric(indices_day)
      names(tmax) <- indices_day
      print(paste0("tmax min: ", round(min(minmax(tmax), 2)), ", tmax max: ", round((max(minmax(tmax))), 2), ", tmax file: ", fileName_tasmax ))
      system.time(tmax.mean <- tapp(tmax, indices_day, fun = "mean", na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      print(paste0("tmax.mean min: ", round(min(minmax(tmax.mean)), 2), ", tmax.mean max: ", round(max(minmax(tmax.mean)), 2)))
      fileName_tmax_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      print(paste0("tmax file out: ", fileName_tmax_out))
      print(system.time(writeRaster(tmax.mean, filename = fileName_tmax_out, overwrite=TRUE, wopt = woptList))); flush.console()
      
      tmin <- rast(fileName_tasmin)
      indices_day <- format(as.Date(names(tmin), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
      indices_day <- as.numeric(indices_day)
      names(tmin) <- indices_day
      print(paste0("tmin min: ", round(min(minmax(tmin), 2)), ", tmin max: ", round((max(minmax(tmin))), 2), ", tmin file: ", fileName_tasmin ))
      system.time(tmin.mean <- tapp(tmin, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      print(paste0("tmin.mean min: ", round(min(minmax(tmin.mean)), 2), ", tmin.mean max: ", round(max(minmax(tmin.mean)), 2)))
      fileName_tmin_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      print(paste0("tmin file out: ", fileName_tmin_out))
      print(system.time(writeRaster(tmin.mean, filename = fileName_tmin_out, overwrite=TRUE, wopt = woptList))); flush.console()
      
      tas <- rast(fileName_tas)
      indices_day <- format(as.Date(names(tas), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
      indices_day <- as.numeric(indices_day)
      names(tas) <- indices_day
      print(paste0("tas min: ", round(min(minmax(tas)), 2), " tas max: ", round(max(minmax(tas)), 2), ", tas file: ", fileName_tas ))
      system.time(tas.mean <- tapp(tas, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      print(paste0("tas.mean min: ", round(min(minmax(tas.mean)), 2), ", tas.mean max: ", round(max(minmax(tas.mean)), 2)))
      fileName_tas_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
      print(paste0("tas file out: ", fileName_tas_out))
      print(system.time(writeRaster(tas.mean, filename = fileName_tas_out, overwrite=TRUE, wopt = woptList))); flush.console()
    }
  }
}

# historical
k = "historical"
l = 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (i in modelChoices) {
  modelName.lower <- tolower(i)
  fileName_rh <- paste0(locOfFiles, modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
  fileName_tasmax <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
  fileName_tasmin <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
  fileName_tas <- paste0(locOfFiles,  modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
  
  rh <- rast(fileName_rh)
  indices_day <- format(as.Date(names(rh), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
  indices_day <- as.numeric(indices_day)
  names(rh) <- indices_day
  print(paste0("rh min: ", round(min(minmax(rh)), 3), " rh max: ", round(max(minmax(rh)), 3), ", rh file: ", fileName_rh ))
  system.time(rh.mean <- tapp(rh, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
  print(paste0("rh.mean min: ", round(min(minmax(rh.mean)), 2), ", rh.mean max: ", round(max(minmax(rh.mean)), 2)))
  fileName_rh_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
  print(paste0("rh file out: ", fileName_rh_out))
  print(system.time(writeRaster(rh.mean, filename = fileName_rh_out, overwrite=TRUE, wopt = woptList))); flush.console()
  
  tmax <- rast(fileName_tasmax)
  indices_day <- format(as.Date(names(tmax), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
  indices_day <- as.numeric(indices_day)
  names(tmax) <- indices_day
  print(paste0("tmax min: ", round(min(minmax(tmax), 2)), ", tmax max: ", round((max(minmax(tmax))), 2), ", tmax file: ", fileName_tasmax ))
  system.time(tmax.mean <- tapp(tmax, indices_day, fun = "mean", na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
  print(paste0("tmax.mean min: ", round(min(minmax(tmax.mean)), 2), ", tmax.mean max: ", round(max(minmax(tmax.mean)), 2)))
  fileName_tmax_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
  print(paste0("tmax file out: ", fileName_tmax_out))
  print(system.time(writeRaster(tmax.mean, filename = fileName_tmax_out, overwrite=TRUE, wopt = woptList))); flush.console()
  
  tmin <- rast(fileName_tasmin)
  indices_day <- format(as.Date(names(tmin), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
  indices_day <- as.numeric(indices_day)
  names(tmin) <- indices_day
  print(paste0("tmin min: ", round(min(minmax(tmin), 2)), ", tmin max: ", round((max(minmax(tmin))), 2), ", tmin file: ", fileName_tasmin ))
  system.time(tmin.mean <- tapp(tmin, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
  print(paste0("tmin.mean min: ", round(min(minmax(tmin.mean)), 2), ", tmin.mean max: ", round(max(minmax(tmin.mean)), 2)))
  fileName_tmin_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
  print(paste0("tmin file out: ", fileName_tmin_out))
  print(system.time(writeRaster(tmin.mean, filename = fileName_tmin_out, overwrite=TRUE, wopt = woptList))); flush.console()
  
  tas <- rast(fileName_tas)
  indices_day <- format(as.Date(names(tas), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
  indices_day <- as.numeric(indices_day)
  names(tas) <- indices_day
  print(paste0("tas min: ", round(min(minmax(tas)), 2), " tas max: ", round(max(minmax(tas)), 2), ", tas file: ", fileName_tas ))
  system.time(tas.mean <- tapp(tas, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
  print(paste0("tas.mean min: ", round(min(minmax(tas.mean)), 2), ", tas.mean max: ", round(max(minmax(tas.mean)), 2)))
  fileName_tas_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
  print(paste0("tas file out: ", fileName_tas_out))
  print(system.time(writeRaster(tas.mean, filename = fileName_tas_out, overwrite=TRUE, wopt = woptList))); flush.console()
}

# do monthly averages

for (k in sspChoices) {
  #    k = "ssp126"
  for (l in startyearChoices) {
    # set up names with Jan-1. Need to set up the endDate_year with a leap year such as 2040
    yearSpan <- paste0(l, "_", l + yearRange)
    
    startDate_year <- paste0(2040, "-01-01")
    endDate_year <- paste0(2040, "-12-31")
    indices_mnth<- seq(as.Date(startDate_year), as.Date(endDate_year), 1)
    indices_mnth <- format(indices_mnth, "%b")
    
    for (i in modelChoices) {
      #     i = "UKESM1-0-LL"
      modelName.lower <- tolower(i)
      fileName_rh <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      fileName_tasmax <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      fileName_tasmin <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      fileName_tas <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
      
      rh <- rast(fileName_rh)
      names(rh) <- indices_mnth
      print(paste0("rh min: ", round(min(minmax(rh)), 3), " rh max: ", round(max(minmax(rh)), 3), ", rh file: ", fileName_rh ))
      system.time(rh.mean <- tapp(rh, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      print(paste0("rh.mean min: ", round(min(minmax(rh.mean)), 2), ", rh.mean max: ", round(max(minmax(rh.mean)), 2)))
      fileName_rh_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      print(paste0("rh file out: ", fileName_rh_out))
      print(system.time(writeRaster(rh.mean, filename = fileName_rh_out, overwrite=TRUE, wopt = woptList))); flush.console()
      
      tmax <- rast(fileName_tasmax)
      names(tmax) <- indices_mnth
      print(paste0("tmax min: ", round(min(minmax(tmax), 2)), ", tmax max: ", round((max(minmax(tmax))), 2), ", tmax file: ", fileName_tasmax ))
      system.time(tmax.mean <- tapp(tmax, indices_mnth, fun = "mean", na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      print(paste0("tmax.mean min: ", round(min(minmax(tmax.mean)), 2), ", tmax.mean max: ", round(max(minmax(tmax.mean)), 2)))
      fileName_tmax_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      print(paste0("tmax file out: ", fileName_tmax_out))
      print(system.time(writeRaster(tmax.mean, filename = fileName_tmax_out, overwrite=TRUE, wopt = woptList))); flush.console()
      
      tmin <- rast(fileName_tasmin)
      names(tmin) <- indices_mnth
      print(paste0("tmin min: ", round(min(minmax(tmin), 2)), ", tmin max: ", round((max(minmax(tmin))), 2), ", tmin file: ", fileName_tasmin ))
      system.time(tmin.mean <- tapp(tmin, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      print(paste0("tmin.mean min: ", round(min(minmax(tmin.mean)), 2), ", tmin.mean max: ", round(max(minmax(tmin.mean)), 2)))
      fileName_tmin_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      print(paste0("tmin file out: ", fileName_tmin_out))
      print(system.time(writeRaster(tmin.mean, filename = fileName_tmin_out, overwrite=TRUE, wopt = woptList))); flush.console()
      
      tas <- rast(fileName_tas)
      names(tas) <- indices_mnth
      print(paste0("tas min: ", round(min(minmax(tas)), 2), " tas max: ", round(max(minmax(tas)), 2), ", tas file: ", fileName_tas ))
      system.time(tas.mean <- tapp(tas, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      print(paste0("tas.mean min: ", round(min(minmax(tas.mean)), 2), ", tas.mean max: ", round(max(minmax(tas.mean)), 2)))
      fileName_tas_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
      print(paste0("tas file out: ", fileName_tas_out))
      print(system.time(writeRaster(tas.mean, filename = fileName_tas_out, overwrite=TRUE, wopt = woptList))); flush.console()
      
    }
  }
}
  
# historical monthly averages
k = "historical"
l = 1991
yearSpan <- paste0(l, "_", l + yearRange)

# set up names with Jan-1. Need to set up the endDate_year with a leap year such as 2040
startDate_year <- paste0(2040, "-01-01")
endDate_year <- paste0(2040, "-12-31")
indices_mnth<- seq(as.Date(startDate_year), as.Date(endDate_year), 1)
indices_mnth <- format(indices_mnth, "%b")

for (i in modelChoices) {
  #     i = "UKESM1-0-LL"
  modelName.lower <- tolower(i)
  fileName_rh <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
  fileName_tasmax <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
  fileName_tasmin <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
  fileName_tas <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
  
  rh <- rast(fileName_rh)
  names(rh) <- indices_mnth
  print(paste0("rh min: ", round(min(minmax(rh)), 3), " rh max: ", round(max(minmax(rh)), 3), ", rh file: ", fileName_rh ))
  system.time(rh.mean <- tapp(rh, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
  print(paste0("rh.mean min: ", round(min(minmax(rh.mean)), 2), ", rh.mean max: ", round(max(minmax(rh.mean)), 2)))
  fileName_rh_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
  print(paste0("rh file out: ", fileName_rh_out))
  print(system.time(writeRaster(rh.mean, filename = fileName_rh_out, overwrite=TRUE, wopt = woptList))); flush.console()
  print(rh.mean)
  
  tmax <- rast(fileName_tasmax)
  names(tmax) <- indices_mnth
  print(paste0("tmax min: ", round(min(minmax(tmax), 2)), ", tmax max: ", round((max(minmax(tmax))), 2), ", tmax file: ", fileName_tasmax ))
  system.time(tmax.mean <- tapp(tmax, indices_mnth, fun = "mean", na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
  print(paste0("tmax.mean min: ", round(min(minmax(tmax.mean)), 2), ", tmax.mean max: ", round(max(minmax(tmax.mean)), 2)))
  fileName_tmax_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
  print(paste0("tmax file out: ", fileName_tmax_out))
  print(system.time(writeRaster(tmax.mean, filename = fileName_tmax_out, overwrite=TRUE, wopt = woptList))); flush.console()
  
  tmin <- rast(fileName_tasmin)
  names(tmin) <- indices_mnth
  print(paste0("tmin min: ", round(min(minmax(tmin), 2)), ", tmin max: ", round((max(minmax(tmin))), 2), ", tmin file: ", fileName_tasmin ))
  system.time(tmin.mean <- tapp(tmin, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
  print(paste0("tmin.mean min: ", round(min(minmax(tmin.mean)), 2), ", tmin.mean max: ", round(max(minmax(tmin.mean)), 2)))
  fileName_tmin_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
  print(paste0("tmin file out: ", fileName_tmin_out))
  print(system.time(writeRaster(tmin.mean, filename = fileName_tmin_out, overwrite=TRUE, wopt = woptList))); flush.console()
  
  tas <- rast(fileName_tas)
  names(tas) <- indices_mnth
  print(paste0("tas min: ", round(min(minmax(tas)), 2), " tas max: ", round(max(minmax(tas)), 2), ", tas file: ", fileName_tas ))
  system.time(tas.mean <- tapp(tas, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
  print(paste0("tas.mean min: ", round(min(minmax(tas.mean)), 2), ", tas.mean max: ", round(max(minmax(tas.mean)), 2)))
  fileName_tas_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
  print(paste0("tas file out: ", fileName_tas_out))
  print(system.time(writeRaster(tas.mean, filename = fileName_tas_out, overwrite=TRUE, wopt = woptList))); flush.console()
}
  
  
  
  # rename files
  renameList <- list.files(locOfFiles, full.names = TRUE)
  renameList <- renameList[grepl("mean", renameList, fixed = TRUE)]
  renameList_out <- gsub("mean", "dyMean20yr", renameList)
  renameList <- gsub("//","/", renameList)
  renameList_out <- gsub("//","/", renameList_out)
  
  for (i in 1:length(renameList)) {
    file.rename(from = renameList[i], to = renameList_out[i])
  }
  
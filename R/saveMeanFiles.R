# save .mean files
library("terra")
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
  terraOptions(memfrac = 4, progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path, memfrac = .9,
}else{
  terraOptions(memfrac = .6,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
}

yearRange <- 19
woptList <- list(gdal=c("COMPRESS=LZW"))

locOfFiles <- "data/bigFiles/"
sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp126") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("UKESM1-0-LL") #, "GFDL-ESM4") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startYearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
#startYearChoices <-  c(2041) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
scenarioChoicesEnsemble <- c("historical", sspChoices)
climateVars <- c("tasmin", "tasmax", "tas", "pr", "hurs", "rsds", "sfcwind", "ps") 
climateVars <- c("tasmin") # "tasmax", "tasmin"

f_means <- function() {
  for (m in climateVars) {
    for (i in modelChoices) {
      # i = "UKESM1-0-LL"
      # l = 2081
      modelName.lower <- tolower(i)
      if (m %in% "rh") {
        fileName_rh <- paste0(locOfFiles, modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
        rh <- rast(fileName_rh)
        indices_day <- format(as.Date(names(rh), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
        indices_day <- as.numeric(indices_day)
        names(rh) <- indices_day
        print(paste0("rh min: ", round(min(minmax(rh)), 3), " rh max: ", round(max(minmax(rh)), 3), ", rh file: ", fileName_rh ))
        system.time(rh.mean <- tapp(rh, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
        print(paste0("rh.mean min: ", round(min(minmax(rh.mean)), 2), ", rh.mean max: ", round(max(minmax(rh.mean)), 2)))
        fileName_rh_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
        print(paste0("rh file out: ", fileName_rh_out))
        print(system.time(writeRaster(rh.mean, filename = fileName_rh_out, overwrite=TRUE, wopt = woptList))); flush.console()
      }
      if (m %in% "tasmax") {
        fileName_tasmax <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
        tmax <- rast(fileName_tasmax)
        indices_day <- format(as.Date(names(tmax), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
        indices_day <- as.numeric(indices_day)
        names(tmax) <- indices_day
        print(paste0("tmax min: ", round(min(minmax(tmax), 2)), ", tmax max: ", round((max(minmax(tmax))), 2), ", tmax file: ", fileName_tasmax ))
        system.time(tmax.mean <- tapp(tmax, indices_day, fun = "mean", na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
        print(paste0("tmax.mean min: ", round(min(minmax(tmax.mean)), 2), ", tmax.mean max: ", round(max(minmax(tmax.mean)), 2)))
        fileName_tmax_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
        print(paste0("tmax file out: ", fileName_tmax_out))
        print(system.time(writeRaster(tmax.mean, filename = fileName_tmax_out, overwrite=TRUE, wopt = woptList))); flush.console()
      }
      if (m %in% "tasmin") {
        fileName_tasmin <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
        tmin <- rast(fileName_tasmin)
        indices_day <- format(as.Date(names(tmin), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
        indices_day <- as.numeric(indices_day)
        names(tmin) <- indices_day
        print(paste0("tmin min: ", round(min(minmax(tmin), 2)), ", tmin max: ", round((max(minmax(tmin))), 2), ", tmin file: ", fileName_tasmin ))
        system.time(tmin.mean <- tapp(tmin, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
        print(paste0("tmin.mean min: ", round(min(minmax(tmin.mean)), 2), ", tmin.mean max: ", round(max(minmax(tmin.mean)), 2)))
        fileName_tmin_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
        print(paste0("tmin file out: ", fileName_tmin_out))
        print(system.time(writeRaster(tmin.mean, filename = fileName_tmin_out, overwrite=TRUE, wopt = woptList))); flush.console()
      }
      if (m %in% "tas") {
        fileName_tas <- paste0(locOfFiles,  modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
        tas <- rast(fileName_tas)
        indices_day <- format(as.Date(names(tas), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
        indices_day <- as.numeric(indices_day)
        names(tas) <- indices_day
        print(paste0("tas min: ", round(min(minmax(tas)), 2), " tas max: ", round(max(minmax(tas)), 2), ", tas file: ", fileName_tas ))
        system.time(tas.mean <- tapp(tas, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
        print(paste0("tas.mean min: ", round(min(minmax(tas.mean)), 2), ", tas.mean max: ", round(max(minmax(tas.mean)), 2)))
        fileName_tas_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
        print(paste0("tas file out: ", fileName_tas_out))
        print(system.time(writeRaster(tas.mean, filename = fileName_tas_out, overwrite=TRUE, wopt = woptList))); flush.console()
      }
      if (m %in% "sfcWind") {
        fileName_sfcWind <- paste0(locOfFiles,  modelName.lower, "_", k, "_sfcWind_global_daily_", yearSpan, ".tif")
        sfcWind <- rast(fileName_sfcWind)
        indices_day <- format(as.Date(names(sfcWind), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
        indices_day <- as.numeric(indices_day)
        names(sfcWind) <- indices_day
        print(paste0("sfcWind min: ", round(min(minmax(sfcWind)), 2), " sfcWind max: ", round(max(minmax(sfcWind)), 2), ", sfcWind file: ", fileName_sfcWind ))
        system.time(sfcWind.mean <- tapp(sfcWind, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
        print(paste0("sfcWind.mean min: ", round(min(minmax(sfcWind.mean)), 2), ", sfcWind.mean max: ", round(max(minmax(sfcWind.mean)), 2)))
        fileName_sfcWind_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_sfcWind_global_daily_", yearSpan, ".tif")
        print(paste0("sfcWind file out: ", fileName_sfcWind_out))
        print(system.time(writeRaster(sfcWind.mean, filename = fileName_sfcWind_out, overwrite=TRUE, wopt = woptList))); flush.console()
      }
      if (m %in% "rsds") {
        fileName_rsds <- paste0(locOfFiles,  modelName.lower, "_", k, "_rsds_global_daily_", yearSpan, ".tif")
        rsds <- rast(fileName_rsds)
        indices_day <- format(as.Date(names(rsds), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
        indices_day <- as.numeric(indices_day)
        names(rsds) <- indices_day
        print(paste0("rsds min: ", round(min(minmax(rsds)), 2), " rsds max: ", round(max(minmax(rsds)), 2), ", rsds file: ", fileName_rsds ))
        system.time(rsds.mean <- tapp(rsds, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
        print(paste0("rsds.mean min: ", round(min(minmax(rsds.mean)), 2), ", rsds.mean max: ", round(max(minmax(rsds.mean)), 2)))
        fileName_rsds_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_rsds_global_daily_", yearSpan, ".tif")
        print(paste0("rsds file out: ", fileName_rsds_out))
        print(system.time(writeRaster(rsds.mean, filename = fileName_rsds_out, overwrite=TRUE, wopt = woptList))); flush.console()
      }
      if (m %in% "ps") {
        fileName_ps <- paste0(locOfFiles,  modelName.lower, "_", k, "_ps_global_daily_", yearSpan, ".tif")
        ps <- rast(fileName_ps)
        indices_day <- format(as.Date(names(ps), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
        indices_day <- as.numeric(indices_day)
        names(ps) <- indices_day
        print(paste0("ps min: ", round(min(minmax(ps)), 2), " ps max: ", round(max(minmax(ps)), 2), ", ps file: ", fileName_ps ))
        system.time(ps.mean <- tapp(ps, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
        print(paste0("ps.mean min: ", round(min(minmax(ps.mean)), 2), ", ps.mean max: ", round(max(minmax(ps.mean)), 2)))
        fileName_ps_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_ps_global_daily_", yearSpan, ".tif")
        print(paste0("ps file out: ", fileName_ps_out))
        print(system.time(writeRaster(ps.mean, filename = fileName_ps_out, overwrite=TRUE, wopt = woptList))); flush.console()
      }
      if (m %in% "pr") {
        fileName_pr <- paste0(locOfFiles,  modelName.lower, "_", k, "_pr_global_daily_", yearSpan, ".tif")
        pr <- rast(fileName_pr)
        indices_day <- format(as.Date(names(pr), format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
        indices_day <- as.numeric(indices_day)
        names(pr) <- indices_day
        print(paste0("pr min: ", round(min(minmax(pr)), 2), " pr max: ", round(max(minmax(pr)), 2), ", pr file: ", fileName_pr ))
        system.time(pr.mean <- tapp(pr, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
        print(paste0("pr.mean min: ", round(min(minmax(pr.mean)), 2), ", pr.mean max: ", round(max(minmax(pr.mean)), 2)))
        fileName_pr_out <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_pr_global_daily_", yearSpan, ".tif")
        print(paste0("pr file out: ", fileName_pr_out))
        print(system.time(writeRaster(pr.mean, filename = fileName_pr_out, overwrite=TRUE, wopt = woptList))); flush.console()
      }
    }
  }
}

for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    f_means()
  }
}

# historical
k = "historical"
l = 1991
yearSpan <- paste0(l, "_", l + yearRange)
f_means()

# do monthly averages

for (k in sspChoices) {
  #    k = "ssp126"
  for (l in startYearChoices) {
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
      system.time(rh.mean <- tapp(rh, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
      print(paste0("rh.mean min: ", round(min(minmax(rh.mean)), 2), ", rh.mean max: ", round(max(minmax(rh.mean)), 2)))
      fileName_rh_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      print(paste0("rh file out: ", fileName_rh_out))
      print(system.time(writeRaster(rh.mean, filename = fileName_rh_out, overwrite=TRUE, wopt = woptList))); flush.console()
      
      tmax <- rast(fileName_tasmax)
      names(tmax) <- indices_mnth
      print(paste0("tmax min: ", round(min(minmax(tmax), 2)), ", tmax max: ", round((max(minmax(tmax))), 2), ", tmax file: ", fileName_tasmax ))
      system.time(tmax.mean <- tapp(tmax, indices_mnth, fun = "mean", na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
      print(paste0("tmax.mean min: ", round(min(minmax(tmax.mean)), 2), ", tmax.mean max: ", round(max(minmax(tmax.mean)), 2)))
      fileName_tmax_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      print(paste0("tmax file out: ", fileName_tmax_out))
      print(system.time(writeRaster(tmax.mean, filename = fileName_tmax_out, overwrite=TRUE, wopt = woptList))); flush.console()
      
      tmin <- rast(fileName_tasmin)
      names(tmin) <- indices_mnth
      print(paste0("tmin min: ", round(min(minmax(tmin), 2)), ", tmin max: ", round((max(minmax(tmin))), 2), ", tmin file: ", fileName_tasmin ))
      system.time(tmin.mean <- tapp(tmin, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
      print(paste0("tmin.mean min: ", round(min(minmax(tmin.mean)), 2), ", tmin.mean max: ", round(max(minmax(tmin.mean)), 2)))
      fileName_tmin_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      print(paste0("tmin file out: ", fileName_tmin_out))
      print(system.time(writeRaster(tmin.mean, filename = fileName_tmin_out, overwrite=TRUE, wopt = woptList))); flush.console()
      
      tas <- rast(fileName_tas)
      names(tas) <- indices_mnth
      print(paste0("tas min: ", round(min(minmax(tas)), 2), " tas max: ", round(max(minmax(tas)), 2), ", tas file: ", fileName_tas ))
      system.time(tas.mean <- tapp(tas, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
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
  system.time(rh.mean <- tapp(rh, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
  print(paste0("rh.mean min: ", round(min(minmax(rh.mean)), 2), ", rh.mean max: ", round(max(minmax(rh.mean)), 2)))
  fileName_rh_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
  print(paste0("rh file out: ", fileName_rh_out))
  print(system.time(writeRaster(rh.mean, filename = fileName_rh_out, overwrite=TRUE, wopt = woptList))); flush.console()
  print(rh.mean)
  
  tmax <- rast(fileName_tasmax)
  names(tmax) <- indices_mnth
  print(paste0("tmax min: ", round(min(minmax(tmax), 2)), ", tmax max: ", round((max(minmax(tmax))), 2), ", tmax file: ", fileName_tasmax ))
  system.time(tmax.mean <- tapp(tmax, indices_mnth, fun = "mean", na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
  print(paste0("tmax.mean min: ", round(min(minmax(tmax.mean)), 2), ", tmax.mean max: ", round(max(minmax(tmax.mean)), 2)))
  fileName_tmax_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
  print(paste0("tmax file out: ", fileName_tmax_out))
  print(system.time(writeRaster(tmax.mean, filename = fileName_tmax_out, overwrite=TRUE, wopt = woptList))); flush.console()
  
  tmin <- rast(fileName_tasmin)
  names(tmin) <- indices_mnth
  print(paste0("tmin min: ", round(min(minmax(tmin), 2)), ", tmin max: ", round((max(minmax(tmin))), 2), ", tmin file: ", fileName_tasmin ))
  system.time(tmin.mean <- tapp(tmin, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
  print(paste0("tmin.mean min: ", round(min(minmax(tmin.mean)), 2), ", tmin.mean max: ", round(max(minmax(tmin.mean)), 2)))
  fileName_tmin_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
  print(paste0("tmin file out: ", fileName_tmin_out))
  print(system.time(writeRaster(tmin.mean, filename = fileName_tmin_out, overwrite=TRUE, wopt = woptList))); flush.console()
  
  tas <- rast(fileName_tas)
  names(tas) <- indices_mnth
  print(paste0("tas min: ", round(min(minmax(tas)), 2), " tas max: ", round(max(minmax(tas)), 2), ", tas file: ", fileName_tas ))
  system.time(tas.mean <- tapp(tas, indices_mnth, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
  print(paste0("tas.mean min: ", round(min(minmax(tas.mean)), 2), ", tas.mean max: ", round(max(minmax(tas.mean)), 2)))
  fileName_tas_out <- paste0(locOfFiles, "mnthMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
  print(paste0("tas file out: ", fileName_tas_out))
  print(system.time(writeRaster(tas.mean, filename = fileName_tas_out, overwrite=TRUE, wopt = woptList))); flush.console()
}



# # rename files
# renameList <- list.files(locOfFiles, full.names = TRUE)
# renameList <- renameList[grepl("mean", renameList, fixed = TRUE)]
# renameList_out <- gsub("mean", "dyMean20yr", renameList)
# renameList <- gsub("//","/", renameList)
# renameList_out <- gsub("//","/", renameList_out)
# 
# for (i in 1:length(renameList)) {
#   file.rename(from = renameList[i], to = renameList_out[i])
# }
s
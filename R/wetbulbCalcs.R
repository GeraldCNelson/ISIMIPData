# working on wet bulb temperature

library(terra)
library(wbgt)

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

terraOptions(memfrac = 4, progress = 0, tempdir =  "data/ISIMIP", verbose = FALSE)

#locOfFiles <- locOfCMIP6tifFiles
locOfFiles <- "data/bigFiles/"
speciesChoice <- c("humans", "cattle", "goat", "swine", "chicken", "sheep") 
speciesChoice <- c("swine", "sheep")
sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_historical <- c(1991)
scenarioChoicesEnsemble <- c("historical", sspChoices)

ext_noAntarctica <- ext(-180, 180, -60, 90)

yearRange <- 19
woptList <- list(gdal=c("COMPRESS=LZW"))
colorList <- (RColorBrewer::brewer.pal(5, "RdYlGn"))

#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2081
yearNumber <- 2043

# convert longitude to time difference to GMT
td <- function(lon) {
  sec <- lon / 0.004167
  hrs <- round(sec/3600, 0)
}

for (k in sspChoices) {
  #    k = "ssp126"
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices <- paste0("X", as.character(indices))
    indices_day <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
    indices_day <- as.numeric(indices_day)
#    indices_month <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%b") # %j is month of the day of the year
    dayAll <- unique(indices_day)
    year <- rep(l, length(day))
    month <- rep(0, length(day))
    hour <- rep(12, length(day))
    urban <- rep(0, length(day))
    zspeed <- rep(2, length(day))
    dT <- rep(0, length(day))
    
    for (i in modelChoices) {
      modelName.lower <- tolower(i)
      fileName_rh_mean <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      # fileName_tmax_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      # fileName_tmin_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      fileName_tas_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
      fileName_ps_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_ps_global_daily_", yearSpan, ".tif")
      fileName_sfcWind_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_sfcWind_global_daily_", yearSpan, ".tif")
      fileName_rsds_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_rsds_global_daily_", yearSpan, ".tif")
      
      rh <- rast(fileName_rh_mean)
      rh_df <- data.frame(x = as.numeric(), y = as.numeric(), relhum = as.numeric())
      for (i in 1:nlyr(rh)) {
        r <- rh[[i]]
      r_df <- as.data.frame(r, xy = TRUE)
      year <- rep(l, nrow(r_df))
      month <- rep(0, nrow(r_df))
      year <- rep(l, nrow(r_df))
      day <- rep(dayAll[i],nrow(r_df))
      hour <- rep(12, nrow(r_df))
      urban <- rep(0, nrow(r_df))
      zspeed <- rep(2, nrow(r_df))
      dT <- rep(0, nrow(r_df))
      
      names(r_df) <- c("lon", "lat", "relhum")
      r_df <- cbind(r_df, day, year, month, hour, urban, zspeed, dT)
      rh_df <- rbind(rh_df, r_df)
      }
      
      #solar
      solar <- rast(fileName_rsds_mean)
      
      
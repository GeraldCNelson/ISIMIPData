# daily mean calcs, 20 years ------
library(terra)
terraOptions(memfrac = 2,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path

woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

locOf20yrFiles <- "/Volumes/PassportMac/ISIMIP/cmip6/climate3b/climate3bCombined"

yearRange20Year <- 19
sspChoices <- c("ssp126", "ssp585")
startYearChoices20yr <-  c(2041, 2081)
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)

#test values
k <- "ssp126"
l <- 2041
j <- "tas"
m <- "ukesm1-0-ll"

climateVars <- c( "tasmin", "tasmax", "tas", "pr", "hurs", "rsds", "sfcwind", "ps") # "tasmax", "tasmin")
modelChoices <- c("UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
completedFilesList <- list.files("/Volumes/PassportMac/ISIMIP/cmip6/climate3bCombined/dailyMn_20Yr/")
# daily mean
f_daily_mean <- function() {
  for (j in climateVars) {
    for (m in modelChoices.lower) {
      print(paste0("climate var: ", j, ", model: ", m, ", ssp choice: ", k, ", start year: ", l))
      fileName_in <- paste0("/Volumes/PassportMac/ISIMIP/cmip6/climate3bCombined/", m, "_", k,  "_",  j, "_", yearSpan, ".tif")
      print(paste0("fileName in: ", fileName_in))
      fileName_out <- paste0("/Volumes/PassportMac/ISIMIP/cmip6/climate3bCombined/dailyMn_20Yr/dailyMn_20Yr_", m, "_", k,  "_",  j, "_", yearSpan, ".tif")
      fileName_out %in% completedFilesList
      if (!fileName_out %in% completedFilesList) {
        print(paste0("fileName out: ", fileName_out))
        r <- rast(fileName_in)
        print(r)
        print(system.time(r.mean <- tapp(r, indices_day, fun = mean, na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
      }
    }
  }
  
}
for (k in sspChoices) {
  for (l in startYearChoices20yr) {
    yearSpan <- paste0(l, "_", l + yearRange20Year)
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange20Year, "-12-31")
    indices_day <- seq(as.Date(startDate), as.Date(endDate), 1)
    #    indices <- paste0("X", as.character(indices))
    indices_day <- format(as.Date(indices_day, format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
    indices_day <- as.numeric(indices_day)
    f_daily_mean()
  }
}

k <- "historical" 
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange20Year)
startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange20Year, "-12-31")
indices_day <- seq(as.Date(startDate), as.Date(endDate), 1)
#    indices <- paste0("X", as.character(indices))
indices_day <- format(as.Date(indices_day, format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
indices_day <- as.numeric(indices_day)
f_daily_mean()

# ensemble daily mean

readRast <- function(m) {
  fileName_in <- paste0("/Volumes/PassportMac/ISIMIP/cmip6/climate3bCombined/", m, "_", k,  "_",  j, "_", yearSpan, ".tif")
  #mri-esm2-0_ssp585_hurs_2041_2060.tif
  r <- rast(fileName_in)
}
f_ensemble_daily_mean <- function() {
  for (j in climateVars) {
    x <- lapply(modelChoices.lower, readRast)
    r <- rast(x)
    print(r)
    system.time(r.mean <- tapp(r, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, wopt = woptList))
    #    r.cv <- tapp(r, 1:12, fun = raster::cv)
    names(r.mean) <- month.abb
    fileName_out <- paste0("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/ensembleMn_mnthMn_10Yr_", k,  "_",  j, "_", yearSpan, ".tif")
    print(system.time(writeRaster(r.mean, fileName_out, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
  }
}

for (k in sspChoices) {
  for (l in startYearChoices20yr) {
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange20Year, "-12-31")
    indices_day <- seq(as.Date(startDate), as.Date(endDate), 1)
    #    indices <- paste0("X", as.character(indices))
    yearSpan <- paste0(l, "_", l + yearRange20Year)
    f_ensemble_daily_mean()
  }
}

# daily mean ensemble, historical -----
k <- "historical"
l <- 1991
startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange20Year, "-12-31")
indices_day <- seq(as.Date(startDate), as.Date(endDate), 1)
#    indices <- paste0("X", as.character(indices))
yearSpan <- paste0(l, "_", l + yearRange20Year)
f_ensemble_daily_mean()



#    # fix monthly mean file names
#    files_in <- list.files("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year")
#    files_out <- paste0("mnthMn_10Yr_", files_in)
#    files_in <- paste0("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/", files_in)
#    files_out <- paste0("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/", files_out)
#    
# for (i in 1:length(files_in)) {
#   file.rename(from = files_in[i], to = files_out[i])
# }

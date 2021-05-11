# this script converts the units of nc tmin, tmax, and pr and all varibles to geotifs
#source("R/globallyUsed.R")
#library(data.table)
#library(raster)
library(terra)
terraOptions(memfrac = 2, progress = 10, tempdir =  "data/ISIMIP", verbose = FALSE) 
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

yearRange <- 9

dirList <- c( "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "historical") #"observed",  "historical", "IPSL-CM6A-LR"
# dirList <- c("MRI-ESM2-0")

sspList <- c("ssp126", "ssp585")
# Note that ssp370 not included
filesInDir_historical <- list.files("/Volumes/ExtremeSSD3/climate3b/historical", full.names = TRUE, recursive = TRUE)
filesInDir_ssp126 <- list.files("/Volumes/ExtremeSSD3/climate3b/ssp126", full.names = TRUE, recursive = TRUE)
filesInDir_ssp585 <- list.files("/Volumes/ExtremeSSD3/climate3b/ssp585", full.names = TRUE, recursive = TRUE)

filestoKeep <- c(filesInDir_historical, filesInDir_ssp126, filesInDir_ssp585)

# make file names more compact
renameFile <- function(inNCfile) {
  inNCfile <- gsub("r1i1p1f1_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("r1i1p1f2_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("global_daily_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("/Volumes/ExtremeSSD3/climate3b/", "/Volumes/PassportMac/ISIMIP/cmip6/climate3b/", inNCfile, fixed = TRUE)
  for (i in paste0(dirList, "/")) {
    inNCfile <- gsub(i, "", inNCfile)
  }
  for (i in paste0(sspList, "/")) {
    inNCfile <- gsub(i, "", inNCfile)
  }
  inNCfile <- gsub("/Volumes/ExtremeSSD3/climate3b/", "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/", inNCfile, fixed = TRUE) # for historical files
  fileName_out <- gsub(".nc", ".tif", inNCfile, fixed = TRUE)
  return(fileName_out)
}

varListComplete <- c("_tasmax_", "_tasmin_", "_tas_", "_pr_", "_hurs_", "_huss_", "_rlds_", "_rsds_", "_sfcwind_", "_ps_", "_prsn_") 
varsToKeep <- varListComplete
#varsToKeep <- c( "_tasmin_", "_tasmax_")
varsToRemove <- varListComplete[!varListComplete %in% varsToKeep]

for (cntr in varsToRemove) {
  filestoKeep <- filestoKeep[!grepl(cntr, filestoKeep, fixed = TRUE)] # keep only file names with ESM names in dirList
}

earlyYearsToRemove <- seq(from = 1850, to = 1980, by = 1)
yearToRemove <- c(earlyYearsToRemove, 1951, 1961, 1971, 1981, 2014, 2015, 2021, 2031, 2061, 2071)
yearToRemove <- c(earlyYearsToRemove, 1951, 1961, 1971, 1991, 2001, 2014, 2015, 2021, 2041, 2051, 2061, 2081, 2091) # to add the 10 year files needed for 30 year combos
for (cntr in yearToRemove) {
  filestoKeep <- filestoKeep[!grepl(cntr, filestoKeep, fixed = TRUE)] # keep only file names with ESM names in dirList
}

#filestoKeep <- data.table(v1 = character())
filestoKeep <- filestoKeep[!grepl("aux.xml", filestoKeep, fixed = TRUE)]
filestoKeep <- filestoKeep[!grepl(".tif", filestoKeep, fixed = TRUE)]
filestoKeep <- unique(filestoKeep)

for (j in 1:length(filestoKeep)) {
  gc()
  fileName_in <- filestoKeep[j]
  print(paste0("fileName_in: ", fileName_in))
  startYear <- substr(fileName_in, (nchar(fileName_in))-11 ,nchar(fileName_in)-8)
  fileName_out <- renameFile(fileName_in)
  #   if (!fileName_out %in% filesInDir_out) {
  rastIn <- rast(fileName_in)
  print(rastIn)
  
  # pr and the temp files need to be converted 
  if (grepl("_pr_", fileName_in)) {
    print(system.time(rastIn <- rastIn * 86400)); flush.console()
  }
  if (grepl("_tasmax_", fileName_in)) {
    print(system.time(rastIn <- rastIn - 273.15)); flush.console()
  }     
  if (grepl("_tasmin_", fileName_in)) {
    print(system.time(rastIn <- rastIn - 273.15)); flush.console()
  }     
  if (grepl("_tas_", fileName_in)) {
    print(system.time(rastIn <- rastIn - 273.15)); flush.console()
  }
  
  startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
  indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  indices <- paste0("X", as.character(indices))
  
  names(rastIn) <- indices
  setMinMax(rastIn)
  print(rastIn)
  print(paste0("max rastIn: ", max(minmax(rastIn)), ", min rastIn: ", min(minmax(rastIn)), ", fileName_out :", fileName_out))
  print(system.time(writeRaster(rastIn, fileName_out,  overwrite = TRUE,  wopt= woptList))); flush.console()
  rastIn <- NULL
}


# this script converts the units of nc tmin, tmax, and pr and all varibles to geotifs
#source("R/globallyUsed.R")
library(data.table)
#library(raster)
library(terra)
woptList <- list(gdal=c("COMPRESS=LZW"))

dirList <- c( "MRI-ESM2-0", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL") #"observed",  "historical",
dirList <- "UKESM1-0-LL"
dirList <- c( "historical")

# # only needed when setting up directories
# dirs.needed_observed <- paste0("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/observed/", dirList)
# dirs.needed_historical <- paste0("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/", dirList)
# dirs.needed_ssp585 <- paste0("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/ssp585/", dirList)
# dirs.needed_ssp126 <- paste0("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/ssp126/", dirList)
# existingDirs <- list.dirs("/Volumes/ExtremeSSD2/climate_land_only/")
# missingDirs_observed <- dirs.needed_observed[!dirs.needed_observed %in% existingDirs]
# missingDirs_historical <- dirs.needed_historical[!dirs.needed_historical %in% existingDirs]
# missingDirs_ssp126 <- dirs.needed_ssp126[!dirs.needed_ssp126 %in% existingDirs]
# missingDirs_ssp585 <- dirs.needed_ssp585[!dirs.needed_ssp585 %in% existingDirs]
# missingDirs <- c(missingDirs_observed, missingDirs_ssp126, missingDirs_ssp585, missingDirs_historical)
# missingDirs <- missingDirs_historical
# for (i in missingDirs) dir.create(i, recursive = TRUE)

filesInDir_observed <- list.files("/Volumes/ExtremeSSD2/climate_land_only/climate3b/observed/", full.names = TRUE, recursive = TRUE)
filesInDir_historical <- list.files("/Volumes/ExtremeSSD2/climate_land_only/climate3b/historical/", full.names = TRUE, recursive = TRUE)
filesInDir_ssp126 <- list.files("/Volumes/ExtremeSSD2/climate_land_only/climate3b/ssp126/", full.names = TRUE, recursive = TRUE)
filesInDir_ssp585 <- list.files("/Volumes/ExtremeSSD2/climate_land_only/climate3b/ssp585/", full.names = TRUE, recursive = TRUE)
filesInDir <- c(filesInDir_historical, filesInDir_observed, filesInDir_ssp126, filesInDir_ssp585)
filesInDir <- gsub("//", "/", filesInDir)
renameFile <- function(inNCfile) {
  inNCfile <- gsub("r1i1p1f1_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("r1i1p1f2_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("/Volumes/ExtremeSSD2/climate_land_only/climate3b", "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected", inNCfile, fixed = TRUE)
  outfile <- gsub(".nc", ".tif", inNCfile, fixed = TRUE)
  return(outfile)
}

varListComplete <- c("_tasmax_", "_tasmin_", "_pr_", "_hurs_", "huss", "rlds", "_rsds_", "_sfcwind_", "_ps_", "_tas_", "_tave_", "prsn")
varsToKeep <- c(  "_tasmax_", "_tasmin_", "tave")
varsToKeep <- c("_hurs_")
varsToRemove <- varListComplete[!varListComplete %in% varsToKeep]

for (cntr in dirList) {
  filestoKeep <- filesInDir[grepl(cntr, filesInDir, fixed = TRUE)] # keep only file names with climate vars included in varsToKeep
}
for (cntr in varsToRemove) {
  filestoKeep <- filestoKeep[!grepl(cntr, filestoKeep, fixed = TRUE)] # keep only file names with ESM names in dirList
}
yearToRemove <- c(1951, 1961, 1971, 1981, 2016)
for (cntr in yearToRemove) {
  filestoKeep <- filestoKeep[!grepl(cntr, filestoKeep, fixed = TRUE)] # keep only file names with ESM names in dirList
}

# for (cntr in c("ukesm", "ipsl")) { # remove these file names since they are already done for varsToKeep
#   filestoKeep <- filestoKeep[!grepl(cntr, filestoKeep, fixed = TRUE)] # keep only file names with ESM names in dirList
# }

# keep just ssp585
filestoKeep <- filestoKeep[grepl("ssp585", filestoKeep, fixed = TRUE)] # keep only file names with ESM names in dirList

#filestoKeep <- data.table(v1 = character())
for (i in varsToKeep) {
  print(i)
  
  if (i %in% "_pr_") {
    for (j in 1:length(filestoKeep)) {
      gc()
      inFile <- filestoKeep[j]
      print(paste0("infile: ", inFile))
      startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
      yearRange <- 9
      outFile <- renameFile(inFile)
      print(paste0("outFile: ", outFile))
      rastIn <- rast(inFile)
  #    rastIn <- setMinMax(rastIn) # add min max values
      print(rastIn)
      #rename raster names
      startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %m is month of the year
      indices <- as.numeric(indices)
      
      #     system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
      print(system.time(rastOut <- rastIn * 86400)); flush.console()
      print(system.time(writeRaster(rastOut, outFile,  overwrite = TRUE, wopt= woptList))); flush.console()
      
    }
  }
  if (i %in% c("_tasmax_", "_tasmin_", "_tas_")) {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
      yearRange <- 9
      outFile <- renameFile(inFile)
      print(paste0("outFile: ", outFile))
      rastIn <- rast(inFile)
      # rastIn <- setMinMax(rastIn) # add min max values
      rastIn
      #rename raster names
      startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
      indices <- as.numeric(indices)
      
      print(system.time(rastOut <- rastIn - 273.15)); flush.console()
      print(rastOut)
      print(system.time(writeRaster(rastOut, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
    }
  }
  if (i %in% c("_hurs_", "_huss_", "_rsds_", "_sfcwind_", "_ps")) {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
      yearRange <- 9
      outFile <- renameFile(inFile)
      print(paste0("variable: ", i, ", outfile: ", outFile))
      rastIn <- rast(inFile)
      #      rastIn <- setMinMax(rastIn) # add min max values
      print(rastIn)
      
      #rename raster names
      startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
      indices <- as.numeric(indices)
      #      system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
      print(paste0("outfile name: ", outFile))
      print(system.time(writeRaster(rastIn, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
    }
  }
}

test <- rast("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/ssp126/GFDL-ESM4/gfdl-esm4_ssp126_tasmin_global_daily_2021_2030.tif")
test2 <- rast("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/ssp126/GFDL-ESM4/gfdl-esm4_ssp126_tasmin_global_daily_2031_2040.tif")
test3 <- c(test, test2)

# # Now do the observed data
# 
# filesInDir <- list.files("/Volumes/ExtremeSSD2/climate_land_only/climate3b/observed", full.names = TRUE, recursive = TRUE)
# filesInDir <- gsub("//", "/", filesInDir)
# 
# renameFileObserved <- function(inNCfile){
#   inNCfile <- gsub("observed", "unitsCorrected/observed", inNCfile, fixed = TRUE)
#   outfile <- gsub(".nc", ".tif", inNCfile, fixed = TRUE)
#   return(outfile)
# }
# 
# for (i in varsToKeep) {
#   print(i)
#   filestoKeep <- filesInDir[grepl(i, filesInDir, fixed = TRUE)]
#   #  filestoKeep <- head(filestoKeep)
#   #  filestoKeep <- paste0(pathPrefix, "/", filestoKeep)
#   
#   starttime <- Sys.time()
#   if (i %in% "_pr_") {
#     for (j in 1:length(filestoKeep)) {
#       inFile <- filestoKeep[j]
#       startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
#       yearRange <- 9
#       outFile <- renameFileObserved(filestoKeep[j])
#       print(outFile)
#       rastIn <- rast(inFile)
#       print(rastIn)
#       
#       #rename raster names
#       startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
#       indices <- seq(as.Date(startDate), as.Date(endDate), 1)
#       indices <- paste0("X", as.character(indices))
#       indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
#       indices <- as.numeric(indices)
#       
#       #      system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
#       print(system.time(rastOut <- rastIn * 86400)); flush.console()
#       print(system.time(writeRaster(rastOut, outFile,  overwrite = TRUE, wopt= woptList))); flush.console()
#       
#       # cdoCommandMult <- paste("cdo -z zip_6 setunit,'mm/day' -mulc,86400 ", inFile, outFile)
#       # system(cdoCommandMult)
#       # # temp.r <- readAll(rastfilestoKeep[j]))
#       # temp.r@data@unit <- "mm/day"
#       # temp.r <- temp.r * 86400
#       #    print(paste0("writing out ", outfiles[j]))
#       #   writeRaster(temp.r, filename = outfiles[j], format = "GTiff", overwrite = TRUE)
#       # }
#     }
#   }
#   if (i %in% c("_tasmax_", "_tasmin_")) {
#     for (j in 1:length(filestoKeep)) {
#       inFile <- filestoKeep[j]
#       startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
#       yearRange <- 9
#       outFile <- renameFileObserved(filestoKeep[j])
#       print(outFile)
#       rastIn <- rast(inFile)
#       
#       #rename raster names
#       startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
#       indices <- seq(as.Date(startDate), as.Date(endDate), 1)
#       indices <- paste0("X", as.character(indices))
#       indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
#       indices <- as.numeric(indices)
#       
#       #      system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
#       print(system.time(rastOut <- rastIn - 273.15)); flush.console()
#       print(system.time(writeRaster(rastOut, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
#       
#       #   cdoCommandMult <- paste("cdo -z zip_6 setunit,'mm/day' -addc,-273.15 ",  inFile, outFile)
#       #   system(cdoCommandMult)
#       # 
#     }
#   }
#   
#   if (i %in% c("_hurs_", "_rsds_", "_sfcwind_")) {
#     for (j in 1:length(filestoKeep)) {
#       inFile <- filestoKeep[j]
#       startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
#       yearRange <- 9
#       outFile <- renameFileObserved(filestoKeep[j])
#       print(outFile)
#       rastIn <- rast(inFile)
#       
#       #rename raster names
#       startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
#       indices <- seq(as.Date(startDate), as.Date(endDate), 1)
#       indices <- paste0("X", as.character(indices))
#       indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
#       indices <- as.numeric(indices)
#       
#       #     system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
#       print(system.time(rastOut <- rastIn)); flush.console()
#       print(paste0("outfile name: ", outFile))
#       
#       print(system.time(writeRaster(rastOut, outFile,  overwrite = TRUE, wopt= woptList))); flush.console()
#     }
#   }
# }

# generate tave - added July 30, 2020


# cdo -setunit="degC" -addc,-273.15 <infile> <outfile>
# cdo -setunit="mm/day" -mulc,86400 <infile> <outfile>

# cdo -setunit="degC" j -addc,-273.15 -z szip outfile

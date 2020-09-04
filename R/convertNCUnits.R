# this script converts the units of nc tmin, tmax, and pr
#source("R/globallyUsed.R")
library(data.table)
#library(raster)
library(terra)
woptList <- list(gdal=c("COMPRESS=LZW"))

dirList <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
#dirs.needed <- paste0("data-raw/ISIMIP/cmip6/unitsCorrected/ssp585/", dirList)
dirs.needed <- paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/ssp126/", dirList)
existingDirs <- list.dirs("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/ssp126/")
missingDirs <- dirs.needed[!dirs.needed %in% existingDirs]
for (i in missingDirs) dir.create(i, recursive = TRUE)

#filesInDir <- list.files("data-raw/ISIMIP/cmip6/original/ssp585", full.names = TRUE, recursive = TRUE)
#filesInDir <- list.files("/Volumes/PassportMac/ISIMIP/cmip6/ssp585", full.names = TRUE, recursive = TRUE)
filesInDir <- list.files("/Volumes/PassportMac/ISIMIP/cmip6/ssp585", full.names = TRUE, recursive = TRUE)
 filesInDir <- list.files("/Volumes/PassportMac/ISIMIP/cmip6/ssp126", full.names = TRUE, recursive = TRUE)
filesInDir <- gsub("//", "/", filesInDir)
renameFile <- function(inNCfile){
  inNCfile <- gsub("r1i1p1f1_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("r1i1p1f2_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("/Volumes/PassportMac/ISIMIP/cmip6/", "/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/", inNCfile, fixed = TRUE)
  outfile <- gsub(".nc", ".tif", inNCfile, fixed = TRUE)
  return(outfile)
}

varsToKeep <- c("_tasmax_", "_tasmin_", "_pr_", "_hurs_", "_rsds_", "_sfcwind_")
varsToKeep <- c( "_sfcwind_")

#filestoKeep <- data.table(v1 = character())
for (i in varsToKeep) {
  print(i)
  filestoKeep <- filesInDir[grepl(i, filesInDir, fixed = TRUE)]
  if (i %in% "_pr_") {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
      yearRange <- 9
      outFile <- renameFile(filestoKeep[j])
      print(outFile)
      rastIn <- rast(inFile)
      
      #rename raster names
      startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
      indices <- as.numeric(indices)
      
      system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
      print(system.time(rastOut <- rastIn_mean * 86400)); flush.console()
      print(system.time(writeRaster(rastOut, outFile,  overwrite = TRUE, wopt= woptList))); flush.console()
      
      # cdoCommandMult <- paste("cdo -z zip_6 setunit,'mm/day' -mulc,86400 ", inFile, outFile)
      # system(cdoCommandMult)
      # temp.r <- readAll(rastfilestoKeep[j]))
      # temp.r@data@unit <- "mm/day"
      # temp.r <- temp.r * 86400
      #    print(paste0("writing out ", outfiles[j]))
      #   writeRaster(temp.r, filename = outfiles[j], format = "GTiff", overwrite = TRUE)
      # }
    }
  }
  if (i %in% c("_tasmax_", "_tasmin_")) {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
      yearRange <- 9
      outFile <- renameFile(filestoKeep[j])
      print(outFile)
      rastIn <- rast(inFile)
      
      #rename raster names
      startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
      indices <- as.numeric(indices)
      
      system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
      print(system.time(rastOut <- rastIn_mean - 273.15)); flush.console()
      print(system.time(writeRaster(rastOut, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
      #   cdoCommandMult <- paste("cdo -z zip_6 setunit,'degC' -addc,-273.15 ",  inFile, outFile)
      #   system(cdoCommandMult)
      # }
    }
  }
  if (i %in% c("_hurs_","_rsds_", "_sfcwind_")) {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
      yearRange <- 9
      outFile <- renameFile(filestoKeep[j])
      print(paste0("variable: ", i, ", outfile: ", outFile))
      rastIn <- rast(inFile)
      
      #rename raster names
      startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
      indices <- as.numeric(indices)
      system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
      print(paste0("outfile name: ", outFile))
      print(system.time(writeRaster(rastIn_mean, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
    }
  }
}



test <- rast("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/ssp126/GFDL-ESM4/gfdl-esm4_ssp126_tasmin_global_daily_2021_2030.tif")
test2 <- rast("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/ssp126/GFDL-ESM4/gfdl-esm4_ssp126_tasmin_global_daily_2031_2040.tif")
test3 <- c(test, test2)

# Now do the observed data

filesInDir <- list.files("/Volumes/PassportMac/ISIMIP/cmip6/observed", full.names = TRUE, recursive = TRUE)
filesInDir <- gsub("//", "/", filesInDir)

renameFileObserved <- function(inNCfile){
  inNCfile <- gsub("observed", "unitsCorrected/observed", inNCfile, fixed = TRUE)
  outfile <- gsub(".nc", ".tif", inNCfile, fixed = TRUE)
  return(outfile)
}

for (i in varsToKeep) {
  print(i)
  filestoKeep <- filesInDir[grepl(i, filesInDir, fixed = TRUE)]
  #  filestoKeep <- head(filestoKeep)
  #  filestoKeep <- paste0(pathPrefix, "/", filestoKeep)
  
  starttime <- Sys.time()
  if (i %in% "_pr_") {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
      yearRange <- 9
      outFile <- renameFile(filestoKeep[j])
      print(outFile)
      rastIn <- rast(inFile)
      
      #rename raster names
      startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
      indices <- as.numeric(indices)
      
      system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
      print(system.time(rastOut <- rastIn * 86400)); flush.console()
      print(system.time(writeRaster(rastOut, outFile,  overwrite = TRUE, wopt= woptList))); flush.console()
      
      
      # cdoCommandMult <- paste("cdo -z zip_6 setunit,'mm/day' -mulc,86400 ", inFile, outFile)
      # system(cdoCommandMult)
      # # temp.r <- readAll(rastfilestoKeep[j]))
      # temp.r@data@unit <- "mm/day"
      # temp.r <- temp.r * 86400
      #    print(paste0("writing out ", outfiles[j]))
      #   writeRaster(temp.r, filename = outfiles[j], format = "GTiff", overwrite = TRUE)
      # }
    }
  }
  if (i %in% c("_tasmax_", "_tasmin_")) {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
      yearRange <- 9
      outFile <- renameFile(filestoKeep[j])
      print(outFile)
      rastIn <- rast(inFile)
      
      #rename raster names
      startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
      indices <- as.numeric(indices)
      
      system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
      print(system.time(rastOut <- rastIn - 273.15)); flush.console()
      print(system.time(writeRaster(rastOut, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
      
      #   cdoCommandMult <- paste("cdo -z zip_6 setunit,'mm/day' -addc,-273.15 ",  inFile, outFile)
      #   system(cdoCommandMult)
      # 
    }
  }
  
  if (i %in% c("_hurs_", "_rsds_", "_sfcwind_")) {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      startYear <- substr(inFile, (nchar(inFile))-11 ,nchar(inFile)-8)
      yearRange <- 9
      outFile <- renameFile(filestoKeep[j])
      print(outFile)
      rastIn <- rast(inFile)
      
      #rename raster names
      startDate <- paste0(startYear, "-01-01"); endDate <- paste0(as.numeric(startYear) + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      indices <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%m") # %n is month of the year
      indices <- as.numeric(indices)
      
      system.time(rastIn_mean <- tapp(rastIn, indices, fun = mean))
      print(system.time(rastOut <- rastIn)); flush.console()
      print(paste0("outfile name: ", outFile))
      
      print(system.time(writeRaster(rastOut, outFile,  overwrite = TRUE, wopt= woptList))); flush.console()
    }
  }
}

# generate tave - added July 30, 2020


# cdo -setunit="degC" -addc,-273.15 <infile> <outfile>
# cdo -setunit="mm/day" -mulc,86400 <infile> <outfile>

# cdo -setunit="degC" j -addc,-273.15 -z szip outfile

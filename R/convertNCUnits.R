# this script converts the units of nc tmin, tmax, and pr
#source("R/globallyUsed.R")
library(data.table)
#library(raster)
library(terra)

pathPrefix <- getwd()

dirList <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
dirs.needed <- paste0("data-raw/ISIMIP/cmip6/unitsCorrected/ssp585/", dirList)
existingDirs <- list.dirs("data-raw/ISIMIP/cmip6/unitsCorrected/ssp585")
missingDirs <- dirs.needed[!dirs.needed %in% existingDirs]
for (i in missingDirs) dir.create(i)

#filesInDir <- list.files("data-raw/ISIMIP/cmip6/original/ssp585", full.names = TRUE, recursive = TRUE)
filesInDir <- list.files("/Volumes/PassportMac/ISIMIP/cmip6/ssp585", full.names = TRUE, recursive = TRUE)
filesInDir <- gsub("//", "/", filesInDir)
renameFile <- function(inNCfile){
  inNCfile <- gsub("r1i1p1f1_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("r1i1p1f2_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("/Volumes/PassportMac/ISIMIP/cmip6/", "data-raw/ISIMIP/cmip6/unitsCorrected/", inNCfile, fixed = TRUE)
  outfile <- gsub(".nc", ".tif", inNCfile, fixed = TRUE)
  return(outfile)
}
varsToKeep <- c("_tasmax_", "_tasmin_", "_pr_", "_hurs_")

#filestoKeep <- data.table(v1 = character())
woptList <- list(gdal=c("COMPRESS=LZW"))
for (i in varsToKeep) {
  print(i)
  filestoKeep <- filesInDir[grepl(i, filesInDir, fixed = TRUE)]
  i#  filestoKeep <- head(filestoKeep)
  #  filestoKeep <- paste0(pathPrefix, "/", filestoKeep)
 # starttime <- Sys.time()
  if (i %in% "_pr_") {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      outFile <- renameFile(filestoKeep[j])
      print(outFile)
      rastIn <- rast(inFile)
      print(system.time(rastOut <- rastIn * 86400)); flush.console()
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
      outFile <- renameFile(filestoKeep[j])
      print(outFile)
      rastIn <- rast(inFile)
      print(system.time(rastOut <- rastIn - 273.15)); flush.console()
      print(system.time(writeRaster(rastOut, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
      #   cdoCommandMult <- paste("cdo -z zip_6 setunit,'degC' -addc,-273.15 ",  inFile, outFile)
      #   system(cdoCommandMult)
      # }
      
      if (i %in% c("_hurs_")) {
        for (j in 1:length(filestoKeep)) {
          inFile <- filestoKeep[j]
          rastIn <- rast(inFile)
          print(system.time(rastOut <- rastIn)); flush.console()
          outFile <- renameFile(filestoKeep[j])
          print(system.time(writeRaster(rastOut, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
        }
      }
    }
  }
}

# Now do the observed data

filesInDir <- list.files("data-raw/ISIMIP/cmip6/observed", full.names = TRUE, recursive = TRUE)
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
      outFile <- renameFileObserved(filestoKeep[j])
      rastIn <- rast(inFile)
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
      outFile <- renameFileObserved(filestoKeep[j])
      rastIn <- rast(inFile)
      print(system.time(rastOut <- rastIn - 273.15)); flush.console()
      print(system.time(writeRaster(rastOut, outFile, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
      
    #   cdoCommandMult <- paste("cdo -z zip_6 setunit,'mm/day' -addc,-273.15 ",  inFile, outFile)
    #   system(cdoCommandMult)
    # 
    }
  }
  
  if (i %in% c("_hurs_")) {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      outFile <- renameFileObserved(filestoKeep[j])
      rastIn <- rast(inFile)
      print(system.time(rastOut <- rastIn)); flush.console()
      print(system.time(writeRaster(rastOut, outFile,  overwrite = TRUE, wopt= woptList))); flush.console()
    }
  }
}

# generate tave - added July 30, 2020

  
  # cdo -setunit="degC" -addc,-273.15 <infile> <outfile>
  # cdo -setunit="mm/day" -mulc,86400 <infile> <outfile>
  
  # cdo -setunit="degC" j -addc,-273.15 -z szip outfile
  
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

filesInDir <- list.files("data-raw/ISIMIP/cmip6/original/ssp585", full.names = TRUE, recursive = TRUE)
filesInDir <- gsub("//", "/", filesInDir)
renameFile <- function(inNCfile){
  inNCfile <- gsub("r1i1p1f1_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("r1i1p1f2_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("original", "unitsCorrected", inNCfile, fixed = TRUE)
  #outfiles <- gsub(".nc", ".tif", outfiles, fixed = TRUE)
  inNCfile <- paste0(inNCfile)
  return(inNCfile)
}
varsToKeep <- c("_tasmax_", "_tasmin_", "_pr_")

#filestoKeep <- data.table(v1 = character())
for (i in varsToKeep) {
  print(i)
  filestoKeep <- filesInDir[grepl(i, filesInDir, fixed = TRUE)]
  #  filestoKeep <- head(filestoKeep)
  #  filestoKeep <- paste0(pathPrefix, "/", filestoKeep)
  
  starttime <- Sys.time()
  if (i %in% "_pr_") {
    for (j in 1:length(filestoKeep)) {
      inFile <- filestoKeep[j]
      outFile <- renameFile(filestoKeep[j])
      cdoCommandMult <- paste("cdo -z zip_6 setunit,'mm/day' -mulc,86400 ", inFile, outFile)
      system(cdoCommandMult)
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
      
      cdoCommandMult <- paste("cdo -z zip_6 setunit,'mm/day' -addc,-273.15 ",  inFile, outFile)
      system(cdoCommandMult)
    }
    
    # temp.r <- readAll(rastfilestoKeep[j]))
    # temp.r@data@unit <- "C"
    # temp.r <- temp.r - 273.15
    # print(paste0("writing out ", outfiles[j]))
    # writeRaster(temp.r, filename = outfiles[j], format = "GTiff", overwrite = TRUE)
    # 
  }
}
endtime <- Sys.time()
endtime - starttime

# Now do the observed data

filesInDir <- list.files("data-raw/ISIMIP/cmip6/observed", full.names = TRUE, recursive = TRUE)
filesInDir <- gsub("//", "/", filesInDir)
renameFileObserved <- function(inNCfile){
  inNCfile <- gsub("observed", "unitsCorrected/observed", inNCfile, fixed = TRUE)
  #outfiles <- gsub(".nc", ".tif", outfiles, fixed = TRUE)
  inNCfile <- paste0(inNCfile)
  return(inNCfile)
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
      cdoCommandMult <- paste("cdo -z zip_6 setunit,'mm/day' -mulc,86400 ", inFile, outFile)
      system(cdoCommandMult)
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
      outFile <- renameFileObserved(filestoKeep[j])
      
      cdoCommandMult <- paste("cdo -z zip_6 setunit,'mm/day' -addc,-273.15 ",  inFile, outFile)
      system(cdoCommandMult)
    }
  }
}

# cdo -setunit="degC" -addc,-273.15 <infile> <outfile>
# cdo -setunit="mm/day" -mulc,86400 <infile> <outfile>

# cdo -setunit="degC" j -addc,-273.15 -z szip outfile

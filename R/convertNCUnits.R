# this script converts the units of nc tmin, tmax, and pr
#source("R/globallyUsed.R")
library(data.table)
library(raster)
library(terra)

dirList <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
dirs.needed <- paste0("/Volumes/ExtremeSSD/ISIMIP/cmip6/unitsCorrected/", dirList)
existingDirs <- list.dirs("/Volumes/ExtremeSSD/ISIMIP/cmip6/unitsCorrected/")
missingDirs <- dirs.needed[!dirs.needed %in% existingDirs]
for (i in missingDirs) dir.create(i)

filesInDir <- list.files("/Volumes/ExtremeSSD/ISIMIP/cmip6/original/ssp585/", full.names = TRUE, recursive = TRUE)
filesInDir <- gsub("//", "/", filesInDir)
outfiles <- filesInDir
outfiles <- gsub("r1i1p1f1_w5e5_", "", outFiles, fixed = TRUE)
outfiles <- gsub("r1i1p1f2_w5e5", "", outFiles, fixed = TRUE)
outfiles <- gsub("original", "unitsCorrected", outFiles, fixed = TRUE)
outfiles <- gsub(".nc", ".tif", outFiles, fixed = TRUE)

varsToKeep <- c("_tasmax_", "_tasmin_", "_pr_")

filestoKeep <- data.table(v1 = character())
for (i in varsToKeep) {
  print(i)
  filestoKeep <- filesInDir[grepl(i, filesInDir, fixed = TRUE)]
  filestoKeep <- head(filestoKeep)
  starttime <- Sys.time()
  if (i %in% "_pr_") {
    for (j in 1:length(filestoKeep)) {
      tempfile1 <- "temp1.nc"
      tempfile2 <- "temp2.nc"
      cdoCommandUnits <- paste("cdo -setunit,'mm/day'", filesInDir[j], tempfile1)
      cdoCommandMult <- paste("cdo -mulc,86400", tempfile1, tempfile2)
      cdoCommandzip <- paste("cdo -z,zip_6", tempfile2, outfiles[j])
      system(cdoCommandUnits)
      system(cdoCommandMult)
      system(cdoCommandzip)
      rm(list(tempfile1, tempfile2))
      
      # temp.r <- readAll(brick(filestoKeep[j]))
      # temp.r@data@unit <- "mm/day"
      # temp.r <- temp.r * 86400
    #    print(paste0("writing out ", outfiles[j]))
    #   writeRaster(temp.r, filename = outfiles[j], format = "GTiff", overwrite = TRUE)
    # }
  }
  if (i %in% c("_tasmax_", "_tasmin_")) {
    for (j in 1:length(filestoKeep)) {
      
        tempfile1 <- "temp1.nc"
        tempfile2 <- "temp2.nc"
        cdoCommandUnits <- paste("cdo -setunit,'degC'", filesInDir[j], tempfile1)
        cdoCommandMult <- paste("cdo -addc,-273.15", tempfile1, tempfile2)
        cdoCommandzip <- paste("cdo -f nc4c -z, zip_6", tempfile2, outfiles[j])
        system(cdoCommandUnits)
        system(cdoCommandMult)
        system(cdoCommandzip)
        rm(list(tempfile1, tempfile2))
      }
      
      # temp.r <- readAll(brick(filestoKeep[j]))
      # temp.r@data@unit <- "C"
      # temp.r <- temp.r - 273.15
      # print(paste0("writing out ", outfiles[j]))
      # writeRaster(temp.r, filename = outfiles[j], format = "GTiff", overwrite = TRUE)
      # 
    }
  }
}
  endtime <- Sys.time()
  endtime - starttime
  
  
  # cdo -setunit="degC" -addc,-273.15 <infile> <outfile>
  # cdo -setunit="mm/day" -mulc,86400 <infile> <outfile>
  
  # cdo -setunit="degC" j -addc,-273.15 -z szip outfile
  
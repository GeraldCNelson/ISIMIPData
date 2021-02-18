#check to see if all needed files are in big files and fix if not
# get list all files that are currently there
library(terra)
terraOptions(memfrac = 2, progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path, memfrac = .9,  

destination <- "/Volumes/ExtremeSSD2/bigFiles/"
sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
climateVarChoices <- c("tasmin", "tasmax", "hurs", "tas")#, "pr",  "rsds", "sfcwind")
climateVarChoices <- c("tasmin")
startYearChoices <-  c(2041, 2081) 
#startYearChoices <-  c(2041) 
startYearChoices_historical <- c(1991)
scenarioChoicesEnsemble <- c("historical", sspChoices)
locOfFiles <- destination

yearRange <- 19
yearRange10 <- 9
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

bigfilesNames <- list.files(destination, full.names = FALSE, recursive = TRUE)

#test values
i <- "UKESM1-0-LL"
k <- "ssp126"
l <- 2081
m <- "tasmin"

missingFiles <- character()
badFiles <- character()
for (m in climateVarChoices) {
  print(m)
  for (k in sspChoices) {
    print(k)
    for (l in startYearChoices) {
      print(l)
      yearSpan <- paste0(l, "_", l + yearRange)
      for (i in modelChoices) {
        modelName.lower <- tolower(i)
        fileNameOnly <- paste0(modelName.lower, "_", k, "_", m, "_global_daily_", yearSpan, ".tif")
        fileName_in <- paste0(locOfFiles,  fileNameOnly)
        if (fileNameOnly %in% bigfilesNames) {
          r <- rast(fileName_in)
          # maxVal <- round(max(r [[1]]@ptr[["range_max"]]), 2)
          # minVal <- round(min(r [[1]]@ptr[["range_min"]]), 2)
          maxVal <- round(max(minmax(r)), 2)
          minVal <- round(min(minmax(r)), 2)
          print(paste0("var: ", m, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName in: ", fileName_in))
          if (m %in% "hurs" & minVal < 1 | maxVal > 100) badFiles <- c(badFiles, fileName_in)
          if (m %in% "tasmax" & maxVal > 80) badFiles <- c(badFiles, fileName_in)
          if (m %in% "tasmin" & maxVal > 80) badFiles <- c(badFiles, fileName_in)
          if (m %in% "tas_" & maxVal > 80) badFiles <- c(badFiles, fileName_in)
        } else {missingFiles <- c(missingFiles, fileName_in)}
      }
    }
  }
  print(" ")
}

# bigfilesNames <- list.files("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/ssp126/", full.names = TRUE, recursive = TRUE)
# bigfilesNames <- bigfilesNames[!grepl("aux.xml", bigfilesNames, fixed = TRUE)]

for (i in bigfilesNames) {
  r <- rast(i)
  maxVal <- round(max(r [[1]]@ptr[["range_max"]]), 2)
  minVal <- round(min(r [[1]]@ptr[["range_min"]]), 2)
  print(paste0("var: ", m, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName in: ", fileName_in))
  if (m %in% "hurs" & minVal < 1 | maxVal > 100) badFiles <- c(badFiles, fileName_in)
  if (m %in% "tasmax" | m %in% "tasmin" & maxVal > 80) {badFiles <- c(badFiles, fileName_in)
  } else {missingFiles <- c(missingFiles, fileName_in)}
  print(" ")
}


# now do historical

for (m in climateVarChoices) {
  print(m)
  for (k in "historical") {
    print(k)
    for (l in 1991) {
      print(l)
      yearSpan <- paste0(l, "_", l + yearRange)
      for (i in modelChoices) {
        modelName.lower <- tolower(i)
        fileNameOnly <- paste0(modelName.lower, "_", k, "_", m, "_global_daily_", yearSpan, ".tif")
        fileName_in <- paste0(locOfFiles,  fileNameOnly)
        if (fileNameOnly %in% bigfilesNames) {
          r <- rast(fileName_in)
          maxVal <- round(max(minmax(r)), 2)
          minVal <- round(min(minmax(r)), 2)
          print(paste0("var: ", m, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName in: ", fileName_in))
          if (m %in% "hurs" & minVal < 1) badFiles <- c(badFiles, fileName_in)
          if (m %in% "tasmax" | m %in% "tasmin" & maxVal > 80) badFiles <- c(badFiles, fileName_in)
        } else {missingFiles <- c(missingFiles, fileName_in)}
      }
    }
    
  }
  print(" ")
}
missing_pr <- missingFiles[grepl("pr", missingFiles, fixed = TRUE)]
missing_hurs <- missingFiles[grepl("hurs", missingFiles, fixed = TRUE)]
missing_tasmax <- missingFiles[grepl("tasmax", missingFiles, fixed = TRUE)]
missing_tasmin <- missingFiles[grepl("tasmin", missingFiles, fixed = TRUE)]
missing_tas <- missingFiles[grepl("tas", missingFiles, fixed = TRUE)]

# missing_tasmin <- missing_tasmin[!missing_tasmin %in% c("/Volumes/PassportMac/bigFiles/ukesm1-0-ll_ssp126_tasmin_global_daily_2041_2060.tif",
#                                                         "/Volumes/PassportMac/bigFiles/mpi-esm1-2-hr_ssp585_tasmin_global_daily_2041_2060.tif")]
# missing_tasmax <- missing_tasmax[!missing_tasmax %in% c("/Volumes/PassportMac/bigFiles/gfdl-esm4_ssp126_tasmax_global_daily_2041_2060.tif")]

# create missing combo files
#for (m in climateVarChoices) {
for (m in c("tas")) {
  for (k in sspChoices) {
    k = "historical"
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      for (i in modelChoices) {
        modelName.lower <- tolower(i)
        fileNameOnly <- paste0(modelName.lower, "_", k, "_", m, "_global_daily_", yearSpan, ".tif")
        fileName_in <- paste0(locOfFiles,  fileNameOnly)
        missings <- paste0("missing_", m)
        if (fileName_in %in% get(missings)) {
          yearSpan1  <- paste0(l, "_", l + yearRange10)
          yearSpan2  <- paste0(l+10, "_", l +10 + yearRange10)
          fileNameR1 <- paste0(modelName.lower, "_", k, "_", m, "_global_daily_", yearSpan1, ".tif")
          fileNameR2 <- paste0(modelName.lower, "_", k, "_", m, "_global_daily_", yearSpan2, ".tif")
          fileloc <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/"
          r1 <- rast(paste0(fileloc, k, "/", i, "/", fileNameR1))
          r2 <- rast(paste0(fileloc, k, "/", i, "/", fileNameR2))
          rout <- c(r1, r2)
          print(rout)
          # something wrong with the date calulations or file length
          #startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
          # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
          # indices <- paste0("X", as.character(indices))
          # names(rout) <- indices
          fileName_out <- paste0(locOfFiles, fileNameOnly)
          print(paste0("fileName_out: ", fileName_out))
          print(system.time(writeRaster(rout, fileName_out, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
          print(paste0("Done with ", fileName_out))
          rout <- NULL
        }
      }
    }
  }
}

#now do repairs. Read in original nc files, convert, and write to unitscorrected

renameFile <- function(inNCfile) {
  inNCfile <- gsub("r1i1p1f1_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("r1i1p1f2_w5e5_", "", inNCfile, fixed = TRUE)
  inNCfile <- gsub("/Volumes/PassportMac/ISIMIP/cmip6/climate3b", "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected", inNCfile, fixed = TRUE)
  inNCfile <- gsub("/Volumes/ExtremeSSD2/climate_land_only/climate3b", "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected", inNCfile, fixed = TRUE) # for historical files
  outfile <- gsub(".nc", ".tif", inNCfile, fixed = TRUE)
  return(outfile)
}
for (m in c( "hurs")) {
  # for (k in sspChoices) {
  for (k in "ssp126") {
    #   for (l in startYearChoices) {
    for (l in 2081) {
      yearSpan <- paste0(l, "_", l + yearRange)
      for (i in modelChoices) {
        modelName.lower <- tolower(i)
        fileNameOnly <- paste0(modelName.lower, "_", k, "_", m, "_global_daily_", yearSpan, ".tif")
        fileName_in <- paste0(locOfFiles,  fileNameOnly)
        
        #        if (fileName_in %in% badFiles) {
        if (fileName_in %in% "/Volumes/PassportMac/bigFiles/mri-esm2-0_ssp126_hurs_global_daily_2081_2100.tif") {
          yearSpan1  <- paste0(l, "_", l + yearRange10)
          yearSpan2  <- paste0(l+10, "_", l +10 + yearRange10)
          fillerText <- "_r1i1p1f1_w5e5_"
          if (i %in% "UKESM1-0-LL") fillerText <- "_r1i1p1f2_w5e5_"
          fileNameR1 <- paste0(modelName.lower, fillerText, k, "_", m, "_global_daily_", yearSpan1, ".nc")
          fileNameR2 <- paste0(modelName.lower, fillerText, k, "_", m, "_global_daily_", yearSpan2, ".nc")
          #         fileloc <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/"
          fileloc <- "/Volumes/PassportMac/ISIMIP/cmip6/climate3b/"
          r1 <- rast(paste0(fileloc, k, "/", i, "/", fileNameR1))
          r2 <- rast(paste0(fileloc, k, "/", i, "/", fileNameR2))
          if (m %in% c("tasmax", "tasmin")) {
            print(system.time(r1 <- r1 - 273.15)); flush.console()
            print(system.time(r2 <- r2 - 273.15)); flush.console()
          }
          
          if (m %in% c("pr")) {
            print(system.time(r1 <- r1  * 86400)); flush.console()
            print(system.time(r2 <- r2  * 86400)); flush.console()
          }
          
          outFile1 <- renameFile(paste0(fileloc, k, "/", i, "/", fileNameR1))
          outFile2 <- renameFile(paste0(fileloc, k, "/", i, "/", fileNameR2))
          #
          print(system.time(writeRaster(r1, outFile1, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
          print(paste0("Done with ", outFile1))
          print(system.time(writeRaster(r2, outFile1, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
          print(paste0("Done with ", outFile2))
          unlink(fileName_in)
          r1 <- r2 <- NULL
        }
      }
    }
  }
}

library(terra)
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

terraOptions(memfrac = 2, progress = 5, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path, memfrac = .9,  
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

for (m in modelChoices) {
  yearRange <- 19
  yearRange10 <- 9
  m <- "UKESM1-0-LL"
  k = "ssp126"
  startYear <- 2041
  climVar <- "tasmin"
  yearSpan1  <- paste0(startYear, "_", startYear + yearRange10)
  yearSpan2  <- paste0(startYear+10, "_", startYear +10 + yearRange10)
  yearSpanFinal <- paste0(startYear, "_", startYear + yearRange)
  m_lower <- tolower(m)
  r1_name <- paste0("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/", k, "/", m, "/",  m_lower, "_", k, "_", climVar, "_global_daily_", yearSpan1, ".tif")
  r2_name <- paste0("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/", k, "/", m, "/",  m_lower, "_", k, "_", climVar, "_global_daily_", yearSpan2, ".tif")
  r1 <- rast(r1_name)
  r2 <- rast(r2_name)
   print(paste0("max r1: ", max(minmax(r1)), ", min r1: ", min(minmax(r1))))
   print(paste0("max r2: ", max(minmax(r2)), ", min r2: ", min(minmax(r2))))
  
  # system.time(r1 <- r1 * 1)
  # r2 <- r2 * 1
  print(system.time(r_out <- c(r1, r2)))
  print(system.time(r_out <- r_out + 0))
  #  r1 <- r2 <- NULL
  gc()
  print(Sys.time())
  fileName_out <- paste0("/Volumes/ExtremeSSD/data/bigFiles/", m_lower,  "_", k, "_", climVar, "_global_daily_", yearSpanFinal, ".tif")
  #  fileName_out <- paste0("/Volumes/PassportMac/bigFiles/", m_lower,  "_", k, "_", climVar, "_global_daily_", yearSpanFinal, ".tif")
  print(system.time(writeRaster(r_out, fileName_out, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
}


r1_name <- paste0("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/ssp585/", m, "/",  m_lower, "_ssp585_tas_global_daily_2081_2090.tif")
r2_name <- paste0("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/ssp585/",  m, "/",  m_lower, "_ssp585_tas_global_daily_2091_2100.tif")
r1 <- rast(r1_name)
r2 <- rast(r2_name)
system.time(r_out <- c(r1, r2))
r1 <- r2 <- NULL
gc()
m_lower <- tolower(m)
fileName_out <- paste0("/Volumes/PassportMac/bigFiles/", m_lower, "_ssp585_tas_global_daily_2081_2090.tif")
print(Sys.time())

system.time(writeRaster(r_out, fileName_out, overwrite = TRUE, format = "GTiff", wopt= woptList)); flush.console()
}


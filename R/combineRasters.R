# combine 10 year rasters to get 20 year rasters

source("R/globallyUsed.R")
library(terra)
woptList <- list(gdal=c("COMPRESS=LZW"))
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/"
climateVars <- c("tasmax", "tasmin",  "tave", "pr", "hurs") #, "rsds", "sfcwind") # "tasmax", "tasmin"


startyearChoices_old <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_new <-  c(2041, 2081) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)

sspChoices <- c("ssp126", "ssp585") #"ssp126",
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
yearRangeOld <- 9
yearRangeNew <- 19
#test values
k <- "ssp585"
l <- 2041
i <- "MPI-ESM1-2-HR"
j <- "tasmax"

# get list of files in all the unitsCorrected dirs and combine the relevant ones

for (k in sspChoices) {
  for (i in modelChoices) {
    fileList <- list.files(paste0(locOfFiles, k, "/", i), full.names = TRUE, recursive = TRUE)
    fileList <- fileList[!grepl("tif.aux.xml", fileList, fixed = TRUE)]
    
    for (l in startyearChoices_new) {
      temp <- fileList
      print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l))
      l1 <- l
      l2 <- l + yearRangeOld
      yearSpanOld1 <- paste0(l1, "_", l1 + yearRangeOld)
      yearSpanOld2 <- paste0(l2, "_", l2 + yearRangeOld)
      yearSpanNew <- paste0(l1, "_", l1 + yearRangeNew)
      
      
      varListComplete <- c("_tasmax_", "_tasmin_", "_pr_", "_hurs_", "huss", "rlds", "_rsds_", "_sfcwind_", "_ps_", "_tas_", "_tave_", "prsn")
      varsToKeep <- c(  "_tasmax_", "_tasmin_", "_tave_", "_pr_", "_hurs_", "_rsds_", "_sfcwind_")
      #      varsToKeep <- c("_hurs_")
      varsToRemove <- varListComplete[!varListComplete %in% varsToKeep]
      
      for (cntr in varsToRemove) {
        filestoKeep <- temp[!grepl(cntr, temp, fixed = TRUE)] # keep only file names with ESM names in dirList
      }
      
      yearListComplete <- seq(from = 1951, to = 2100, by = 10)
      
      yearsToKeep <- c(startyearChoices_new, startyearChoices_new + 10)
      yearsToRemove <-  yearListComplete[!yearListComplete %in% yearsToKeep]
      yearsToRemove <- c(yearsToRemove, 2015)
      
      for (cntr in yearsToRemove) {
        filestoKeep <- filestoKeep[!grepl(cntr, filestoKeep, fixed = TRUE)] # keep only file names with the correct start year
      }
      
      for (j in climateVars) {
        temp <- filestoKeep
        filesToKeepClimVar <- temp[grepl(j, temp, fixed = TRUE)] # keep only file names with ESM names in dirList
      }
      
      periodsToRemove <- c("2041_2060", "2081_2100") # to exclude 20 year tifs already created
      for (cntr in periodsToRemove) {
        filesToKeepClimVar <- filesToKeepClimVar[!grepl(cntr, filesToKeepClimVar, fixed = TRUE)] # keep only file names with the correct start year
      }
      
      fileNameOut <- paste0(locOfFiles, k, "/", i, "/", tolower(i), "_", k, "_", j, "_", "global_daily", "_",yearSpanNew, ".tif")
      if (!fileNameOut %in% filesToKeepClimVar){
        print(paste0("fileNameOut that needs to be done : ", fileNameOut))
        print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l, ", variable: ", j, ", startyearChoices_new: ", startyearChoices_new[2] ))
        
        r1 <- rast(filesToKeepClimVar[grepl(startyearChoices_new[2], filesToKeepClimVar, fixed = TRUE)])
        r2 <- rast(filesToKeepClimVar[grepl(startyearChoices_new[2] + yearRangeOld, filesToKeepClimVar, fixed = TRUE)])
        rout <- c(r1, r2)
        print(fileNameOut)
        print(system.time(writeRaster(rout, fileNameOut, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()}
    }
  }
}


# need to do historical ensemble only for the moment
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/ensemble"
climateVars <- c("tasmax", "tasmin",  "tave", "pr", "hurs") #, "rsds", "sfcwind") # "tasmax", "tasmin"


#startyearChoices_old <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_new <-  c(1991) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRangeOld <- 9
yearRangeNew <- 19
#test values

j <- "tasmax"
l = 1991

# get list of files in all the unitsCorrected dirs and combine the relevant ones

fileList <- list.files(paste0(locOfFiles), full.names = TRUE, recursive = TRUE)
fileList <- fileList[!grepl("tif.aux.xml", fileList, fixed = TRUE)]

for (l in startyearChoices_new) {
  temp <- fileList
  print(paste0("start year: ", l))
  l1 <- l
  l2 <- l + yearRangeOld + 1 
  yearSpanOld1 <- paste0(l1, "_", l1 + yearRangeOld)
  yearSpanOld2 <- paste0(l2, "_", l2 + yearRangeOld)
  yearSpanNew <- paste0(l1, "_", l1 + yearRangeNew)
  
  
  varListComplete <- c("_tasmax_", "_tasmin_", "_pr_", "_hurs_", "huss", "rlds", "_rsds_", "_sfcwind_", "_ps_", "_tas_", "_tave_", "prsn")
  varsToKeep <- c(  "_tasmax_", "_tasmin_", "_tave_", "_pr_", "_hurs_", "_rsds_", "_sfcwind_")
  #      varsToKeep <- c("_hurs_")
  varsToRemove <- varListComplete[!varListComplete %in% varsToKeep]
  
  for (cntr in varsToRemove) {
    filestoKeep <- temp[!grepl(cntr, temp, fixed = TRUE)] # keep only file names with ESM names in dirList
  }
  
  yearListComplete <- seq(from = 1951, to = 2100, by = 10)
  
  yearsToKeep <- c(startyearChoices_new, startyearChoices_new + 10)
  yearsToRemove <-  yearListComplete[!yearListComplete %in% yearsToKeep]
  
  for (cntr in yearsToRemove) {
    filestoKeep <- filestoKeep[!grepl(cntr, filestoKeep, fixed = TRUE)] # keep only file names with the correct start year
  }
  
  periodsToRemove <- c("1991_2010") # to exclude 20 year tifs already created
  for (cntr in periodsToRemove) {
    filestoKeep <- filestoKeep[!grepl(cntr, filestoKeep, fixed = TRUE)] # keep only file names with the correct start year
  }
  
  for (j in climateVars) {
    temp <- filestoKeep
    filesToKeepClimVar <- temp[grepl(j, temp, fixed = TRUE)] # keep only file names with ESM names in dirList
    
    fileNameOut <- paste0(locOfFiles,  "/", "ensemble_historical_", j, "_",yearSpanNew, ".tif")
    if (!fileNameOut %in% filesToKeepClimVar){
      print(paste0("fileNameOut that needs to be done : ", fileNameOut))
      print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l, ", variable: ", j, ", startyearChoices_new: ", startyearChoices_new[2] ))
      
      r1 <- rast(filesToKeepClimVar[grepl(startyearChoices_new[1], filesToKeepClimVar, fixed = TRUE)])
      r2 <- rast(filesToKeepClimVar[grepl(startyearChoices_new[1] + yearRangeOld, filesToKeepClimVar, fixed = TRUE)])
      rout <- c(r1, r2)
      print(fileNameOut)
      print(system.time(writeRaster(rout, fileNameOut, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()}
  }
}
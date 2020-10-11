# combine 10 year rasters to get 20 year rasters

source("R/globallyUsed.R")
library(terra)
woptList <- list(gdal=c("COMPRESS=LZW"))
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/"
climateVars <- c( "tasmin", "tasmax", "tave", "pr", "hurs", "rsds", "sfcwind") # "tasmax", "tasmin"

climateVarstoKeep <- c("tasmax") # "tasmax", "tasmin"

startyearChoices_old <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_new <-  c(2041, 2081) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_new_historical <-  c(1991) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)

sspChoices <- c("ssp585", "ssp126") #ssp585") #"ssp126",
sspChoices <- "ssp585"
modelChoices <- c("UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices <- c( "MRI-ESM2-0")
modelChoices.lower <- tolower(modelChoices)
yearRangeOld <- 9
yearRangeNew <- 19
#test values
k <- "historical"
l <- 1991
i <- "MRI-ESM2-0"
j <- "tasmax"

# get list of files in all the unitsCorrected dirs and combine the relevant ones

for (k in sspChoices) {
  for (i in modelChoices) {
    fileList <- list.files(paste0(locOfFiles, k, "/", i), full.names = TRUE, recursive = TRUE)
    fileList <- fileList[!grepl("tif.aux.xml", fileList, fixed = TRUE)]
    startYearChoices <- startyearChoices_new
    if (k %in% "historical") startYearChoices <- startyearChoices_new_historical
    for (l in startYearChoices) {
      temp <- fileList
      print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l))
      l1 <- l
      l2 <- l + yearRangeOld
      yearSpanOld1 <- paste0(l1, "_", l1 + yearRangeOld)
      yearSpanOld2 <- paste0(l2, "_", l2 + yearRangeOld)
      yearSpanNew <- paste0(l1, "_", l1 + yearRangeNew)
      
      varListComplete <- c("_tasmax_", "_tasmin_", "_pr_", "_hurs_", "huss", "rlds", "_rsds_", "_sfcwind_", "_ps_", "_tas_", "_tave_", "prsn")
 #     varsToKeep <- c("_tasmax_", "_tasmin_", "_pr_", "_hurs_",  "_rsds_", "_sfcwind_") # "_tasmax_", "_tasmin_", "_pr_", "_hurs_",# remove "_tave_", for now
      varsToRemove <- varListComplete[!varListComplete %in% climateVarstoKeep]
      
      filestoKeep = c()
      for (cntr in climateVarstoKeep) {
        filestoKeep <- c(filestoKeep, temp[grepl(cntr, temp, fixed = TRUE)]) # keep file names with the climate variables in climateVarstoKeep
      }
      
      yearListComplete <- seq(from = 1951, to = 2100, by = 10)
      
      yearsToKeep <- c(l, l + 10)
      yearsToRemove <-  yearListComplete[!yearListComplete %in% yearsToKeep]
      yearsToRemove <- c(yearsToRemove, 2015)
      
      for (cntr in yearsToRemove) {
        filestoKeep <- filestoKeep[!grepl(cntr, filestoKeep, fixed = TRUE)] # keep only file names with the correct start year
      }
      
      for (j in climateVarstoKeep) {
        gc()
        print(paste0("climate variable: ", j))
        temp <- filestoKeep
        filesToKeepClimVar <- temp[grepl(j, temp, fixed = TRUE)] # keep only file names with ESM names in dirList
        fileInDir <- list.files(paste0(locOfFiles, k, "/", i), full.names = TRUE, recursive = TRUE)
        periodsToRemove <- c("1991_2010", "2041_2060", "2081_2100") # to exclude 20 year tifs already created
        for (cntr in periodsToRemove) {
          filesToKeepClimVar <- filesToKeepClimVar[!grepl(cntr, filesToKeepClimVar, fixed = TRUE)] # keep only file names with the correct start year
        }
        print(filesToKeepClimVar)
        
        fileNameOut <- paste0(locOfFiles, k, "/", i, "/", tolower(i), "_", k, "_", j, "_", "global_daily", "_", yearSpanNew, ".tif")
        #      unlink(fileNameOut) # use this when you want fileNameOut to be redone
        if (!fileNameOut %in% fileInDir){
          print(paste0("fileNameOut that needs to be done : ", fileNameOut))
          print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l, ", variable: ", j, ", startYearChoices: ", startYearChoices ))
          fileNameR1 <- filesToKeepClimVar[1]
          fileNameR2 <- filesToKeepClimVar[2]
          r1 <- rast(fileNameR1)
          r2 <- rast(fileNameR2)
          rout <- c(r1, r2)
          startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRangeNew, "-12-31")
          indices <- seq(as.Date(startDate), as.Date(endDate), 1)
          indices <- paste0("X", as.character(indices))
          names(rout) <- indices
          fileNameOut <- gsub("__", "_", fileNameOut) # a kludge until I can figure out the underscore sources.
          print(fileNameOut)
          print(system.time(writeRaster(rout, fileNameOut, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
          rout <- NULL
          gc()
        }
      }
    }
  }
}
  
  # need to do historical ensemble only for the moment
  locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/ensemble"
  climateVars <- c("tasmax", "tasmin",  "pr", "hurs", "rsds", "sfcwind") # "tasmax", "tasmin" "tave", 
  
  #startyearChoices_old <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
  startyearChoices_new <-  c(1991) 
  
  yearRangeOld <- 9
  yearRangeNew <- 19
  #test values
  
  j <- "rsds"
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
 #   varsToKeep <- c(  "_tasmax_", "_tasmin_", "_tave_", "_pr_", "_hurs_", "_rsds_", "_sfcwind_")
    varsToRemove <- varListComplete[!varListComplete %in% climateVarstoKeep]
    
    filestoKeep = c()
    for (cntr in climateVarstoKeep) {
      filestoKeep <- c(filestoKeep, temp[grepl(cntr, temp, fixed = TRUE)]) # remove file names with the climate variables in varsToRemove
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
      filesToKeepClimVar <- temp[grepl(j, temp, fixed = TRUE)] 
      
      fileNameOut <- paste0(locOfFiles,  "/", "ensemble_historical_", j, "_", yearSpanNew, ".tif")
      if (!fileNameOut %in% filesToKeepClimVar){
        print(paste0("fileNameOut that needs to be done : ", fileNameOut))
        print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l, ", variable: ", j, ", startyearChoices_new: ", startyearChoices_new[1] ))
        
        fileNameR1 <- filesToKeepClimVar[1]
        fileNameR2 <- filesToKeepClimVar[2]
        r1 <- rast(fileNameR1)
        r2 <- rast(fileNameR2)
        rout <- c(r1, r2)
        startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRangeNew, "-12-31")
        indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        indices <- paste0("X", as.character(indices))
        names(rout) <- indices
        print(fileNameOut)
        print(system.time(writeRaster(rout, fileNameOut, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()}
    }
  }
  
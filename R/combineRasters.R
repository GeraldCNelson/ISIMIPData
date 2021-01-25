# combine 10 year rasters to get 20 year rasters

#source("R/globallyUsed.R")
library(terra)
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

terraOptions(memfrac = 2,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/"
climateVars <- c( "tasmin", "tasmax", "tas", "pr", "hurs", "rsds", "sfcwind", "ps") # "tasmax", "tasmin"

climateVarstoKeep <- c("ps") # "tasmax", "tasmin"

startyearChoices_old <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_new <-  c(2041, 2081) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_new <-  c(2081) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_new_historical <-  c(1991) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)

sspChoices <- c("ssp585", "ssp126") #ssp585") #"ssp126",
#sspChoices <- "ssp585"
modelChoices <- c("UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "IPSL-CM6A-LR")
# modelChoices <- c("MPI-ESM1-2-HR")
modelChoices.lower <- tolower(modelChoices)
yearRangeOld <- 9
yearRangeNew <- 19
#test values
k <- "historical"
l <- 1991
i <- "UKESM1-0-LL"
j <- "hurs"

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
      
      varListComplete <- c("_tasmax_", "_tasmin_", "_pr_", "_hurs_", "huss", "rlds", "_rsds_", "_sfcwind_", "_ps_", "_tas_", "prsn")
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
        
        fileName_out <- paste0(locOfFiles, k, "/", i, "/", tolower(i), "_", k, "_", j, "_", "global_daily", "_", yearSpanNew, ".tif")
        unlink(fileName_out) # use this when you want fileName_out to be redone
        if (!fileName_out %in% fileInDir){
          print(paste0("fileName_out that needs to be done : ", fileName_out))
          print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l, ", variable: ", j, ", startYearChoices: ", startYearChoices ))
          fileNameR1 <- filesToKeepClimVar[1]
          fileNameR2 <- filesToKeepClimVar[2]
          r1 <- rast(fileNameR1)
          r2 <- rast(fileNameR2)
          rout <- c(r1, r2)
          print(rout)
          startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRangeNew, "-12-31")
          indices <- seq(as.Date(startDate), as.Date(endDate), 1)
          indices <- paste0("X", as.character(indices))
          names(rout) <- indices
          fileName_out <- gsub("__", "_", fileName_out) # a kludge until I can figure out the underscore sources.
          print(paste0("fileName_out", fileName_out))
          print(system.time(writeRaster(rout, fileName_out, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
          print(paste("Done with ", fileName_out))
          rout <- NULL
          gc()
        }
      }
    }
  }
}

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
      
      varListComplete <- c("_tasmax_", "_tasmin_", "_pr_", "_hurs_", "huss", "rlds", "_rsds_", "_sfcwind_", "_ps_", "_tas_", "prsn")
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
        
        fileName_out <- paste0(locOfFiles, k, "/", i, "/", tolower(i), "_", k, "_", j, "_", "global_daily", "_", yearSpanNew, ".tif")
        unlink(fileName_out) # use this when you want fileName_out to be redone
        if (!fileName_out %in% fileInDir){
          print(paste0("fileName_out that needs to be done : ", fileName_out))
          print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l, ", variable: ", j, ", startYearChoices: ", startYearChoices ))
          fileNameR1 <- filesToKeepClimVar[1]
          fileNameR2 <- filesToKeepClimVar[2]
          r1 <- rast(fileNameR1)
          r2 <- rast(fileNameR2)
          rout <- c(r1, r2)
          print(rout)
          startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRangeNew, "-12-31")
          indices <- seq(as.Date(startDate), as.Date(endDate), 1)
          indices <- paste0("X", as.character(indices))
          names(rout) <- indices
          fileName_out <- gsub("__", "_", fileName_out) # a kludge until I can figure out the underscore sources.
          print(paste0("fileName_out", fileName_out))
          print(system.time(writeRaster(rout, fileName_out, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
          print(paste("Done with ", fileName_out))
          rout <- NULL
          gc()
        }
      }
    }
  }
}

#startyearChoices_old <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
k <- "historical"
l <- 1991

yearRangeOld <- 9
yearRangeNew <- 19
for (i in modelChoices) {
  fileList <- list.files(paste0(locOfFiles, k, "/", i), full.names = TRUE, recursive = TRUE)
  fileList <- fileList[!grepl("tif.aux.xml", fileList, fixed = TRUE)]
  temp <- fileList
  print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l))
  l1 <- l
  l2 <- l + yearRangeOld
  yearSpanOld1 <- paste0(l1, "_", l1 + yearRangeOld)
  yearSpanOld2 <- paste0(l2, "_", l2 + yearRangeOld)
  yearSpanNew <- paste0(l1, "_", l1 + yearRangeNew)
  
  varListComplete <- c("_tasmax_", "_tasmin_", "_pr_", "_hurs_", "huss", "rlds", "_rsds_", "_sfcwind_", "_ps_", "_tas_", "prsn")
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
    
    fileName_out <- paste0(locOfFiles, k, "/", i, "/", tolower(i), "_", k, "_", j, "_", "global_daily", "_", yearSpanNew, ".tif")
    unlink(fileName_out) # use this when you want fileName_out to be redone
    if (!fileName_out %in% fileInDir){
      print(paste0("fileName_out that needs to be done : ", fileName_out))
      print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l, ", variable: ", j, ", startYearChoices: ", startYearChoices ))
      fileNameR1 <- filesToKeepClimVar[1]
      fileNameR2 <- filesToKeepClimVar[2]
      r1 <- rast(fileNameR1)
      r2 <- rast(fileNameR2)
      rout <- c(r1, r2)
      print(rout)
      startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRangeNew, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      names(rout) <- indices
      fileName_out <- gsub("__", "_", fileName_out) # a kludge until I can figure out the underscore sources.
      print(paste0("fileName_out", fileName_out))
      print(system.time(writeRaster(rout, fileName_out, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
      print(paste("Done with ", fileName_out))
      rout <- NULL
      gc()
    }
  }
}





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
    
    fileName_out <- paste0(locOfFiles,  "/", "ensemble_historical_", j, "_", yearSpanNew, ".tif")
    unlink(fileName_out) # use this when you want fileName_out to be redone
    if (!fileName_out %in% filesToKeepClimVar){
      print(paste0("fileName_out that needs to be done : ", fileName_out))
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
      print(fileName_out)
      print(system.time(writeRaster(rout, fileName_out, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()}
  }
}

# code to check all rasters
library(terra)
fileNameHolder <- character()
filesToCheck <- list.files("data/bigFiles", full.names = TRUE)
filesToCheck <- filesToCheck[!grepl("aux.xml", filesToCheck, fixed = TRUE)]
filesToCheck <- unique(filesToCheck) # just in case
filesToCheck_hurs <- filesToCheck[grepl("hurs", filesToCheck, fixed = TRUE)]
filesToCheck_tasmax <- filesToCheck[grepl("tasmax", filesToCheck, fixed = TRUE)]
filesToCheck_tasmin <- filesToCheck[grepl("tasmin", filesToCheck, fixed = TRUE)]
for (i in filesToCheck_hurs) {
  print(i)
  temp <- rast(i)
  minVal <- min(temp[[1]]@ptr[["range_min"]])
  print(paste0("minVal: ", minVal))
  if (minVal < 0) {
    print(i)
    fileNameHolder <- c(fileNameHolder, i)}
}

for (i in fileNameHolder) {
  file.remove(i)
}

# check tasmax  
fileNameHolder <- character()
for (i in filesToCheck_tasmax) {
  print(i)
  temp <- rast(i)
  maxVal <- max(temp[[1]]@ptr[["range_max"]])
  #    maxVal <- min(global(temp, max, na.rm = TRUE))
  print(paste0("maxVal: ", maxVal))
  if (maxVal > 200) {
    print(i)
    fileNameHolder <- c(fileNameHolder, i)}
}

for (i in filesToCheck_tasmax) {
  i_new <- gsub("__", "_", i)
  file.rename(from = i, to = i_new)
}

for (i in filesToCheck_tasmin) {
  i_new <- gsub("__", "_", i)
  file.rename(from = i, to = i_new)
}

# check tasmin  
fileNameHolder <- character()
for (i in filesToCheck_tasmin) {
  print(i)
  temp <- rast(i)
  maxVal <- max(temp[[1]]@ptr[["range_max"]])
  print(paste0("maxVal: ", maxVal))
  if (maxVal > 200) {
    print(i)
    fileNameHolder <- c(fileNameHolder, i)}
}

# for (i in fileNameHolder) {
#   file.remove(i)
# }


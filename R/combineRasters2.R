library(terra)
woptList <- list(gdal=c("COMPRESS=LZW"))
terraOptions(memfrac = 4,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
climateVars <- c( "tasmin", "tasmax", "tas", "pr", "hurs", "rsds", "sfcwind", "ps") # "tasmax", "tasmin"
modelChoices <- c("UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "IPSL-CM6A-LR")
#modelChoices <- c(  "IPSL-CM6A-LR")
yearRangeOld <- 9
yearRangeNew <- 19
climateVarstoKeep <- c("tasmin", "tasmax", "tas", "pr", "hurs", "rsds", "sfcwind", "ps") # "tasmax", "tasmin"
#climateVarstoKeep <- c("sfcwind") # "tasmax", "tasmin"

startYearChoices <-  c(2041, 2081) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
#startyearChoices <-  c(2081) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices_new_historical <-  c(1991) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
sspChoices <- c("ssp585", "ssp126") #ssp585") #"ssp126",
#sspChoices <- c("ssp585") #ssp585") #"ssp126",

all.files <- list.files("/Volumes/PassportMac/ISIMIP/climate_land_only/unitsCorrected/", full.names = TRUE, recursive = TRUE)
all.files <- all.files[!grepl("aux.xml", all.files, fixed = TRUE)]
all.files <- all.files[!grepl("ensemble_", all.files, fixed = TRUE)]

# keep vars needed
neededVars <-  all.files[grepl("_", climateVarstoKeep, "_", all.files, fixed = TRUE)]

#keep years  needed
yearToRemove <- c(1951, 1961, 1971, 1981, 2014, 2015, 2021, 2031, 2061, 2071)
for (cntr in yearToRemove) {
  neededVars <- neededVars[!grepl(cntr, neededVars, fixed = TRUE)] # keep only file names with ESM names in dirList
}

#test values
k <- "historical"
l <- 1991
i <- "UKESM1-0-LL"
j <- "ps"

locOfFiles <- "data/bigFiles/ps/"
locOfFilesbigFiles <- "data/bigFiles/"

for (k in sspChoices) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    for (l in startYearChoices) {
      j <- climateVarstoKeep
      print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l))
      l1 <- l
      l2 <- l + yearRangeOld + 1
      yearSpanOld1 <- paste0(l1, "_", l1 + yearRangeOld)
      yearSpanOld2 <- paste0(l2, "_", l2 + yearRangeOld)
      yearSpanNew <- paste0(l1, "_", l1 + yearRangeNew)
      fileNameR1 <- paste0(locOfFiles, modelName.lower, "_", k, "_", j, "_", "global_daily", "_", yearSpanOld1, ".tif")
      fileNameR2 <- paste0(locOfFiles, modelName.lower, "_", k, "_", j, "_", "global_daily", "_", yearSpanOld2, ".tif")
      r1 <- rast(fileNameR1)
      r2 <- rast(fileNameR2)
      rout <- c(r1, r2)
      startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRangeNew, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      names(rout) <- indices
      
      fileNameOut <- paste0(locOfFilesbigFiles, modelName.lower, "_", k, "_", j, "_", "global_daily", "_", yearSpanNew, ".tif")
      unlink(fileNameOut) # use this when you want fileNameOut to be redone
      
      print(paste0("fileNameOut: ", fileNameOut))
      print(system.time(writeRaster(rout, fileNameOut, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
      print(paste("Done with ", fileNameOut))
      rout <- NULL
      gc()
    }
  }
}

# historical -----
k <- "historical"
l <- 1991
filesInDir <- list.files(locOfFilesbigFiles)
locOfFiles <- "data/bigFiles/ps/"
for (i in modelChoices) {
  modelName.lower <- tolower(i)
  for (j in climateVarstoKeep) {
    print(paste0("variable: ", j, ", model: ", i, ", ssp choice: ", k, ", start year: ", l))
    if (!paste0(modelName.lower, "_", k, "_", j, "_", "global_daily", "_", yearSpanNew, ".tif") %in% filesInDir) {
      fileNameOut <- paste0(locOfFilesbigFiles, modelName.lower, "_", k, "_", j, "_", "global_daily", "_", yearSpanNew, ".tif")
      l1 <- l
      l2 <- l + yearRangeOld + 1
      yearSpanOld1 <- paste0(l1, "_", l1 + yearRangeOld)
      yearSpanOld2 <- paste0(l2, "_", l2 + yearRangeOld)
      yearSpanNew <- paste0(l1, "_", l1 + yearRangeNew)
      fileNameR1 <- paste0(locOfFiles, modelName.lower, "_", k, "_", j, "_", "global_daily", "_", yearSpanOld1, ".tif")
      fileNameR2 <- paste0(locOfFiles, modelName.lower, "_", k, "_", j, "_", "global_daily", "_", yearSpanOld2, ".tif")
      r1 <- rast(fileNameR1)
      r2 <- rast(fileNameR2)
      rout <- c(r1, r2)
      startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRangeNew, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      names(rout) <- indices
      
     #    unlink(fileNameOut) # use this when you want fileNameOut to be redone
      
      print(paste0("fileNameOut: ", fileNameOut))
      print(system.time(writeRaster(rout, fileNameOut, overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
      print(paste("Done with ", fileNameOut))
      rout <- NULL
      gc()
    }
  }
}

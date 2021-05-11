library(terra)
woptList <- list(gdal=c("COMPRESS=LZW"))
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))

terraOptions(memfrac = 2,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
climateVars <- c( "tasmin", "tasmax", "tas", "pr", "hurs", "rsds", "sfcwind", "ps") # "tasmax", "tasmin"
modelChoices <- c("UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "IPSL-CM6A-LR")
#modelChoices <- c( "UKESM1-0-LL") #, "GFDL-ESM4", "IPSL-CM6A-LR")
yearRangeOld <- 9
yearRangeNew <- 19
climateVarstoKeep <- c("tasmin", "tasmax", "tas", "pr", "hurs", "huss", "rsds", "rlds", "sfcwind", "ps", "prsn") # "tasmax", "tasmin"
#climateVarstoKeep <- c("sfcwind") # "tasmax", "tasmin"

startYearChoices20yr <-  c(2041, 2081) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
#startYearChoices <-  c(2081) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices_new_historical <-  c(1991) #1991, 2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
sspChoices <- c("ssp126", "ssp585")
#sspChoices <- c("ssp585")

# all.files <- list.files("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/", full.names = TRUE, recursive = TRUE)
# all.files <- all.files[!grepl("aux.xml", all.files, fixed = TRUE)]
# all.files <- all.files[!grepl("ensemble_", all.files, fixed = TRUE)]
# 
# # keep vars needed
# neededVars <-  all.files[grepl("_", climateVarstoKeep, "_", all.files, fixed = TRUE)]
# 
# #keep years  needed
# yearToRemove20yr <- c(1951, 1961, 1971, 1981, 2014, 2015, 2021, 2031, 2061, 2071)
# for (cntr in yearToRemove) {
#   neededVars <- neededVars[!grepl(cntr, neededVars, fixed = TRUE)] 
# }

#test values
k <- "ssp585"
l <- 2041
i <- "UKESM1-0-LL"
j <- "ps"

locOf10yrFiles <- "/Volumes/PassportMac/ISIMIP/cmip6/climate3b/"
locOfFilesbigFiles <- "/Volumes/PassportMac/ISIMIP/cmip6/climate3bCombined/"

for (k in sspChoices) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    for (l in startYearChoices20yr) {
      for (j in climateVarstoKeep) {
        print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l))
        l1 <- l
        l2 <- l + yearRangeOld + 1
        yearSpanOld1 <- paste0(l1, "_", l1 + yearRangeOld)
        yearSpanOld2 <- paste0(l2, "_", l2 + yearRangeOld)
        yearSpanNew <- paste0(l1, "_", l1 + yearRangeNew)
        fileNameR1 <- paste0(locOf10yrFiles, modelName.lower, "_", k, "_", j, "_", yearSpanOld1, ".tif")
        fileNameR2 <- paste0(locOf10yrFiles, modelName.lower, "_", k, "_", j, "_", yearSpanOld2, ".tif")
        r1 <- rast(fileNameR1)
        r2 <- rast(fileNameR2)
        rout <- c(r1, r2)
        startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRangeNew, "-12-31")
        # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        # indices <- paste0("X", as.character(indices))
        # names(rout) <- indices
        # 
        fileName_out <- paste0(locOfFilesbigFiles, modelName.lower, "_", k, "_", j, "_", yearSpanNew, ".tif")
        #       unlink(fileName_out) # use this when you want fileName_out to be redone
        terra:::readAll(rout) # notation needed to access the readAll function
        print(paste0("fileName_out: ", fileName_out))
        print(system.time(writeRaster(rout, fileName_out, overwrite = TRUE,  wopt= woptList))); flush.console()
        print(paste("Done with ", fileName_out))
        rout <- NULL
        gc()
      }
    }
  }
}

# historical -----
k <- "historical"
l <- 1991
for (i in modelChoices) {
  modelName.lower <- tolower(i)
  for (j in climateVarstoKeep) {
    print(paste0("model: ", i, ", ssp choice: ", k, ", start year: ", l))
    l1 <- l
    l2 <- l + yearRangeOld + 1
    yearSpanOld1 <- paste0(l1, "_", l1 + yearRangeOld)
    yearSpanOld2 <- paste0(l2, "_", l2 + yearRangeOld)
    yearSpanNew <- paste0(l1, "_", l1 + yearRangeNew)
    fileNameR1 <- paste0(locOf10yrFiles, modelName.lower, "_", k, "_", j, "_", yearSpanOld1, ".tif")
    fileNameR2 <- paste0(locOf10yrFiles, modelName.lower, "_", k, "_", j, "_", yearSpanOld2, ".tif")
    r1 <- rast(fileNameR1)
    r2 <- rast(fileNameR2)
    rout <- c(r1, r2)
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRangeNew, "-12-31")
    # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    # indices <- paste0("X", as.character(indices))
    # names(rout) <- indices
    # 
    fileName_out <- paste0(locOfFilesbigFiles, modelName.lower, "_", k, "_", j, "_", yearSpanNew, ".tif")
    #       unlink(fileName_out) # use this when you want fileName_out to be redone
    print(system.time(terra:::readAll(rout))) # notation needed to access the readAll function
    print(paste0("fileName_out: ", fileName_out))
    print(system.time(writeRaster(rout, fileName_out, overwrite = TRUE,  wopt= woptList))); flush.console()
    print(paste("Done with ", fileName_out))
    rout <- NULL
    gc()
  }
}

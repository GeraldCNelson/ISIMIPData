# climate data annual mean 
source("R/globallyUsed.R")
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
# library(stringr)

sspChoices <- c("ssp585") #"ssp126", "ssp585"
modelChoices <- c("GFDL-ESM4", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
#modelChoices <- c( "IPSL-CM6A-LR", "UKESM1-0-LL")
variableChoices <- c( "tasmax", "tasmin", "pr", "hurs", "tave") # "tasmin", tasmax
startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
locOfFiles <- locOfCMIP6ncFiles
yearRange <- 9

# test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
j <- "tave"


# useCores <- detectCores() - 1 # max number of cores
# useCores <- 3 # better for memory intensive activities

# varList <- c("startyearChoices", "sspChoices", "modelChoices", "wrld_land",  "locOfFiles")
# libList <- c("terra", "ncdf4", "stringr")
# 
# cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R

# foreach(k = sspChoices) %:%
#   foreach(i = modelChoices) %:%
#   foreach(l = startyearChoices) %:%
#   foreach(j = variableChoices) %dopar% {

for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      for (j in variableChoices) {
        modelName.lower <- tolower(i)
        yearSpan <- paste0(l, "_", l + yearRange)
        layerNames <- readRDS(paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))
        
        print(paste0("working on start year: ", l, " variable: ", j, " ssp choice: ", k, " model: ", " pid: ", Sys.getpid(), " systime: ", Sys.time()))
        fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
        fileNameIn <- paste0(fileNameIn, ".tif")
        temp <- paste(locOfFiles, k, "/", i, "/", fileNameIn, sep = "")
        rastIn <- rast(temp)
        
        print(system.time(rastIn.mean <- mean(rastIn)))
        rastIn.mean
        
        fileNameOut_annualMean <- paste0("annualMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        
        writeRaster(rastIn.mean, filename = paste0("data/cmip6/annualMean/", fileNameOut_annualMean), format = "GTiff", overwrite = TRUE)
        print(paste0("writing rastIn.mean ", fileNameOut_annualMean))
      }
    }
  }
}
#stopCluster(cl)

# do same calculations on observed data
observedlist <- c("hurs", "tasmax", "tave", "tasmin", "pr")
print(system.time(tasmax <- rast(tasmax.observed)))
print(system.time(tasmin <- rast(tasmin.observed)))
print(system.time(pr <- rast(pr.observed)))
print(system.time(hurs <- rast(hurs.observed)))
l = 2001
yearSpan <- paste0(l, "_", l + yearRange)

for (j in observedlist) {
    print(system.time(j.mean <- mean(get(j))))
  j.mean
  
  fileNameOut_annualMean <- paste0("annualMean_", j, "_", "observed_", yearSpan, ".tif")
  #  fileNameOut_monthCV <- paste0("monthCV_", j, "_", "observed_", yearSpan, ".tif")
  
  writeRaster(j.mean, filename = paste0("data/cmip6/annualMean/", fileNameOut_annualMean), format = "GTiff", overwrite = TRUE)
  #  writeRaster(rastIn.cv, filename = paste0("data/cmip6/annualMean/", fileNameOut_monthCV), format = "GTiff", overwrite = TRUE)
}


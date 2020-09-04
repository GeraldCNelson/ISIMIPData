# climate data annual mean 
source("R/globallyUsed.R")
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
# library(stringr)

sspChoices <- c("ssp585") #"ssp126", "ssp585"
modelChoices <- c("GFDL-ESM4", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
#modelChoices <- c( "IPSL-CM6A-LR", "UKESM1-0-LL")
climateVars <- c( "tasmax", "tasmin", "pr", "hurs", "tave") # "tasmin", tasmax
climateVars <- c( "pr") # "tasmin", tasmax

startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
locOfFiles <- locOfCMIP6ncFiles
yearRange <- 9

# test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051
j <- "pr"


# useCores <- detectCores() - 1 # max number of cores
# useCores <- 3 # better for memory intensive activities

# varList <- c("startyearChoices", "sspChoices", "modelChoices", "wrld_land",  "locOfFiles")
# libList <- c("terra", "ncdf4", "stringr")
# 
# cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R

# foreach(k = sspChoices) %:%
#   foreach(i = modelChoices) %:%
#   foreach(l = startyearChoices) %:%
#   foreach(j = climateVars) %dopar% {

for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      for (j in climateVars) {
        modelName.lower <- tolower(i)
        yearSpan <- paste0(l, "_", l + yearRange)
        #       layerNames <- readRDS(paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))
        
        startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
        indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        indices <- paste0("X", as.character(indices))
        
        print(paste0("working on start year: ", l, " variable: ", j, " ssp choice: ", k, " model: ", " pid: ", Sys.getpid(), " systime: ", Sys.time()))
        fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
        fileNameIn <- paste0(fileNameIn, ".tif")
        temp <- paste(locOfFiles, k, "/", i, "/", fileNameIn, sep = "")
        rastIn <- rast(temp)
        names(rastIn) <- indices
        rastIn
        
        print(system.time(rast.mean <- mean(rastIn))) # one layer with means of daily values for the 10 year period
        rast.mean
        
        indices_mean <- format(as.Date(names(rastIn), format = "X%Y-%m-%d"), format = "%m")
        
        indices_mean <- as.numeric(indices_mean)
        if (j %in% "pr") {
          print(system.time(rast.monthlyMean <- tapp(rastIn, indices_mean, fun = sum))) # create the monthly total rainfall values instead of average daily values
        } else {
          print(system.time(rast.monthlyMean <- tapp(rastIn, indices_mean, fun = mean)))
        }
        names(rast.monthlyMean) <- month.abb
        
        fileNameOut_annualMean <- paste0("annualMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        fileNameOut_monthlyMean <- paste0("monthlyMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        
        writeRaster(rast.mean, filename = paste0("data/cmip6/annualMean/", fileNameOut_annualMean), format = "GTiff", overwrite = TRUE)
        print(paste0("writing rast.mean: ", fileNameOut_annualMean))
        
        writeRaster(rast.monthlyMean, filename = paste0("data/cmip6/monthlyMean/", fileNameOut_monthlyMean), format = "GTiff", overwrite = TRUE)
        print(paste0("writing rast.monthlymean: ", fileNameOut_monthlyMean))
        
      }
    }
  }
}
#stopCluster(cl)

# do same calculations on observed data
observedlist <- c("hurs", "tasmax", "tasmin", "pr", "tave")
observedlist <- c( "pr")
print(system.time(tasmax <- rast(tasmax.observed)))
print(system.time(tasmin <- rast(tasmin.observed)))
print(system.time(pr <- rast(pr.observed)))
print(system.time(hurs <- rast(hurs.observed)))
print(system.time(tave <- rast(tave.observed)))
l = 2001
yearSpan <- paste0(l, "_", l + yearRange)

startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
indices <- seq(as.Date(startDate), as.Date(endDate), 1)

for (j in observedlist) {
  print(system.time(rast.mean <- mean(get(j))))
  rast.mean
  
  indices_mean <- format(indices, format = "%m")
  indices_mean <- as.numeric(indices_mean)
  if (j %in% "pr") {
    print(system.time(rast.monthlyMean <- tapp(get(j), indices_mean, fun = sum))) # create the monthly total rainfall values instead of average daily values
  } else {
    print(system.time(rast.monthlyMean <- tapp(get(j), indices_mean, fun = mean)))
  }
  names(rast.monthlyMean) <- month.abb
  
  
  fileNameOut_annualMean <- paste0("annualMean_", j, "_", "observed_", yearSpan, ".tif")
  #  fileNameOut_monthCV <- paste0("monthCV_", j, "_", "observed_", yearSpan, ".tif")
  
  fileNameOut_annualMean <- paste0("annualMean_", j, "_", "observed_", yearSpan, ".tif")
  fileNameOut_monthlyMean <- paste0("monthlyMean_", j, "_", "observed_", yearSpan, ".tif")
  
  writeRaster(rast.mean, filename = paste0("data/cmip6/annualMean/", fileNameOut_annualMean), format = "GTiff", overwrite = TRUE)
  #  writeRaster(rastIn.cv, filename = paste0("data/cmip6/annualMean/", fileNameOut_monthCV), format = "GTiff", overwrite = TRUE)
  print(paste0("writing rast.mean ", fileNameOut_annualMean))
  
  writeRaster(rast.monthlyMean, filename = paste0("data/cmip6/monthlyMean/", fileNameOut_monthlyMean), format = "GTiff", overwrite = TRUE)
  print(paste0("writing rast.mean ", fileNameOut_monthlyMean))
  
}

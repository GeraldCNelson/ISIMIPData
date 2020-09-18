# climate data annual mean 
source("R/globallyUsed.R")
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct
# library(stringr)
woptList <- list(gdal=c("COMPRESS=LZW"))

sspChoices <- c("ssp126", "ssp585") #"ssp126", "ssp585"
modelChoices <- c(  "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR", "MRI-ESM2-0", "GFDL-ESM4") #, "MRI-ESM2-0", "GFDL-ESM4", "MRI-ESM2-0","MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
modelChoices <- c( "IPSL-CM6A-LR", "MRI-ESM2-0", "GFDL-ESM4")
climateVars <- c( "tasmax", "tasmin", "pr", "hurs", "tave", "rsds", "sfcwind") # "tasmin", tasmax
climateVars <- c( "pr") #, "hurs", "tave") # "tasmin", tasmax

startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
locOfFiles <- locOfCMIP6tifFiles
yearRange <- 9

# test values
i <- "GFDL-ESM4"
k <- "ssp126"
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
        gc()
        modelName.lower <- tolower(i)
        yearSpan <- paste0(l, "_", l + yearRange)
        #       layerNames <- readRDS(paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))
        
        startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
        indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        indices <- paste0("X", as.character(indices))
        
        print(paste0("working on start year: ", l, ", variable: ", j, " ssp choice: ", k, " model: ", " pid: ", Sys.getpid(), " systime: ", Sys.time()))
        fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
        fileNameIn <- paste0(fileNameIn, ".tif")
        temp <- paste(locOfFiles, k, "/", i, "/", fileNameIn, sep = "")
        rastIn <- rast(temp)
        names(rastIn) <- indices
        rastIn
        
        print(system.time(rast.mean <- mean(rastIn))) # one layer with one value per cell; the means of daily values for the 10 year period
        rast.mean
        
        indices_mean_month <- format(as.Date(names(rastIn), format = "X%Y-%m-%d"), format = "%m")
        indices_mean_day <- format(as.Date(names(rastIn), format = "X%Y-%m-%d"), format = "%j")
        indices_mean_month <- as.numeric(indices_mean_month)
        indices_mean_day <- as.numeric(indices_mean_day)
        
        if (j %in% "pr") {
          #create the da
          print(system.time(rast.dailyMean <- tapp(rastIn, indices_mean_day, fun = mean))) # daily mean for the 10 year period
          # indices_dayave <- as.numeric(format(as.Date(names(rast.dailyMean), format = "X%j"), format = "%m"))
          print(system.time(rast.monthlyMean <- tapp(pr, indices_mean_month, fun = sum))) # sum for pr. this sums the values for all days in a month across the whole data set. Need to divide by yearRange to get the per month value
          print(system.time(rast.monthlyMean <- rast.monthlyMean/yearRange))
          
        } else {
          print(system.time(rast.monthlyMean <- tapp(rastIn, indices_mean_month, fun = mean)))
        }
        names(rast.monthlyMean) <- month.abb
        
        fileNameOut_annualMean <- paste0("annualMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        fileNameOut_monthlyMean <- paste0("monthlyMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        
        print(paste0("writing annual mean: ", fileNameOut_annualMean))
        writeRaster(rast.mean, filename = paste0("data/cmip6/annualMean/", fileNameOut_annualMean), format = "GTiff", overwrite = TRUE, wopt= woptList)
        
        print(paste0("writing monthly mean: ", fileNameOut_monthlyMean))
        writeRaster(rast.monthlyMean, filename = paste0("data/cmip6/monthlyMean/", fileNameOut_monthlyMean), format = "GTiff", overwrite = TRUE, wopt= woptList)
      }
    }
  }
}
#stopCluster(cl)

# # do same calculations on observed data
# observedlist <- c("hurs", "tasmax", "tasmin", "pr", "tave", "rsds", "sfcwind")
# observedlist <- c( "pr")
# print(system.time(tasmax <- rast(tasmax.observed)))
# print(system.time(tasmin <- rast(tasmin.observed)))
# print(system.time(pr <- rast(pr.observed)))
# print(system.time(hurs <- rast(hurs.observed)))
# print(system.time(tave <- rast(tave.observed)))
# l = 2001
# yearSpan <- paste0(l, "_", l + yearRange)
# 
# startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
# indices <- seq(as.Date(startDate), as.Date(endDate), 1)
# 
# for (j in observedlist) {
#   print(system.time(rast.mean <- mean(get(j))))
#   rast.mean
#   
#   if (j %in% "pr") {
#     #create the monthly sum of daily means
#     indices_mean_day <- format(indices, format = "%j")
#     indices_mean_day <- as.numeric(indices_mean_day)
#     print(system.time(rast.dailyMean <- tapp(pr, indices_mean_day, fun = mean)))
#     
#     indices_mean_month <- format(indices, format = "%m")
#     indices_mean_month <- as.numeric(indices_mean_month)
#     print(system.time(rast.monthlyMean <- tapp(pr, indices_mean_month, fun = sum))) # sum for pr. this sums the values for all days in a month across the whole data set. Need to divide by yearRange to get the per month value
#     print(system.time(rast.monthlyMean <- rast.monthlyMean/yearRange))
#   } else {
#     print(system.time(rast.monthlyMean <- tapp(get(j), indices_mean, fun = mean))) # mean for other variables
#   }
#   names(rast.monthlyMean) <- month.abb
#   
#   
#  # fileNameOut_annualMean <- paste0("annualMean_", j, "_", "observed_", yearSpan, ".tif")
#   #  fileNameOut_monthCV <- paste0("monthCV_", j, "_", "observed_", yearSpan, ".tif")
#   
#   fileNameOut_annualMean <- paste0("annualMean_", j, "_", "observed_", yearSpan, ".tif")
#   fileNameOut_monthlyMean <- paste0("monthlyMean_", j, "_", "observed_", yearSpan, ".tif")
#   
#   print(paste0("writing rast annual mean ", fileNameOut_annualMean))
#   writeRaster(rast.dailyMean, filename = paste0("data/cmip6/annualMean/", fileNameOut_annualMean), format = "GTiff", overwrite = TRUE, wopt= woptList)
#   #  writeRaster(rastIn.cv, filename = paste0("data/cmip6/annualMean/", fileNameOut_monthCV), format = "GTiff", overwrite = TRUE)
#   
#   print(paste0("writing rast monthly mean ", fileNameOut_monthlyMean))
#   writeRaster(rast.monthlyMean, filename = paste0("data/cmip6/monthlyMean/", fileNameOut_monthlyMean), format = "GTiff", overwrite = TRUE, wopt= woptList)
#   
# }

# do same calculations on historical data; note ensemble calculations for historical done in ensembleCalcsHistoricalData.R. Here we just need to calculate the monthly means or sums for precip
climateVars <- c( "tasmax", "tasmin", "tave", "pr", "tave", "rsds", "sfcwind", "hurs") #"hurs",
observedlist <- c( "pr")
startyearChoices <-  c(1981, 1991, 2001) 
#startyearChoices <-  c(2001) 
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/ensemble/"

#test values
l = 2001
j = "tave"

for (l in startyearChoices) {
  
  yearSpan <- paste0(l, "_", l + yearRange)
  
  startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
  indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  
  for (j in climateVars) {
    gc()
    
    print(paste0("working on start year: ", l, ", variable: ", j, ", pid: ", Sys.getpid(), " systime: ", Sys.time()))
    fileNameIn <- paste("ensemble_historical",  j, yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".tif")
    temp <- paste(locOfFiles, fileNameIn, sep = "")
    rastIn <- rast(temp)
    rastIn
    
    print(system.time(rast.mean <- mean(rastIn))) # one layer with one value per cell; the means of daily values for the 10 year period
    rast.mean
    
    indices_mean_month <- format(indices, format = "%m")
    indices_mean_day <- format(indices, format = "%j")
    indices_mean_month <- as.numeric(indices_mean_month)
    indices_mean_day <- as.numeric(indices_mean_day)
    
    if (j %in% "pr") {
       print(system.time(rast.dailyMean <- tapp(rastIn, indices_mean_day, fun = mean)))
      
      indices_mean_month <- format(indices, format = "%m")
      indices_mean_month <- as.numeric(indices_mean_month)
      print(system.time(rast.monthlyMean <- tapp(rastIn, indices_mean_month, fun = sum))) # sum for pr. this sums the values for all days in a month across the whole data set. Need to divide by yearRange to get the per month value
#      print(system.time(rast.monthlyMean <- rast.monthlyMean/yearRange)) don't divide by yearRange here
    } else {
      print(system.time(rast.monthlyMean <- tapp(rastIn, indices_mean_month, fun = mean)))
    }
    names(rast.monthlyMean) <- month.abb
    
    fileNameOut_annualMean <- paste0("ensembleAnnualMean_", j, "_historical_", yearSpan, ".tif")
    fileNameOut_monthlyMean <- paste0("ensembleMonthlyMean_", j, "_historical_", yearSpan, ".tif")
    
    print(paste0("writing annual mean: ", fileNameOut_annualMean))
    writeRaster(rast.mean, filename = paste0("data/cmip6/annualMean/", fileNameOut_annualMean), format = "GTiff", overwrite = TRUE, wopt= woptList)
    
    print(paste0("writing monthly mean: ", fileNameOut_monthlyMean))
    writeRaster(rast.monthlyMean, filename = paste0("data/cmip6/monthlyMean/", fileNameOut_monthlyMean), format = "GTiff", overwrite = TRUE, wopt= woptList)
  }
}





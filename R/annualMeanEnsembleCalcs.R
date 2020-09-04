# combine 10 year rasters across models to get ensemble annual means and coefficients of variation

source("R/globallyUsed.R")
# library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct

startyearChoices <-  c(2021, 2051, 2091) #2021, 2051, 2091) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data
climateVars <- c( "tasmax", "tasmin", "tave", "pr", "hurs") 

yearRange <- 9
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")
modelChoices.lower <- tolower(modelChoices)
#test values
k <- "ssp585"
l <- 2021
j <- "pr"


# varList <- c("modelChoices", "thiList",  "startyearChoices_ensemble", "sspChoices", "tmpDirName")
# libList <- c("rast", "data.table")

# UseCores <- detectCores() - 1 # max number of cores
# useCores <- 3 # better for memory intensive activities
# cl <- clusterSetup(varList, libList, useCores = useCores)

# x <- foreach(l = startyearChoices_ensemble, .combine = rbind) %:%
#   foreach(k = sspChoices, .combine = rbind)  %dopar% {
readRast <- function(m) {
  fileNameIn <- paste0("data/cmip6/annualMean/annualMean_", j, "_", m, "_", k,  "_", yearSpan, ".tif")
  r <- rast(fileNameIn)
  r
}

# weight needed for global averages
#areaWeight <- area(r.mean, sum = FALSE) / 1000000
areaWeight <- rast("data-raw/landAreaWeight.tif") # created July 27, 2020
globalMeanHolder <- data.table(climVar = character(), scenario = character(), yearSpan = character(), globalMeanValue = numeric())

for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l, ", pid number: ", Sys.getpid()))
    
    for (j in climateVars) {
      
      # make a list of SpatRasters
      x <- lapply(modelChoices.lower, readRast)
      r <- rast(x)
#      system.time(r.mean <- tapp(r, 1:12, fun = mean))
      system.time(r.mean <- mean(r))
      print(paste0("climate var.: ", j, ", start year: ", l,  ", pid number: ", Sys.getpid()))
      print(r.mean)
      
      # special handling for coefficient of variation
      if (j %in% c("tasmin", "tasmax", "tave")) {
        rKel <- r + 273.15 # convert temp to Kelvin for cv calc on temps
#        system.time(r.cv <- tapp(rKel, 1:12, fun = raster::cv))
        system.time(r.cv <-  tapp(rKel, 1, fun = raster::cv))
      }
      if (j %in% "pr") {
       rPrecip <- r + 10
 #       system.time(r.cv <- tapp(rPrecip, 1:12, fun = raster::cv))
        system.time(r.cv <- tapp(rPrecip, 1, fun = raster::cv))
      }
      if (j %in% "hurs") {
  #      system.time(r.cv <- tapp(r, 1:12, fun = raster::cv))
        system.time(r.cv <- tapp(r, 1, fun = raster::cv))
      }
      print(r.cv)
      
 # get global land average (less Antarctica)
      temp <- global(r.mean, fun = "mean", weights = areaWeight, na.rm = TRUE)
      temp.mean <- mean(temp$weighted_mean)
      newRow <- list(j, k, yearSpan, temp.mean)
      globalMeanHolder <- rbind(globalMeanHolder, newRow)
      
      
      # # approach 2
      # startTime <- Sys.time()
      # rs <- rstk(x)
      # rs.mean2 <- lapp(rs, fun = terra::mean)
      # rs.cv2 <- lapp(rs, fun = raster::cv)
      # endTime <- Sys.time()
      # endTime - startTime
      
      # print(paste0( "Done setting doing raster indices for rasterList stack for species: ", speciesName, ", start year: ", l))
      # rasterList.mean <- tapp(rasterList, index, fun = mean)
      # rasterList.cv <- tapp(rasterList, index, fun = raster::cv) #, na.rm = TRUE)
      # names(rasterList.mean) <- month.abb
      # names(rasterList.cv) <- month.abb
      # print(paste0( "Done updating raster names with month.abb for species: ", speciesName, ", start year: ", l))
      fileNameMean <- paste0("data/cmip6/annualMean/ensembleAnnualMean_", j,  "_",  yearSpan, "_", k, ".tif") 
      fileNameCV <- paste0("data/cmip6/annualMean/ensembleAnnualCV_", j,  "_",  yearSpan, "_", k, ".tif")
      fileNameSD <- paste0("data/cmip6/annualMean/ensembleAnnualSD_", j,  "_",  yearSpan, "_", k, ".tif")
      writeRaster(r.mean, filename = fileNameMean, format = "GTiff", overwrite = TRUE)
      writeRaster(r.cv, filename = fileNameCV, format = "GTiff", overwrite = TRUE)
#      writeRaster(r.sd, filename = fileNameSD, format = "GTiff", overwrite = TRUE)
      print(paste("fileNameMeanOut: ", fileNameMean))
      print(paste0( "Done writing out files for variable: ", j, ", start year: ", l, ", pid number: ", Sys.getpid()))
    }
    #    unlink(tmpDirName, recursive = TRUE)
    gc(reset = FALSE, full = TRUE) 
  }
}

# stopCluster(cl)

# add 2001-2010 global mean values to globalMeanHolder
l <- 2001
yearSpan <- paste0(l, "_", l + yearRange)

for (j in climateVars) {
  fileNameIn <- paste0("data/cmip6/annualMean/annualMean_", j, "_", "observed",  "_", yearSpan, ".tif")
  r <- rast(fileNameIn)
  r <- 
  #      system.time(r.mean <- tapp(r, 1:12, fun = mean))
  system.time(r.mean <- mean(r))
  print(paste0("climate var.: ", j, ", start year: ", l,  ", pid number: ", Sys.getpid()))
  print(r.mean)
  
  # get global land average (less Antarctica)
  temp <- global(r.mean, fun = "mean", weights = areaWeight, na.rm = TRUE)
  temp.mean <- mean(temp$weighted_mean)
  newRow <- list(j, k, yearSpan, temp.mean)
  globalMeanHolder <- rbind(globalMeanHolder, newRow)
  write.csv(globalMeanHolder, file = "data/cmip6/annualMean/globalMeans.csv", row.names = FALSE)
  
}

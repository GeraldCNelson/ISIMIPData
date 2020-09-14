library(terra)
Sys.setenv(PROJ_LIB = "/usr/local/Cellar/proj/7.0.1/share/proj") # only needed for my mac system apparently.
terraOptions(progress = 20, tempdir =  "data/ISIMIP/") # need to use a relative path
sspChoices <- c("ssp585") #"ssp126", "ssp585"
modelChoices <- c("GFDL-ESM4", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
modelChoices <- c( "IPSL-CM6A-LR", "UKESM1-0-LL")
climateVars <- c( "tasmax", "tasmin", "pr", "hurs") # "tasmin", tasmax
startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
locOfFiles <- "data-raw/ISIMIP/cmip6/unitsCorrected/"
yearRange <- 9

monthlyMeanCompleted <- list.files("data/cmip6/monthlyMean/")
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      for (j in climateVars) {
        yearSpan <- paste0(l, "_", l + yearRange)
        modelName.lower <- tolower(i)
        fileNameOut_monthlyMean <- paste0("monthlyMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        
        if (!paste0(fileNameOut_monthlyMean) %in% monthlyMeanCompleted) {
          
          #       print(paste0("working on start year: ", l, ", variable: ", j, ", ssp choice: ", k, ", model: ",   ", start time: ", Sys.time()))
          layerNames <- readRDS(paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))
          fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
          fileNameIn <- paste0(fileNameIn, ".nc")
          temp <- paste(locOfFiles, k, "/", i, "/", fileNameIn, sep = "")
          print(paste0("Working on : ", temp))
          ncin <- rast(temp) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
          print(ncin)
          names(ncin) <- layerNames
          indices <- layerNames
          indices <- format(as.Date(indices, format = "X%Y.%m.%d"), format = "%m")
          indices <- as.numeric(indices)
          print(system.time(ncin.mean <- tapp(ncin, indices, fun = mean)))
          # ncin.cv <- tapp(ncin, indices, fun = cv, na.rm = TRUE)
          names(ncin.mean) <- month.abb
          fileNameOut_monthlyMean <- paste0("monthlyMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
          writeRaster(ncin.mean, filename = paste0("data/cmip6/monthlyMean/", fileNameOut_monthlyMean), format = "GTiff", overwrite = TRUE)
          print(paste0("Done with :" , fileNameOut_monthlyMean))
        }else{
          print(paste("This file has already been created: ", fileNameOut_monthlyMean))
        }
      }
    }
  }
}

# do same calculations on observed data

tasmax <- tasmax.observed
tasmin <- tasmin.observed
pr <- pr.observed
hurs <- hurs.observed
yearSpan <- paste0(l, "_", l + yearRange)

observedlist <- c("hurs", "tasmax", "tasmin", "pr")
layerNames <- readRDS(paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))

for (j in observedlist) {
  filenameIn <- paste0("data-raw/ISIMIP/cmip6/unitsCorrected/observed/gswp3-w5e5_obsclim_", j, "_global_daily_2001_2010.nc")
  print(paste0("Working on : ", filenameIn))
   ncin <- rast(temp) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
  
  names(ncin) <- layerNames
  indices <- layerNames
  indices <- format(as.Date(indices, format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)
  print(system.time(ncin.mean <- tapp(ncin, indices, fun = mean)))
  
  names(ncin.mean) <- month.abb
  
  fileNameOut_monthlyMean <- paste0("monthlyMean_", j, "_", "observed_", yearSpan, ".tif")
 # fileNameOut_monthCV <- paste0("monthCV_", j, "_", "observed_", yearSpan, ".tif")
  
  writeRaster(ncin.mean, filename = paste0("data/cmip6/monthlyMean/", fileNameOut_monthlyMean), format = "GTiff", overwrite = TRUE)
  
  gc(reset = FALSE, full = TRUE)
}  

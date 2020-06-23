library(terra)
Sys.setenv(PROJ_LIB = "/usr/local/Cellar/proj/7.0.1/share/proj") # only needed for my mac system apparently.
terraOptions(progress = 20, tempdir =  "data/ISIMIP/") # need to use a relative path
sspChoices <- c("ssp585") #"ssp126", "ssp585"
modelChoices <- c("GFDL-ESM4", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
modelChoices <- c( "IPSL-CM6A-LR", "UKESM1-0-LL")
variableChoices <- c( "hurs", "tasmax", "tasmin", "pr") # "tasmin", tasmax
startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
locOfFiles <- "data-raw/ISIMIP/cmip6/unitsCorrected/"
yearRange <- 9

monthMeanCompleted <- list.files("data/cmip6/monthMean/")
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      for (j in variableChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        modelName.lower <- tolower(i)
        fileNameOut_monthMean <- paste0("monthMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        
        if (!paste0(fileNameOut_monthMean) %in% monthMeanCompleted) {
          
          #       print(paste0("working on start year: ", l, " variable: ", j, " ssp choice: ", k, " model: ",   " start time: ", Sys.time()))
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
          fileNameOut_monthMean <- paste0("monthMean_", j, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
          writeRaster(ncin.mean, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthMean), format = "GTiff", overwrite = TRUE)
          print(paste0("Done with :" , fileNameOut_monthMean))
        }else{
          print(paste("This file has already been created: ", fileNameOut_monthMean))
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
observedlist <- c("hurs", "tasmax", "tasmin", "pr")

for (j in observedlist) {
  filenameIn <- get(j)
  print(paste0("Working on : ", filenameIn))
  
  #in the hurs file, the variable name is rhs
  ncin <- rast(filenameIn) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
  # ncin <- readAll(ncin) # seems to speed up processing if ncin is an nc file
  # ncin <- fixUnits(var = j, ncin = ncin) # fixes temp and precip units; assumes ncin values are raw units
  
  indices <- format(as.Date(names(ncin), format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)
  ncin.mean <- tapp(ncin, indices, fun = mean, na.rm = TRUE)
  ncin.cv <- tapp(ncin, indices, fun = cv, na.rm = TRUE)
  names(ncin.mean) <- month.abb
  names(ncin.cv) <- month.abb
  
  yearSpan <- "2001_2010"
  
  fileNameOut_monthMean <- paste0("monthMean_", j, "_", "observed_", yearSpan, ".tif")
  fileNameOut_monthCV <- paste0("monthCV_", j, "_", "observed_", yearSpan, ".tif")
  
  writeRaster(ncin.mean, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthMean), format = "GTiff", overwrite = TRUE)
  writeRaster(ncin.cv, filename = paste0("data/cmip6/monthMean/", fileNameOut_monthCV), format = "GTiff", overwrite = TRUE)
  
  gc(reset = FALSE, full = TRUE)
}  
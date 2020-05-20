# analysis of precipitation data

source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
library(foreach) #Provides foreach looping construct
library(stringr)

sspChoices <- c("ssp585") #"ssp126", "ssp585"
modelChoices <- c("GFDL-ESM4", "MRI-ESM2-0", "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
#modelChoices <- c( "MRI-ESM2-0")
variableChoices <- c( "pr") # "tasmax", "pr", "hurs") # "tasmin", tasmax
startyearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
locOfFiles <- locOfCMIP6ncFiles
yearRange <- 9

IPCC_WG2_Ch5_crop_temperature_table <- as.data.table(read_excel("data-raw/crops/Crop_temperature_table_summary_02052020.xlsx", range = "A1:S26"))
setnames(IPCC_WG2_Ch5_crop_temperature_table, old = names(IPCC_WG2_Ch5_crop_temperature_table), new = make.names(names(IPCC_WG2_Ch5_crop_temperature_table)))
cropChoices <- unique(IPCC_WG2_Ch5_crop_temperature_table$crop)

# info on managing raster disk use - https://stackoverflow.com/questions/25426405/raster-package-taking-all-hard-drive

#Sys.setenv(PROJ_LIB = "/usr/local/Cellar/proj/6.2.1/share/proj") # use until the sf and gdal issues get sorted out. See https://github.com/r-spatial/sf/issues/1060

useCores <- detectCores() - 1 # max number of cores
useCores <- 3 # better for memory intensive activities

varList <- c("startyearChoices", "sspChoices", "modelChoices", "wrld_land",  "locOfFiles", "cropChoices")
libList <- c("raster", "ncdf4", "stringr")

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2091
j = "pr"
m = 1

cl <- clusterSetup(varList, libList, useCores) # function created in globallyUsed.R

foreach(i = modelChoices) %:%
  foreach(l = startyearChoices) %:%
  foreach(j = variableChoices) %:%
  foreach(k = sspChoices) %dopar% {
    require(raster)
    tmpDirName <- paste0(locOfFiles, "rasterTmp_", Sys.getpid(), "/")
    rasterOptions(tmpdir = tmpDirName)
    dir.create(tmpDirName)
    
    print(paste0("working on start year: ", l, " variable: ", j, " ssp choice: ", k, " model: ", " pid: ", Sys.getpid(), " systime: ", Sys.time()))
    modelName.lower <- tolower(i)
    startTime <-  Sys.time()
    yearSpan <- paste0(l, "_", l + yearRange)
    
    fileNameIn <- paste(modelName.lower, k, j, "global_daily", yearSpan, sep = "_")
    fileNameIn <- paste0(fileNameIn, ".nc")
    
    temp <- paste(locOfFiles, k, "/", i, "/", fileNameIn, sep = "")
    print(paste0("Working on : ", temp))
    brick.pr <- brick(temp, varname = j) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"

    # the overlay function needs a user defined function on the relationship between the two rasters
    overlayfunction <- function(x,y) {
      return(x * y)
    }
    
    for (m in 1:length(cropChoices)) { #fruits variable is from the globallyUsed.R file
      cropName <- cropChoices[m]
      fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(cropName), ".tif")
      print(paste0("fileNameMaskIn: ", fileNameMask.in))
      mask <- raster(fileNameMask.in)
      startTime <- Sys.time()
      brick.pr.masked <- overlay(brick.pr, mask, fun = overlayfunction)
      endTime <- Sys.time()
      temp <- endTime - startTime
      print(paste0("time to crop precip data: ", temp))
      
      
       fileNameMean.masked <- paste0("data/cmip6/chillingHours/chillHrs_", hemisphereName, "_ensembleMean_masked_", cropName, "_",  yearSpan, "_", k, ".tif")
      fileNameCV.masked <- paste0("data/cmip6/chillingHours/chillHrs_", hemisphereName, "_ensembleCV_masked_", cropName, "_",  yearSpan, "_", k, ".tif")
      print(paste("fileNameMean.masked: ", fileNameMean.masked))
      writeRaster(mean.masked, filename = fileNameMean.masked, format = "GTiff", overwrite = TRUE)
      writeRaster(CV.masked, filename = fileNameCV.masked, format = "GTiff", overwrite = TRUE)
    }
    
    
    
    
    
    fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(speciesName), ".tif")
    
    cropName <- m
    cropCalFilesLoc <- "data-raw/crops/cropCalendars/ALL_CROPS_netCDF_0.5deg_filled/"
    fileInName <- paste0(cropName, ".crop.calendar.fill.nc")
    #    locNFileIn <- paste0(filesLoc, fileInName, ".gz")
    locNFileIn <- paste0(cropCalFilesLoc, fileInName)
    R.utils::gunzip(paste0(locNFileIn, ".gz"), remove = FALSE)
    
    #   croppingCalendar_plantstart <- readAll(raster(locNFileIn, var = "plant.start"))
    #   croppingCalendar_plantend <-  readAll(raster(locNFileIn, var = "plant.end"))
    # croppingCalendar_plantrange <- raster(locNFileIn, var = "plant.range")
    croppingCalendar_plant <- raster(locNFileIn, var = "plant")
    croppingCalendar_harvest <- raster(locNFileIn, var = "harvest")
    # croppingCalendar_harveststart <- raster(locNFileIn, var = "harvest.start")
    # croppingCalendar_harvestend <- raster(locNFileIn, var = "harvest.end")
    # 
    #gdd_cropped <- mask(gdd, rInAreaAgg)
    croppingCalendar_plant_crop <- mask(croppingCalendar_plant, rInAreaAgg)
    croppingCalendar_harvest_crop <- mask(croppingCalendar_harvest, rInAreaAgg)
    cal <- readAll(stack(croppingCalendar_plant_crop, croppingCalendar_plant_crop))
    unlink(locNFileIn) # delete the .nc file when no longer needed.
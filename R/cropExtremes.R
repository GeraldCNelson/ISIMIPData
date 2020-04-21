# crop min and max temp analysis
source("R/globallyUsed.R")
rcpChoices <- c("rcp85") #"rcp45", 
modelChoices <- c( "GFDL-ESM2M") # "IPSL-CM5A-LR", "MIROC5", "HadGEM2-ES",
variableChoices <- c("tasmax_day") # "tasmax_day", "pr_day", "hurs_day") # "tasmin_day", tasmax_day
spatialCoverageChoices <- "landonly"
startday <- "0101"
endday <- "1231"
filler <- "r1i1p1_EWEMBI" # EWEMBI is the bias correction method. r1i1p1 is a single ensemble member selected for each model. Explained
# somewhat in https://www.geosci-model-dev-discuss.net/gmd-2019-143/gmd-2019-143.pdf.
startyearChoices <-  c(2011, 2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)


yearRange <- 9

Sys.setenv(PROJ_LIB = "/usr/local/Cellar/proj/6.2.1/share/proj") # use until the sf and gdal issues get sorted out. See https://github.com/r-spatial/sf/issues/1060

IPCC_WG2_Ch5_crop_temperature_table <- read_excel("data-raw/crops/IPCC WG2 Ch5 crop temperature table 200220202.xlsx")
tLethal_max <- sort(unique(IPCC_WG2_Ch5_crop_temperature_table$Tlethal_max))

for (i in modelChoices) {
  for (j in variableChoices) {
    for (k in rcpChoices) {
      for (l in startyearChoices) {
        startTime <-  Sys.time()
        yearSpan <- paste0(l, startday, "_", l + yearRange, endday)
        fileNameIn <- paste(j, i, k, filler, spatialCoverageChoices, yearSpan, sep = "_")
        fileNameIn <- paste0(fileNameIn, ".nc")
        print(paste0("Working on : ", fileNameIn))
        temp <- paste0("data-raw/ISIMIP/", fileNameIn)
        ncin <- ncdf4::nc_open(temp)
        # get raster version of the data for a particular variable. This makes it easy to do raster math on the data.
        varToGet <- names(ncin[['var']])
        print(paste("variable name:", varToGet))
        print(paste("units:", ncin[["var"]][[varToGet]][["units"]]))
        print(paste("years covered:", yearSpan))
        
        ncin.brick <- brick(temp, varname = varToGet) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
        print("created ncin.brick")
        ncin.brick <- readAll(ncin.brick) # loads all of the raster into memory. Should speed up calculations  but help file says shouldn't normally be used
        ncin.brick <- fixUnits(var = varToGet, ncin.brick = ncin.brick) # fixes temp and precip units; assumes ncin.brick values are raw units
        indices <- format(as.Date(names(ncin.brick), format = "X%Y.%m.%d"), format = "%m")
        indices <- as.numeric(indices)
        print(paste("time to load brick: ", difftime(Sys.time(), startTime, units = "mins")))
        
        for (m in 1:nrow(IPCC_WG2_Ch5_crop_temperature_table)) {
          cropName <- as.character(IPCC_WG2_Ch5_crop_temperature_table[m, "Common name"])
          tLethal_max <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "Tlethal_max"])
          tLethal_min <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "Tlethal_min"])
          
          lowerOpt <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "Topt_min"])
          upperOpt <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "Topt_max"])
          #         print(paste("years covered:", yearSpan)), 
          print(paste0(cropName, ", lower optimum: ", lowerOpt, ", upper optimum: ", upperOpt, ", Lethal lower temp: ", tLethal_min, ", Lethal upper temp: ", tLethal_max))
          
          fileNameOut_lethal <- paste0("tLethalmax_", tLethal_max, "_", j, "_", i, "_", k, "_", filler, "_", spatialCoverageChoices, "_", yearSpan, ".tif")
          fileNameOut_suitability <- paste0("optTempRange", cropName, "_", j, "_", i, "_", k, "_", filler, "_", spatialCoverageChoices, "_", yearSpan, ".tif")
          startTime <-  Sys.time()
          no_cores <- parallel::detectCores() - 1
          raster::beginCluster(no_cores)
           monthLethalHighTempCount <- raster::stackApply(ncin.brick, indices, fun = function(x, ...){sum(x > tLethal_max)}, progress = "text") 
          names(monthLethalHighTempCount) <- month.abb
          print(paste("time to calc monthly lethal high: ", difftime(Sys.time(), startTime, units = "mins")))
          startTime <-  Sys.time()
          monthOptTempRangeCount <- stackApply(ncin.brick, indices, fun = function(x, ...){sum(x <  upperOpt & x > lowerOpt)}, progress = "text") 
          names(monthOptTempRangeCount) <- month.abb
          raster::endCluster()
          print(paste("time to calc monthly opt temp range: ", difftime(Sys.time(), startTime, units = "mins")))
          print(paste("Lethal high file name out: ", fileNameOut_lethal))
          print(paste("optTempRange file name out: ", fileNameOut_suitability))
          writeRaster(monthLethalHighTempCount, filename = paste0("data/", fileNameOut_lethal), format = "GTiff", overwrite = TRUE)
          writeRaster(monthoptTempRangepCount, filename = paste0("data/", fileNameOut_suitability), format = "GTiff", overwrite = TRUE)
        }
      }
    }
  }
}

library(RColorBrewer)
library(rworldmap)
library(colorspace)# use pal <- choose_palette() to see what this is about


# read the rasters back in
for (i in modelChoices) {
  for (j in variableChoices) {
    for (k in rcpChoices) {
      for (l in startyearChoices) {
        for (m in 1:nrow(IPCC_WG2_Ch5_crop_temperature_table)) {
          yearSpan <- paste0(l, startday, "_", l + yearRange, endday)
          fileNameIn <- paste(j, i, k, filler, spatialCoverageChoices, yearSpan, sep = "_")
          fileNameIn <- paste0("tLethalmax_", m, "_",fileNameIn)
          fileNameIn <- paste0("data/", fileNameIn, ".tif")
          temp <- brick(fileNameIn)
          names(temp) <- month.abb
          yearSpan_abb <- paste(l, "_", l + yearRange)
          
          titleText <- paste0("Lethal_tmax ", m, " C, ", yearSpan_abb, ", \nmodel = ", i, ", RCP = ", k)
          myat <- c(0, 50, 100, 150, 200, 250)
          g <- levelplot(temp, main = titleText, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red" ))
          g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
          plotFileName <- paste0("graphics/ldTmax_", m, " C ", i, "_", l, "_", j, "_", gsub(".", "_", k, fixed = TRUE), ".jpg")
          
          jpeg(plotFileName, width = 900, height = 700, quality = 100)
          print(g)
          dev.off()
        }
      }s
    }
  }
}

#------ section on crop suitability 
for (i in modelChoices) {
  for (j in variableChoices) {
    for (k in rcpChoices) {
      for (l in startyearChoices) {
        for (m in 1:nrow(IPCC_WG2_Ch5_crop_temperature_table)) {
          yearSpan <- paste0(l, startday, "_", l + yearRange, endday)
          fileNameIn <- paste(j, i, k, filler, spatialCoverageChoices, yearSpan, sep = "_")
          cropName <- as.character(IPCC_WG2_Ch5_crop_temperature_table[m, "Common name"])
          fileNameIn <- paste0(fileNameIn, ".nc")
          print(paste0("Working on : ", fileNameIn))
          temp <- paste0("data-raw/ISIMIP/", fileNameIn)
          ncin <- ncdf4::nc_open(temp)
          # get raster version of the data for a particular variable. This makes it easy to do raster math on the data.
          varToGet <- names(ncin[['var']])
          print(paste("variable name:", varToGet))
          print(paste("units:", ncin[["var"]][[varToGet]][["units"]]))
          print(paste("years covered:", yearSpan))
          lowerOpt <- IPCC_WG2_Ch5_crop_temperature_table[m, "Topt_min"]
          upperOpt <- IPCC_WG2_Ch5_crop_temperature_table[m, "Topt_max"]
          print(paste(cropName, "lower optimum:", lowerOpt, "upper optimum:", upperOpt))
          
          ncin.brick <- brick(temp, varname = varToGet) # because there is no explicit projection info in the netcdf files, this is assumed - +proj=longlat +datum=WGS84"
          print("created ncin.brick")
          ncin.brick <- readAll(ncin.brick) 
          ncin.brick <- fixUnits(var = varToGet, ncin.brick = ncin.brick) # fixes temp and precip units; assumes ncin.brick values are raw units
          indices <- format(as.Date(names(ncin.brick), format = "X%Y.%m.%d"), format = "%m")
          indices <- as.numeric(indices)
          
          monthTempCount <- stackApply(ncin.brick, indices, fun = function(x, ...){sum((x < upperOpt) & x > lowerOpt)}, progress = "text") 
          names(monthTempCount) <- month.abb
          print(paste("file name out: ", fileNameOut))
          writeRaster(monthTempCount, filename = paste0("data/", fileNameOut), format = "GTiff", overwrite = TRUE)
        }
      }
    }
  }
}

# read the rasters back in to plot optimum temperature range graphs
for (i in modelChoices) {
  for (j in variableChoices) {
    for (k in rcpChoices) {
      for (l in startyearChoices) {
        for (m in 1:nrow(IPCC_WG2_Ch5_crop_temperature_table)) {
          yearSpan <- paste0(l, startday, "-", l + yearRange, endday)
          fileNameIn <- paste(j, i, k, filler, spatialCoverageChoices, yearSpan, sep = "_")
          cropName <- as.character(IPCC_WG2_Ch5_crop_temperature_table[m, "Common name"])
          fileNameIn <- paste0("suitability_", cropName, "_",fileNameIn)
          fileNameIn <- paste0("data/", fileNameIn, ".tif")
          lowerOpt <- IPCC_WG2_Ch5_crop_temperature_table[m, "Topt_min"]
          upperOpt <- IPCC_WG2_Ch5_crop_temperature_table[m, "Topt_max"]
          
          temp <- brick(fileNameIn)
          names(temp) <- month.abb
          yearSpan_abb <- paste(l, "-", l + yearRange)
          
          titleText <- paste0("Regions with optimum temperature range (", lowerOpt, " - ", upperOpt, "C) for ", cropName, " ", yearSpan_abb, ", \nmodel = ", i, ", RCP = ", k)
          myat <- c(0, 50, 100, 150, 200, 250)
          g <- levelplot(temp, main = titleText, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red" ))
          g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
          plotFileName <- paste0("graphics/optTempRange_", cropName, i, "_", l, "_", j, "_", gsub(".", "_", k, fixed = TRUE), ".jpg")
          print(paste0("creating ", plotFileName))
          jpeg(plotFileName, width = 1800, height = 1400, quality = 100)
          print(g)
          dev.off()
        }
      }
    }
  }
}

monthLethalHighTempCount <- raster::stackApply(ncin.brick, indices, fun = sum(x > tLethal_max), progress = "text", timer = TRUE, export = "tLethal_max")

clusterR(ncin.brick, stackApply, args = list(fun = myFun, progress = "text", timer = TRUE, export = "tLethal_max"))
raster::endCluster()

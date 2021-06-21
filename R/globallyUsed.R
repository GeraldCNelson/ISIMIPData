#library(ncdf4)
#library(PCICt)
#library(ncdf4.helpers)
options("rgdal_show_exportToProj4_warnings"="none") # directions given with library(rgdal)
# library(rgdal) - commented out because it has a project function
#library(gdalUtils)
library(rgeos)
# library(sp) 
library(sf)
library(maps)
library(maptools)
library(data.table)
#library(raster)
library(rasterVis)
library(ggplot2)
library(readxl)
library(lubridate)

varNamesInfo <- as.data.table(read_excel("data-raw/varNamesLookup.xlsx"))

# data(wrld_simpl)
# wrld_land <- subset(wrld_simpl, !NAME == "Antarctica")
# wrld_land@bbox <- bbox(rbind(c(-180, -90), c(180, 90)))

#an alternative
# data(wrld_simpl)
# wrld_land <- subset(wrld_simpl, !NAME == "Antarctica")
# wrld_land <- as(wrld_land, "sf")
# new_bb = c(-180, -90, 180, 90)
# names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
# attr(new_bb, "class") = "bbox"
# attr(st_geometry(wrld_land), "bbox") = new_bb
# 
# borders <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
# world_outline <- as(st_geometry(borders), Class = "Spatial")

# rcpChoices <- c("rcp45", "rcp85") 
# modelChoices <- c("HadGEM2-ES", "GFDL-ESM2M",  "MIROC5", "IPSL-CM5A-LR", "modMean")
# modelChoices_short <- unlist(lapply(strsplit(modelChoices, "-"), `[[`, 1))
# climateVars <- c( "hurs_day", "tasmin_day", "tasmax_day")
# spatialCoverageChoices <- "landonly"
# startday <- "0101"
# endday <- "1231"
# yearRange <- 9

# create needed directories if they don't exist ------
# dataDirs <- list.dirs(path = "data")
# write.csv(paste0("data-raw/", dataDirs, col.names = false))
#graphicsDirs <- list.dirs(path = "graphics")


for (q in c("dataDirs.csv", "graphicsDirs.csv")) {
  dirList <- read.csv(paste0("data-raw/", q), header = FALSE)
  temp <- as.character(dirList$V1)
  for (r in 1:length(temp)) if (!dir.exists(temp[r])) dir.create(temp[r])
}

# tmpDirName <- paste0(locOfCMIP6tifFiles, "rasterTmp", Sys.getpid(), "/")

# gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
#                              pypath=NULL, readpoly=TRUE, quiet=TRUE) {
#   if (isTRUE(readpoly)) require(rgdal)
#   if (is.null(pypath)) {
#     pypath <- Sys.which('gdal_polygonize.py')
#   }
#   if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
#   owd <- getwd()
#   on.exit(setwd(owd))
#   setwd(dirname(pypath))
#   if (!is.null(outshape)) {
#     outshape <- sub('\\.shp$', '', outshape)
#     f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep = '.'))
#     if (any(f.exists))
#       stop(sprintf('File already exists: %s',
#                    toString(paste(outshape, c('shp', 'shx', 'dbf'),
#                                   sep = ' .')[f.exists])), call.=FALSE)
#   } else outshape <- tempfile()
#   if (is(x, 'Raster')) {
#     writeRaster(x, {f <- tempfile(fileext = '.tif')})
#     rastpath <- normalizePath(f)
#   } else if (is.character(x)) {
#     rastpath <- normalizePath(x)
#   } else stop('x must be a file path (character string), or a Raster object.')
#   system2('python', args = (sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
#                                     pypath, rastpath, gdalformat, outshape)))
#   if (isTRUE(readpoly)) {
#     shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose = !quiet)
#     return(shp) 
#   }
#   return(NULL)
# }

# source of crop temperature values
cropCharacteristics_annual <- as.data.table(read_excel("data-raw/crops/GCB_annual_crops_temp_requirements_11032021.xlsx", range = "A1:J21"))
cropCharacteristics_annual[, Crop := tolower(Crop)]
# cropCharacteristics_perennial <- as.data.table(read_excel("data-raw/crops/perennnial_crop_temp_table_summary_29_52020.xlsx", range = "A1:S10"))
# data.table::setnames(cropCharacteristics_perennial, old = names(cropCharacteristics_perennial), new = make.names(names(cropCharacteristics_perennial)))

# cropChoice_cereals <- cropCharacteristics_annual[Crop_type == "Cereal", Crop]
# cropChoice_pulses <- cropCharacteristics_annual[Crop_type == "Pulse", Crop]
# cropChoice_oilseeds <- cropCharacteristics_annual[Crop_type == "Oilseed", Crop]
# cropChoice_RnTubers <- cropCharacteristics_annual[Crop_type == "Root/tuber", Crop]
# cropChoice_vegetables <- cropCharacteristics_annual[Crop_type == "Vegetable", Crop]
# cropChoice_sugar <- cropCharacteristics_annual[Crop_type == "Sugar", Crop]
# cropChoice_perennials <- perennial_crop_temp_table[Crop_type == "Fruit and nuts", title]
# cropChoices <- c("cropChoice_cereals", "cropChoice_legumes", "cropChoice_oilseeds", "cropChoice_RnTubers", "cropChoice_vegetables", "cropChoice_sugar") # omitted until we get gdd coefficients, "cropChoice_perennials")


# THI formulas - now incorporated into a cpp file and run from the thiCalcs2.R script
# mostly from Lallo
# note: The Dry Bulb temperature, usually referred to as "air temperature", is the air property that is most commonly used. 
# When people refer to the temperature of the air they are normally referring to the dry bulb temperature.
# thi.cattle <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) # rh in %, tmin and tmax in C
# thi.sheep <- tmax - ((0.31 - (0.31 * (rh / 100))) * (tmax - 14.4)) # source: Marai, I. F. M., El-Darawany, A. A., Fadiel, A., and Abdel-Hafez, M. A. M. (2007). Physiological traits as affected by heat stress in sheep—A review. Small Rumin. Res. 71, 1–12. doi:10.1016/j.smallrumres.2006.10.003.
# #      thi.goat <- tmax - (0.55 - (0.0055 * (1 - rh) * (tmax - 14.4))) #Finocchiaro, R., van Kaam, J. B. C. H. M., Portolano, B., and Misztal, I. (2005). Effect of Heat Stress on Production of Mediterranean Dairy Sheep. J. Dairy Sci. 88, 1855–1864. doi:10.3168/jds.S0022-0302(05)72860-5.
# thi.goat <-  (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) # rh in %, tmin and tmax in C
# thi.yak <- (0.8 * tmax) + (rh / 100) * (tmax - 14.4) + 46.4 # Ta is air temperature in C
# thi.broiler <- 0.85 * tmax + 0.15 * tmin
# thi.layer <- 0.60 * tmax + 0.40 * tmin
# thi.chicken <- 0.60 * tmax + 0.40 * tmin # using broiler formula
# thi.pigs <- tmax - (0.55 - (0.0055 * rh) * (tmax - 14.5))


# formula.thi.cattle <- "(1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))" # rh in %, tmin and tmax in C
# formula.thi.sheep <- "tmax - ((0.31 - (0.31 * (rh / 100))) * (tmax - 14.4))" # source: Marai, I. F. M., El-Darawany, A. A., Fadiel, A., and Abdel-Hafez, M. A. M. (2007). Physiological traits as affected by heat stress in sheep—A review. Small Rumin. Res. 71, 1–12. doi:10.1016/j.smallrumres.2006.10.003.
# #      thi.goat <- tmax - (0.55 - (0.0055 * (1 - rh) * (tmax - 14.4))) #Finocchiaro, R., van Kaam, J. B. C. H. M., Portolano, B., and Misztal, I. (2005). Effect of Heat Stress on Production of Mediterranean Dairy Sheep. J. Dairy Sci. 88, 1855–1864. doi:10.3168/jds.S0022-0302(05)72860-5.
# formula.thi.goat <-  "(1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))" # rh in %, tmin and tmax in C
# formula.thi.yak <- "(0.8 * tmax) + (rh / 100) * (tmax - 14.4) + 46.4" # Ta is air temperature in C
# formula.thi.broiler <- "0.85 * tmax + 0.15 * tmin"
# formula.thi.layer <- "0.60 * tmax + 0.40 * tmin"
# formula.thi.chicken <- "0.60 * tmax + 0.40 * tmin" # using broiler formula
# formula.thi.pigs <- "tmax - (0.55 - (0.0055 * rh) * (tmax - 14.5))"

#list of fruits to be used

fruits <-  c("apple", "almond", "blueberry", "cherry", "grape", "walnut")

removedFruit <- c("almond", "apricot", "avocado", "berrynes", "cranberry", "currant", "persimmon", "sourcherry", "grapefruitetc", 
                  "lemonlime", "pistachio", "pear", "strawberry", "orange", "peachetc",  "rasberry",  "stonefruitnes")

clusterSetup <- function(varList, libList, useCores) {
  cl <- makeCluster(useCores,  outfile = "", , setup_strategy = "sequential", homogeneous = TRUE) # added homogeneous = TRUE because all the nodes are doing the same thing.
  registerDoParallel(cl, cores = useCores)
  if (!missing(libList)) {
    varList <- c(varList, "libList")
  }
  clusterExport(cl, varlist = c(varList))
  clusterEvalQ(cl, sapply(libList, require, character.only = TRUE))
  return(cl)
}

# no longer used because I'm saving the file names without the filler text.
# fixFiller <- function(i) {
#   if (i %in% c("GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR")) filler <- "r1i1p1f1_w5e5" # the bias correction method. "r1i1p1f1_w5e5" for all models except UKESM1-0-LL
#   if (i %in% c("UKESM1-0-LL")) filler <- "r1i1p1f2_w5e5" # the bias correction method. "r1i1p1f1_w5e5" for all models except UKESM1-0-LL
#   return(filler)
# }

# commented fixUnits out because reading from the unitsCorrected directory
# fixUnits <- function(var, ncin.brick) {
#   if (var %in% c("tasmax", "tasmin")) ncin.brick <- ncin.brick - 273.15
#   if (var %in% c("pr")) ncin.brick <- ncin.brick * 86400
#   return(ncin.brick)
# }

# # historical data names and locations
# 
# hurs.historical <- paste0(locOfCMIP6tifFiles,   "historical/ensemble/ensemble_historical_hurs_2001_2010.tif")
# tasmax.historical <- paste0(locOfCMIP6tifFiles, "historical/ensemble/ensemble_historical_tasmax_2001_2010.tif")
# tasmin.historical <- paste0(locOfCMIP6tifFiles, "historical/ensemble/ensemble_historical_tasmin_2001_2010.tif")
# pr.historical <- paste0(locOfCMIP6tifFiles, "historical/ensemble/ensemble_historical_pr_2001_2010.tif")
# tas.historical <- paste0(locOfCMIP6tifFiles, "historical/ensemble/ensemble_historical_tas_2001_2010.tif")
# historicallist <- c("hurs", "tasmax", "tasmin", "pr", "tas")

##' To check if files (incl. directories) are symbolic links:
is.symlink <- function(paths) isTRUE(nzchar(Sys.readlink(paths), keepNA = TRUE))
## will return all FALSE when the platform has no `readlink` system call.
is.symlink("/foo/bar")

# used in calculating cumulative growing degree days
gddSum <- function(i, v) {
  j <- !is.na(i[,1])
  r <- rep(NA, nrow(i))
  x <- cbind(i[j,,drop = FALSE], v[j,,drop = FALSE])
  r[j] <- apply(x, 1, function(y) sum(y[ (y[1]:y[2]) + 2 ] )) 
  r
}

getcropAreaYield <- function(cropName, dataType) { #dataType is area or yield
  tifZipUrl <-  " https://s3.us-east-2.amazonaws.com/earthstatdata/HarvestedAreaYield175Crops_Geotiff.zip"
  tifzipFile <- paste0("data-raw/crops/HarvestedAreaYield175Crops_Geotiff.zip")
  tifFileLoc <- "data-raw/crops/HarvestedAreaYield175Crops_Geotiff/GeoTiff/"
  tiffilecrop <- cropName
  if (dataType %in% "area") {
    tifcropFile <- paste0(tiffilecrop, "_HarvestedAreaHectares.tif")
  } else {tifcropFile <- paste0(tiffilecrop, "_YieldPerHectare.tif")
  }
  tifzipFilePath <- paste0("HarvestedAreaYield175Crops_Geotiff/GeoTiff/", tiffilecrop, "/", tifcropFile)
  #  tifOut <- unzip(zipfile = tifzipFile, files = tifzipFilePath)
  tifOut <- rast(paste0(tifFileLoc, tiffilecrop, "/", tifcropFile))
  return(tifOut)
}

# savepdf <- function(fileName) {
#   pdf(fileName)
# }
# 
# savepdf <- function(file, width, height, destDir) {
#   fname <- paste0(destDir, "/", file)
#   pdf(fname, width, height, pointsize = 10)
#   par( mar = c(0,0,0,0)) #mgp=c(2.2,0.45,0), tcl=-0.4, 
# }

# faster version
f.gdd <- function(cropMask, tas, tbase, tbase_max) {
  # testFun <- function(tmax, tmin){return((tmax+tmin)/2)}
  # system.time(tas <-  lapp(tmin, fun=testFun))
  # system.time(tas <- (tmax + tmin) / 2 - tbase)
  # tas[tas < 0] <- 0
  # #  tbase_max <- tbase_max - tbase
  # tas[tas > tbase_max] <- tbase_max
  print(system.time(gdd <- tas - tbase))
  print(system.time(gdd[is.na(cropMask), ] <- NA))
  gdd
}

# faster version with mask loading included
f.gdd_model <- function(cropMask, tas, tbase, tbase_max, m) {
  fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(m), ".tif")
  cropMask <- rast(fileNameMask.in)
  gdd <- tas - tbase
  gdd[gdd < 0] <- 0
  #  tbase_max <- tbase_max - tbase
  gdd[gdd > tbase_max] <- tbase_max
  gdd[cropMask == 0, ] <- NA
  gdd
}

f.gdd.clamped <- function(cropMask, tmin, tmax, tbase, tbase_max) {
  #	tbase_max  <- ifelse(tbase_max > 0, tbase_max, Inf) 
  tmax_clamped <- apply(tmax, 1, function(i) clamp(i, tbase, tbase_max, Values = TRUE))
  tmin_clamped <- apply(tmin, 1, function(i) clamp(i, tbase, tbase_max, Values = TRUE))
  r <- t((tmax_clamped + tmin_clamped) / 2 - tbase)
  r[cropMask == 0, ] <- NA
  r
}

gdd.f1 <- function(mask, tmin, tmax, tbase, tbase_max) {
  #	tbase_max  <- ifelse(tbase_max > 0, tbase_max, Inf) 
  tmax_clamped <- apply(tmax, 1, function(i) clamp(i, tbase, tbase_max))
  tmin_clamped <- apply(tmin, 1, function(i) clamp(i, tbase, tbase_max))
  r <- t((tmax_clamped + tmin_clamped) / 2 - tbase)
  r[is.na(mask), ] <- NA
  r
}

f.gdd.clamped_masked <- function(cropMask, tmin, tmax, tbase, tbase_max) {
  #	tbase_max  <- ifelse(tbase_max > 0, tbase_max, Inf) 
  tmin[cropMask == 0, ] <- NA
  tmax[cropMask == 0, ] <- NA
  tmin[tmin < tbase] <- tbase
  tmin[tmax >  tbase_max] <- tbase_max
  tmax_clamped <- apply(tmax, 1, function(i) clamp(i, tbase, tbase_max, Values = TRUE))
  tmin_clamped <- apply(tmin, 1, function(i) clamp(i, tbase, tbase_max, Values = TRUE))
  r <- t((tmax_clamped + tmin_clamped) / 2 - tbase)
  r[cropMask == 0, ] <- NA
  r
}

loadSpatialData <- function(dataVar) {
  print(dataVar)
  require(sf)
  #  browser()
  
  switch(dataVar,
         world = {outvar <- st_read("data-raw/regionInformation/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")},
         lakes = {outvar <- st_read("data-raw/regionInformation/ne_10m_lakes/ne_10m_lakes.shp")},
         #          roads = {outvar <- st_read("data-raw/regioninformation/roads10.shp")},
         roads = {outvar <- st_read("data-raw/regionInformation/ne_10m_roads/ne_10m_roads.shp")},
         #           cities = {outvar <- st_read("data-raw/regionInformation/cities10.shp")},
         populatedAreas = {outvar <- st_read("data-raw/regionInformation/ne_10m_populated_places/ne_10m_populated_places.shp")},
         #           urbanAreas = {outvar <- st_read("data-raw/regioninformation/ne_10m_urban_areas/ne_10m_urban_areas.shp") },
         protectedAreas2 = {outvar <- st_read("data-raw/regionInformation/WDPA_Aug2020-shapefile/WDPA_Aug2020-shapefile2/WDPA_Aug2020-shapefile-polygons.shp")},
         protectedAreas1 = {outvar <- st_read("data-raw/regionInformation/WDPA_Aug2020-shapefile/WDPA_Aug2020-shapefile1/WDPA_Aug2020-shapefile-polygons.shp")},
         protectedAreas0 = {outvar <- st_read("data-raw/regionInformation/WDPA_Aug2020-shapefile/WDPA_Aug2020-shapefile0/WDPA_Aug2020-shapefile-polygons.shp")},
         rivers = {outvar <- st_read("data-raw/regionInformation/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")}
         #           rivers = {outvar <- st_read("data-raw/regioninformation/rivers10.shp")}
  )
  return(outvar)
}

createRegionSpatialData <- function(dataVar, regionBox) { # needs to be run after loadSpatialData
  require(sf)
  if (exists("regionBox")) {
    switch(dataVar,
           world =          {outvar <- st_crop(world, regionBox)},
           lakes =          {outvar <- st_crop(lakes, regionBox)},
           rivers =         {outvar <- st_crop(rivers, regionBox)},
           roads =          {outvar <- st_crop(roads, regionBox)},
           cities =          {outvar <- st_crop(cities, regionBox)},
           populatedAreas =  {outvar <- st_crop(populatedAreas, regionBox)},
           urbanAreas =      {outvar <- st_crop(urbanAreas, regionBox)},
           protectedAreas2 = {outvar <- st_crop(protectedAreas2, regionBox)},
           protectedAreas1 = {outvar <- st_crop(protectedAreas1, regionBox)},
           protectedAreas0 = {outvar <- st_crop(protectedAreas0, regionBox)},
           
    )
    return(outvar)
  } else {stop("regionbox doesn't exist")}
}

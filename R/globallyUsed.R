Sys.setenv(PROJ_LIB = "/usr/local/Cellar/proj/7.0.0/share/proj") # use until the sf and gdal issues get sorted out. If you get the error pj_obj_create: Cannot find proj.db, check to see if the proj version (currently 7.0.0) has changed
library(ncdf4)
#library(PCICt)
#library(ncdf4.helpers)
library(raster)
library(rgdal)
library(gdalUtils)
library(rgeos)
library(sp)
library(sf)
library(maps)
library(maptools)
library(data.table)
library(rasterVis)
library(ggplot2)
library(readxl)
library(rworldmap)

data("coastsCoarse")

data(wrld_simpl)
wrld_land <- subset(wrld_simpl, !NAME == "Antarctica")
wrld_land@bbox <- bbox(rbind(c(-180, -90), c(180, 90)))

#an alternative
data(wrld_simpl)
wrld_land <- subset(wrld_simpl, !NAME == "Antarctica")
wrld_land <- as(wrld_land, "sf")
new_bb = c(-180, -90, 180, 90)
names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
attr(new_bb, "class") = "bbox"
attr(st_geometry(wrld_land), "bbox") = new_bb

borders <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
world_outline <- as(st_geometry(borders), Class = "Spatial")

# rcpChoices <- c("rcp45", "rcp85") 
# modelChoices <- c("HadGEM2-ES", "GFDL-ESM2M",  "MIROC5", "IPSL-CM5A-LR", "modMean")
# modelChoices_short <- unlist(lapply(strsplit(modelChoices, "-"), `[[`, 1))
# variableChoices <- c( "hurs_day", "tasmin_day", "tasmax_day")
# spatialCoverageChoices <- "landonly"
# startday <- "0101"
# endday <- "1231"
# yearRange <- 9

# create needed directories if they don't exist ------
# dataDirs <- list.dirs(path = "data")
# write.csv(paste0("data-raw/", dataDirs, col.names = false))
#graphicsDirs <- list.dirs(path = "graphics")

# function to identify operating system
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


for (j in c("dataDirs.csv", "graphicsDirs.csv")) {
  dirList <- read.csv(paste0("data-raw/", j), header = FALSE)
temp <- as.character(dirList$V1)
for (i in 1:length(temp)) if (!dir.exists(temp[i])) dir.create(temp[i])
}

# paths to manage large data sets across machines
if (get_os() %in% "osx") locOfCMIP6ncFiles <- "/Volumes/Extreme\ SSD/ISIMIP/cmip6/"
if (get_os() %in% c("Linux", "linux")) locOfCMIP6ncFiles <- "data-raw/ISIMIP/cmip6"
tmpDirName <- paste0(locOfCMIP6ncFiles, "rasterTmp", Sys.getpid(), "/")

gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep = '.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep = ' .')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext = '.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args = (sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                    pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose = !quiet)
    return(shp) 
  }
  return(NULL)
}


# THI formulas
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
# thi.swine <- tmax - (0.55 - (0.0055 * rh) * (tmax - 14.5))


formula.thi.cattle <- "(1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))" # rh in %, tmin and tmax in C
formula.thi.sheep <- "tmax - ((0.31 - (0.31 * (rh / 100))) * (tmax - 14.4))" # source: Marai, I. F. M., El-Darawany, A. A., Fadiel, A., and Abdel-Hafez, M. A. M. (2007). Physiological traits as affected by heat stress in sheep—A review. Small Rumin. Res. 71, 1–12. doi:10.1016/j.smallrumres.2006.10.003.
#      thi.goat <- tmax - (0.55 - (0.0055 * (1 - rh) * (tmax - 14.4))) #Finocchiaro, R., van Kaam, J. B. C. H. M., Portolano, B., and Misztal, I. (2005). Effect of Heat Stress on Production of Mediterranean Dairy Sheep. J. Dairy Sci. 88, 1855–1864. doi:10.3168/jds.S0022-0302(05)72860-5.
formula.thi.goat <-  "(1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))" # rh in %, tmin and tmax in C
formula.thi.yak <- "(0.8 * tmax) + (rh / 100) * (tmax - 14.4) + 46.4" # Ta is air temperature in C
formula.thi.broiler <- "0.85 * tmax + 0.15 * tmin"
formula.thi.layer <- "0.60 * tmax + 0.40 * tmin"
formula.thi.chicken <- "0.60 * tmax + 0.40 * tmin" # using broiler formula
formula.thi.swine <- "tmax - (0.55 - (0.0055 * rh) * (tmax - 14.5))"

clusterSetup <- function(varList, libList, useCores) {
  cl <- makeCluster(useCores,  outfile = "")
  registerDoParallel(cl)
  if (!missing(libList)) {
    varList <- c(varList, "libList")
  }
  clusterExport(cl, varlist = c(varList))
  clusterEvalQ(cl, sapply(libList, require, character.only = TRUE))
  return(cl)
}

fixFiller <- function(i) {
  if (i %in% c("GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR")) filler <- "r1i1p1f1_w5e5" # the bias correction method. "r1i1p1f1_w5e5" for all models except UKESM1-0-LL
  if (i %in% c("UKESM1-0-LL")) filler <- "r1i1p1f2_w5e5" # the bias correction method. "r1i1p1f1_w5e5" for all models except UKESM1-0-LL
  return(filler)
}

fixUnits <- function(var, ncin.brick) {
  if (var %in% c("tasmax", "tasmin")) ncin.brick <- ncin.brick - 273.15
  if (var %in% c("pr")) ncin.brick <- ncin.brick * 86400
  return(ncin.brick)
}

# observed data names and locations

hurs.observed <- paste0(locOfCMIP6ncFiles, "observed/gswp3-w5e5_obsclim_hurs_global_daily_2001_2010.nc")
tasmax.observed <- paste0(locOfCMIP6ncFiles, "observed/gswp3-w5e5_obsclim_tasmax_global_daily_2001_2010.nc")
tasmin.observed <- paste0(locOfCMIP6ncFiles, "observed/gswp3-w5e5_obsclim_tasmin_global_daily_2001_2010.nc")
pr.observed <- paste0(locOfCMIP6ncFiles, "observed/gswp3-w5e5_obsclim_pr_global_daily_2001_2010.nc")
observedlist <- c("hurs", "tasmax", "tasmin", "pr")

# note that these don't have the path to the file
rh.observed.cmip5 <- c("hurs_ewembi1_daily_1991_2000.nc", "hurs_ewembi1_daily_2001_2010.nc")
tmax.observed.cmip5 <- c("tasmax_ewembi1_daily_1991_2000.nc", "tasmax_ewembi1_daily_2001_2010.nc") 
tmin.observed.cmip5 <- c("tasmin_ewembi1_daily_1991_2000.nc", "tasmin_ewembi1_daily_2001_2010.nc")
observedlist <- c("rh.observed.cmip5", "tmax.observed.cmip5", "tmin.observed.cmip5")

##' To check if files (incl. directories) are symbolic links:
is.symlink <- function(paths) isTRUE(nzchar(Sys.readlink(paths), keepNA=TRUE))
## will return all FALSE when the platform has no `readlink` system call.
is.symlink("/foo/bar")




# test of heat stress package
library(HeatStress)
library(terra)
library(meteor)
# library(Rcpp)
# sourceCpp("R/cpp/dewpoint.cpp")

terraOptions(memfrac = 2,  ncopies = 1, progress = 10, tempdir =  "data/ISIMIP", verbose = FALSE) # need to use a relative path

source("R/ISIMIPconstants.R")
source("R/ISIMIPspatialConstants.R")

l <- 2041
k <- "ssp585"
yearRange <- 19
yearSpan <- paste0(l, "_", l + yearRange)
modelChoice <- "GFDL-ESM4"
modelChoice_lower <- tolower(modelChoice)

locOfClimFiles <- "climdata/"
fileName_tas <- paste0(locOfClimFiles,  modelChoice_lower, "_tas_", k, "_", yearSpan, ".tif")
fileName_hurs <-  paste0(locOfClimFiles,  modelChoice_lower, "_hurs_", k, "_", yearSpan, ".tif")
fileName_sfcwind <-  paste0(locOfClimFiles,  modelChoice_lower, "_sfcwind_", k, "_", yearSpan, ".tif")
fileName_rsds <-  paste0(locOfClimFiles,  modelChoice_lower, "_rsds_", k, "_", yearSpan, ".tif")
fileName_dewpoint_in <- paste0(locOfClimFiles,  modelChoice_lower, "_dwp_", k, "_", yearSpan, ".tif")

tas <- rast(fileName_tas)
hurs <- rast(fileName_hurs)
wind <- rast(fileName_sfcwind)
solar <- rast(fileName_rsds)
dewp <- rast(fileName_dewpoint_in)
lat <- init(tas, "y")
lon <- init(tas, "x")

# one year only
startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31") 
dates <- seq(as.Date(startDate), as.Date(endDate), 1)
indices <- seq(as.Date(startDate), as.Date(paste0(l, "-12-31") ), by = "days")
indicesChar <- paste0("X", dates)
tas_yr <- subset(tas, indicesChar)
hurs_yr <- subset(hurs, indicesChar)
wind_yr <- subset(wind, indicesChar)
solar_yr <- subset(solar, indicesChar)
dewp_yr <- subset(dewp, indicesChar)
lat <- init(tas_yr, "y")
lon <- init(tas_yr, "x")


# tas_cell <- as.numeric(extract(tas, cellNum))
# dewp_cell <- as.numeric(extract(dewp, cellNum))
# radiation_cell <- as.numeric(extract(radiation, cellNum))
# wind_cell <- as.numeric(extract(wind, cellNum))
# test <- wbgt.Liljegren(tas = tas_cell, dewp = dewp_cell, wind = wind_cell, radiation = radiation_cell, dates, lon, lat, tolerance = 1e-04, noNAs = FALSE, swap = FALSE, hour = FALSE)
# print(system.time(wbgt <- lapp(combined, f_wbgt, dates, lon, lat )))


# for dewpoint, see dewpointFileGenerator.R

combined <- sds(tas_yr, dewp_yr, wind_yr, solar_yr, lon, lat)
f_wbgt <- function (vtas, vdewp, vwind, vrad, vlon, vlat, dates) {
  print(vtas[1])
  if (is.na(vtas[1])) {return(vtas)}
  print(Sys.time())
  
  r <- vtas
  for (i in 1:nrow(vtas)) {
    print("i ", i, ", nrow(vtas): ", nrow(vtas))
    x <- wbgt.Liljegren(vtas[i,], vdewp[i,], vwind[i,], vrad[i,], dates, vlon[i,1], vlat[i,1])
    r[i,] <- x$data
  }
  r
}

print(system.time(out <- lapp(combined, fun = f_wbgt, dates)))

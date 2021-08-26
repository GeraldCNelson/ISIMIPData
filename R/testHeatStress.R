# test of heat stress package
library(HeatStress)
library(terra)
library(meteor)
if (rstudioapi::getActiveProject() == "/Users/gcn/Documents/workspace/testbed") setwd("/Users/gcn/Documents/workspace/ISIMIPData")
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

tas <- crop(tas, extent_noAntarctica)
hurs <- crop(hurs, extent_noAntarctica)
wind <- crop(wind, extent_noAntarctica)
solar <- crop(solar, extent_noAntarctica)
dewp <- crop(dewp, extent_noAntarctica)
lat <- crop(lat, extent_noAntarctica)
lon <- crop(lon, extent_noAntarctica)

# one year only
startDate <- paste0(l, "-01-01"); endDate <- paste0(l, "-12-31") 
dates <- seq(as.Date(startDate), as.Date(endDate), 1)
indices <- seq(as.Date(startDate), as.Date(paste0(l, "-12-31") ), by = "days")
indicesChar <- paste0("X", indices)
tas_yr <- subset(tas, indicesChar)
hurs_yr <- subset(hurs, indicesChar)
wind_yr <- subset(wind, indicesChar)
solar_yr <- subset(solar, indicesChar)
dewp_yr <- subset(dewp, indicesChar)
lat_yr <- init(tas_yr, "y")
lon_yr <- init(tas_yr, "x")


# tas_cell <- as.numeric(extract(tas, cellNum))
# dewp_cell <- as.numeric(extract(dewp, cellNum))
# radiation_cell <- as.numeric(extract(radiation, cellNum))
# wind_cell <- as.numeric(extract(wind, cellNum))
# test <- wbgt.Liljegren(tas = tas_cell, dewp = dewp_cell, wind = wind_cell, radiation = radiation_cell, dates, lon, lat, tolerance = 1e-04, noNAs = FALSE, swap = FALSE, hour = FALSE)
# print(system.time(wbgt <- lapp(combined, f_wbgt, dates, lon, lat )))


# for dewpoint, see dewpointFileGenerator.R

combined <- sds(tas_yr, dewp_yr, wind_yr, solar_yr, lon_yr, lat_yr)
f_wbgt <- function (vtas, vdewp, vwind, vrad, vlon, vlat, dates) {
  print(Sys.time())
  vtas_out <<- vtas
  # if (is.na(vtas[1, 1])) {
  #   #   print(Sys.time())
  #   return(vtas)
  # }
  
  r <- vtas
  not_na <- which(!is.na(vtas[,1]))
  for (i in not_na) {
    x <- wbgt.Liljegren(vtas[i,], vdewp[i,], vwind[i,], vrad[i,], dates, vlon[i,1], vlat[i,1])
    r[i,] <- x$data
  }
  r
}

print(system.time(out <- lapp(combined, fun = f_wbgt, dates)))

combined_c <- c(tas_yr, dewp_yr, wind_yr, solar_yr, lon_yr, lat_yr)
combined_smaller <- c(tas_yr, dewp_yr)
plot(tas_yr$`X2041-03-07`)
clickout <- click(combined_c, xy = TRUE, cell = TRUE, show = TRUE)
clickout_data <- clickout[4:length(clickout)]
daysInYear <- 365
clickout_tas <- clickout_data[1:daysInYear]
clickout_tdewp <- clickout_data[(daysInYear*1 + 1):(daysInYear*2)]
clickout_wind <- clickout_data[(daysInYear*2 + 1):(daysInYear*3)]
clickout_vrad <- clickout_data[(daysInYear*3 + 1):(daysInYear*4)]
lon_loc <- clickout$x
lat_loc <- clickout$y
wbgt.Liljegren(clickout_tas, clickout_tdewp, clickout_wind, clickout_vrad, dates, lon_loc, lat_loc)
anyNA(clickout_tas)
anyNA(clickout_tdewp)
anyNA(clickout_wind)
anyNA(clickout_vrad)



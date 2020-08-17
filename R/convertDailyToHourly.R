# generate hourly temp data
library(chillR)
library(terra)
terraOptions(memfrac = 1,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) 
locOfCMIP6ncFiles <- "data-raw/ISIMIP/cmip6/unitsCorrected/ssp585/"
yearRange <- 9
#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2051

latMin <- -60
latMax <- 60
lonMin <- -180
lonMax <- 180
testExtent <- ext(lonMin, lonMax, latMin, latMax)


print(paste0("model: ", i, " start year: ", l, " ssp: ", k, " pid: ", Sys.getpid(), " systime: ", Sys.time()))

modelName.lower <- tolower(i)
yearSpan <- paste0(l, "_", l + yearRange)

filePrefix.tmax <- paste0(i, "/", modelName.lower, "_", k, "_tasmax_global_daily_")
filePrefix.tmin <-  paste0(i, "/", modelName.lower, "_", k, "_tasmin_global_daily_")
filePrefix.rh <-  paste0(i, "/", modelName.lower, "_", k, "_hurs_global_daily_")
fileSuffix <- paste0(yearSpan, ".tif")

fileName.tmax <- paste0(locOfCMIP6ncFiles, filePrefix.tmax, fileSuffix)
fileName.tmin <- paste0(locOfCMIP6ncFiles, filePrefix.tmin, fileSuffix)
fileName.rh <- paste0(locOfCMIP6ncFiles, filePrefix.rh, fileSuffix)

#    print(fileName.tmax)
tmax <- rast(fileName.tmax)
print("tmax loaded")
#    print(fileName.tmin)
tmin <- rast(fileName.tmin)
print("tmin loaded")
print(fileName.rh)
rh <- rast(fileName.rh)

startDate <- paste0(l, "-01-01")
endDate <- paste0(l + yearRange, "-12-31")

indices <- seq(as.Date(startDate), as.Date(endDate), 1)
indices <- paste0("X", as.character(indices))
names(tmax) <- names(tmin) <- names(rh) <- indices

# crop files to -60 to 60 lat.
print(system.time(tmin_crop <- crop(tmin, testExtent)))
print(system.time(tmax_crop <- crop(tmax, testExtent)))

# now get just one year for testing
startDate <- paste0(l, "-01-01")
endDate <- paste0(l , "-12-31")
nlayers <- length(seq(as.Date(startDate), as.Date(endDate), 1))

tmax_l <- subset(tmax_crop, 1:nlayers)
tmin_l <- subset(tmin_crop, 1:nlayers)

# convert to data frame for use with chillR
print(system.time(tmax_l_df <- as.data.frame(tmax_l, xy = TRUE, long = TRUE)))
print(system.time(tmin_l_df <- as.data.frame(tmin_l, xy = TRUE, long = TRUE)))
names(tmax_l_df)[1:2] <- names(tmin_l_df)[1:2] <- c("lon", "lat")

library(tidyr)
tmax_l_long <- gather(tmax_l_df, datInfo, tmax, X2051.01.01:X2051.12.31, factor_key=TRUE)
foo <- data.frame(do.call('rbind', strsplit(as.character(tmax_l_long$col1),'.',fixed=TRUE)))
tmax_l_long$year <- strsplit(x = "tmax_l_long$col1", split = ".", fixed = TRUE)[1]
startYear <- l
endYear <- l + yearSpan

start_date = 1
end_date = 366
columns = c("Tmin", "Tmax")
end_at_present = TRUE)

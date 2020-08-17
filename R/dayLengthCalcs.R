# this script calculates the sunrise and sunset times and number of hours between them for each half degree grid for each day in the year variable 'yearForCalc', currently 2000 which is a leap year.
# The day length values should be identical for all cells at a given latitude around the world. The sunrise and sunset times wil shift with the time zone, set for GMT in the code below.
# the number of days * cells between -60 and 60 lat = 88206 for 2000.
library(suncalc)
library(chillR)
yearForCalc <- "2000"
startDate <- paste0(yearForCalc, "-01-01")
endDate <- paste0(yearForCalc, "-12-31")
indices <- seq(as.Date(startDate), as.Date(endDate), 1)

lon = 0
startLat <- -60
endLat <- 60

suncalcData <- data.frame(date = rep(indices, 241), lat = rep(seq(startLat, endLat, 0.5), length(date)), lon = 0)

suncalcResults <- getSunlightTimes(data = suncalcData,  keep = c("sunrise", "sunset"), tz = "GMT")

suncalcResults$hours <- as.numeric(round((suncalcResults$sunset -  suncalcResults$sunrise), 2))

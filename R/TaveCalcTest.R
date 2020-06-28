library(terra)
terraOptions(memfrac = 3,  progress = 10, tempdir =  "data/ISIMIP/")
tmax <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")

terra:::.mem_info(tmax, 1)
tmax <- NULL
tmax <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")
terra:::.mem_info(tmax, 1)

tmin <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmin_global_daily_2051_2060.nc")

tbase <- 2.6
tbase_max <- 18.4

system.time(tavg <- (tmax + tmin) / 2 - tbase)

# tmp <- rstk(list(tmin, tmax))
# system.time( tavg <- lapp(tmp, function(mn, mx){ (mn + mx) / 2 - tbase})  )

tminAsRaster <- raster::brick(tmin)

plot(tmin[[25]])

plot(tminAsRaster[[25]])

system.time(clamp(tmin, lower = tbase, upper = tbase_max, values = TRUE))

f.chillhrs <- function(tmin, tmax) {
  ch <- (7 - tmin)/(tmax - tmin)
  ch[tmin > 7] <- 0
  ch[tmax < 7 & tmin <= 7] <- 24
  ch
}

system.time(chillHrs <- f.chillhrs(tmin, tmax))
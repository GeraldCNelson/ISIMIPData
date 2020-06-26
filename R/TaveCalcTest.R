library(terra)
terraOptions(memfrac = 0.9,  progress = 10, tempdir =  "data/ISIMIP/")
tmax <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")
tmin <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmin_global_daily_2051_2060.nc")

tbase <- 2.6
tbase_max <- 18.4

system.time(tavg <- (tmax + tmin) / 2 - tbase)


tmp <- rstk(list(tmin, tmax))
system.time( tavg <- lapp(tmp, function(mn, mx){ (mn + mx) / 2 - tbase})  )
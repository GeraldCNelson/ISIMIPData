library(terra)
terraOptions(memfrac = 3,  progress = 10, tempdir =  "data/ISIMIP/")
tmax <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")

terra:::.mem_info(tmax, 1)

tmin <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmin_global_daily_2051_2060.nc")
terra:::.mem_info(tmin, 1)

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

#v2
f.chillhrs2 <- function(tmin, tmax) {
  ch <- pmax(0, (7 - tmin))/(tmax - tmin)
  ch[tmax < 7] <- 24
  ch
}

#v3
library(Rcpp)
cppFunction('std::vector<double> chill(std::vector<double> tmn, std::vector<double> tmx) {
size_t n = tmn.size();
std::vector<double> out(n);
for (size_t i=0; i<n; i++) {
if (tmx[i] < 7) {
out[i] = 24;
} else if (tmn[i] < 7) {
out[i] = (7 - tmn[i])/(tmx[i] - tmn[i]);
}
}
return out;
}')

tmp <- rstk(tmin, tmax)

system.time(chillHrs1 <- f.chillhrs(tmin, tmax))
system.time(chillHrs2 <- lapp(tmp, f.chillhrs2))
system.time(chillHrs3 <- lapp(tmp, chill))
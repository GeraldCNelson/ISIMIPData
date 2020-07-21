library(terra)
terraOptions(memfrac = 3,  progress = 10, tempdir =  "data/ISIMIP/", verbose = TRUE)
tmax <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")

terra:::.mem_info(tmax, 1)

tmin <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmin_global_daily_2051_2060.nc")
terra:::.mem_info(tmin, 1)

tbase <- 2.6
tbase_max <- 18.4

system.time(tavg <- (tmax + tmin) / 2)

system.time(tavg <- (tmax + tmin) / 2 - tbase)

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

system.time(chillHrs <- f.chillhrs(tmin, tmax))


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

tmp <- sds(tmin, tmax)

system.time(chillHrs1 <- f.chillhrs(tmin, tmax))
system.time(chillHrs2 <- lapp(tmp, f.chillhrs2))
system.time(chillHrs3 <- lapp(tmp, chill))

croppingCalendar <- rast("NAtemp/Wheat.crop.calendar.fill.nc")
wheatMask <- rast("NAtemp/rasterMask_wheat.tif")
croppingCalendar_masked<- mask(croppingCalendar, wheatMask)
crs(croppingCalendar) <- crs(wheatMask)

system.time(rastOut <- tmax - 273.15)
system.time(rastOut2 <- tmax  * 86400)

library(ncdf4)
nc_open("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")



s <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")
f <- "test.tif"
zf <- "test.zip"

print(system.time(a <- writeRaster(s, f, overwrite=TRUE))); file.info(f)$size; flush.console()
zip(zf, f); file.info(zf)$size

print(system.time(a <- writeRaster(s, f, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); file.info(f)$size; flush.console()
#[1] 34242
zip(zf, f); file.info(zf)$size

print(system.time(a <- writeRaster(s, f, overwrite=TRUE, wopt=list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=1", ZLEVEL = 1))))); file.info(f)$size; flush.console()
zip(zf, f); file.info(zf)$size
print(system.time(a <- writeRaster(s, f, overwrite=TRUE, wopt=list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", ZLEVEL = 6))))); file.info(f)$size; flush.console()
zip(zf, f); file.info(zf)$size




library(terra)
terraOptions(memfrac = 2,  progress = 10, tempdir =  "data/ISIMIP/", verbose = TRUE)
tmax <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")

terra:::.mem_info(tmax, 1)

tmin <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmin_global_daily_2051_2060.nc")
terra:::.mem_info(tmin, 1)

tbase <- 2.6
tbase_max <- 18.4

# print(system.time(tavg <- (tmax + tmin) / 2)); flush.console()
# print(system.time(tavgCor <- tavg - tbase)); flush.console()
# 
# print(system.time(tavg <- (tmax + tmin) / 2 - tbase)); flush.console()

tminAsRaster <- raster::brick(tmin)

plot(tmin[[25]])

plot(tminAsRaster[[25]])

print(system.time(clamp(tmin, lower = tbase, upper = tbase_max, values = TRUE))); flush.console()

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

print(system.time(chillHrs <- f.chillhrs(tmin, tmax)))


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

print(system.time(chillHrs1 <- f.chillhrs(tmin, tmax))); flush.console()
print(system.time(chillHrs2 <- lapp(tmp, f.chillhrs2))); flush.console()
print(system.time(chillHrs3 <- lapp(tmp, chill))); flush.console()

croppingCalendar <- rast("NAtemp/Wheat.crop.calendar.fill.nc")
wheatMask <- rast("NAtemp/rasterMask_wheat.tif")
croppingCalendar_masked<- mask(croppingCalendar, wheatMask)
crs(croppingCalendar) <- crs(wheatMask)

print(system.time(rastOut <- tmax - 273.15)); flush.console()
print(system.time(rastOut2 <- tmax  * 86400)); flush.console()

library(ncdf4)
nc_open("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")


# test of compression choices
s <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")
f <- "test.tif"
zf <- "test.zip"

print(system.time(a <- writeRaster(s, f, overwrite=TRUE))); flush.console()
zip(zf, f); file.info(zf)$size

print(system.time(a <- writeRaster(s, f, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); file.info(f)$size; flush.console()
#[1] 34242
zip(zf, f); file.info(zf)$size

print(system.time(a <- writeRaster(s, f, overwrite=TRUE, wopt=list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=1", ZLEVEL = 1))))); file.info(f)$size; flush.console()
zip(zf, f); file.info(zf)$size
print(system.time(a <- writeRaster(s, f, overwrite=TRUE, wopt=list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", ZLEVEL = 6))))); file.info(f)$size; flush.console()
zip(zf, f); file.info(zf)$size

# test of creating an extent and cropping with it
s <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")
library(rworldmap)
data("coastsCoarse")
locLat <- 67.2597
locLong <- 15.3846
extentRange <- 5 # a value of 2 means 2 of the units of the raster; if it is 1/2 degree cells, this would be 1 degree

#extent is  vector (length=4; order= xmin, xmax, ymin, ymax)
testExtent <- ext(locLong - extentRange, locLong + extentRange, locLat - extentRange, locLat + extentRange)
testCrop <- crop(s, testExtent)
plot(s, 40)
points(locLong, locLat)
plot(coastsCoarse, add = TRUE)
plot(testCrop, 40)
points(locLong, locLat)
text(locLong, locLat, "test", pos = 1) # 1 is below
plot(coastsCoarse, add = TRUE)

# areaSum <- area(tmax[[1]], sum = TRUE)
# areaWeight <- area(tmax, sum = FALSE)/areaSum #If the coordinate reference system is longitude/latitude the values returned are in square meter for area and meter for perimeter.
areaWeight <- area(tmax, sum = FALSE) / 1000000
# temp.unw <-  global(tmax, fun = "mean",na.rm = TRUE)
mean(temp$weighted_mean)
mean(temp.unw$mean)

# effect of project on scale bar
tmax <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")
plot(tmax[[50]])
scalebar(10000, xy = NULL, type = "bar", divs = 4, below = "kilometers", 
         lonlat = TRUE, adj=c(0.5, -0.5), lwd = 2)
crsRob <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
tmax_proj <- terra::project(tmax[[50]], crsRob)
plot(tmax_proj[[50]])
scalebar(10000, xy = NULL, type = "bar", divs = 4, below = "kilometers", 
         lonlat = TRUE, adj=c(0.7, -0.7), lwd = 2)

#growing degree days problem on linux
library(terra)
terraOptions(memfrac = 1,  progress = 10, tempdir =  "data/ISIMIP/", verbose = TRUE)
tave <- rast("data-raw/ISIMIP/cmip6/unitsCorrected/ssp585/UKESM1-0-LL/ukesm1-0-ll_ssp585_tave_global_daily_2051_2060.tif")
terra:::.mem_info(tave, 1)
print(system.time(tave <- tave * 1)); flush.console()
terra:::.mem_info(tave, 1)

i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2051
m <- "Wheat"
tbase <- 2.6
tbase_max <-  18.4

# from an earlier RH contribution
gdd.f3 <- function(tave, tbase, tbase_max) {
  tave[tave < 0] <- 0
  tbase_max <- tbase_max - tbase
  tave[tave > tbase_max] <- tbase_max
  tave
}

print(system.time(gdd <- app(tave, fun=gdd.f3(tave, tbase, tbase_max)))); flush.console()



print(system.time(gdd <- app(tave, fun=function(x){ 
  x[x > tbase_max] <- tbase_max
  y <- x - tbase
  y[y < 0] <- 0
  return(y)} ))); flush.console()

#do function elements separately
print(system.time(tave[tave > tbase_max] <- tbase_max)); flush.console()
print(system.time(gdd <- tave - tbase)); flush.console()
print(system.time(gdd[gdd < 0] <- 0)); flush.console()





# original function 
f1.gdd <- function(tmin, tmax, tbase, tbase_max) {

	overlayfunction_mask <- function(x,y) {
	  return(x * y)
	}

	gddFunction2 <- function(x, y, z) {
	   function(x, y) (x + y) / 2 - z
	}

	tmin_cropArea <- overlay(tmin, mask, fun = overlayfunction_mask)
	tmax_cropArea <- overlay(tmax, mask, fun = overlayfunction_mask)
	if (tbase_max > 0) {
		tmax_clamped <- clamp(tmax_cropArea, lower = tbase, upper = tbase_max, Values = TRUE)
		tmin_clamped <- clamp(tmin_cropArea, lower = tbase, upper = tbase_max, Values = TRUE)
	} else {
		tmax_clamped <- clamp(tmax_cropArea, lower = tbase, upper = Inf, Values = TRUE)
		tmin_clamped <- clamp(tmin_cropArea, lower = tbase, upper = Inf, Values = TRUE)
	}
	gdd <- overlay(x = tmax_clamped, y = tmin_clamped, fun = gddFunction2(x, y, z = tbase))
	gdd
}


# rewrite that takes vector/matrix as input (provided by overlay)
# all computations in one step. No temp files created.
gdd.f1 <- function(mask, tmin, tmax, tbase, tbase_max) {
#	tbase_max  <- ifelse(tbase_max > 0, tbase_max, Inf) 
	tmax_clamped <- apply(tmax, 1, function(i) clamp(i, tbase, tbase_max))
	tmin_clamped <- apply(tmin, 1, function(i) clamp(i, tbase, tbase_max))
	r <- t((tmax_clamped + tmin_clamped) / 2 - tbase)
	r[is.na(mask), ] <- NA
	r
}

# a variation on the above. Clamping is normally done on 
# the average temperature. That would save one step.
# clamping between 0 and (tbase_max - tbase)
gdd.f2 <- function(mask, tmin, tmax, tbase, tbase_max) {
#	tbase_max  <- ifelse(tbase_max > 0, tbase_max, Inf) 
	tavg <- (tmax + tmin) / 2 - tbase
	r <- t(apply(tavg, 1, function(i) clamp(i, 0, tbase_max - tbase)))
	r[is.na(mask), ] <- NA
	r
}

# faster version
gdd.f3 <- function(mask, tmin, tmax, tbase, tbase_max) {
	tavg <- (tmax + tmin) / 2 - tbase
	tavg[tavg < 0] <- 0
	tbase_max <- tbase_max - tbase
	tavg[tavg > tbase_max] <- tbase_max
	tavg[is.na(mask), ] <- NA
	tavg
}

 
fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(m), ".tif")
mask <- rast(fileNameMask.in)
#paste0(fileNameOut, ".tif")
# example data
library(raster)
r <- rast()
tmp <- 25 * sin(seq(0,pi,1/365)) - 5
tmin <- stack(lapply(1:365, function(i)setValues(r, tmp[i])))
tmin[seq(1,ncell(tmin),111)] <- NA
tmax <- tmin + 10
mask <- setValues(r, rep(c(NA,1), ncell(r)/2))
# 
# tb <- 5
# tbm <- 20

tb <- Tbase
tbm <- Tbase_max
system.time(x <- f1.gdd(tmin, tmax, tb, tbm) )
#   user  system elapsed 
#   9.81    2.51   12.64 
 
 
tbm  <- ifelse(tbm > 0, tbm, Inf) 
system.time(y <- overlay(mask, tmin, tmax, fun=function(x, y, z) gdd.f1(x, y, z, tb, tbm)))
#   user  system elapsed 
#   7.19    0.35    7.60 

system.time(z <- overlay(mask, tmin, tmax, fun=function(x, y, z) gdd.f2(x, y, z, tb, tbm)))
#   user  system elapsed 
#   4.46    0.19    4.68 
 
system.time(a <- overlay(mask, tmin, tmax, fun=function(x, y, z) gdd.f3(x, y, z, tb, tbm)))
#   user  system elapsed 
#   1.55    0.09    1.70 
 

# if you know you can keep the values in memory
system.time(x <- setValues(tmin, gdd.f3(values(mask), values(tmin), values(tmax), tb, tbm)))
#   user  system elapsed 
#   0.82    0.00    0.82 

# if you know you can keep the values in memory. Creates x1 with the gdd.f2 function
system.time(x1 <- setValues(tmin, gdd.f2(values(mask), values(tmin), values(tmax), tb, tbm)))
#   user  system elapsed 
#   0.82    0.00    0.82 

# if you know you can keep the values in memory. Creates x2 with the gdd.f1 function
system.time(x2 <- setValues(tmin, gdd.f1(values(mask), values(tmin), values(tmax), tb, tbm)))



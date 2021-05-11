# estimate dew point, and daily rhmin and rhmax
library(terra)

vapres <- function(t) { # t is either dew point temp or one of tmin/tmax
  #  Result in pascals NOT kPa
  # from Appendix D of Federal meteorological handbook No. 3. FCM-H3-1997. Office of Federal Coordinator for Meteorological Services and Supporting Research (OFCM), Washington DC, USA
  v <- 611.21 * exp(17.502 * t/(240.97 + t))
}

dewtab <- function(cellVector) {
  # ESTIMATING DEWPOINT FROM MEAN DAILY TEMPERATURE (T) AND     
  # DIURNAL TEMPERATURE RANGE. (R)                             
  # THIS FUNCTION APPROXIMATES VALUES IN THE TABLE 1            
  # ON PAGE 413 . LINACRE E.T. (1977) 'A SIMPLE FORMULA FOR      
  # ESTIMATING EVAPORATION RATES IN VARIOUS CLIMATES USING      
  # TEMPERATURE DATA ALONE.' AGRIC.METEOROL. 18:409-424         
  #  P.G.JONES CIAT 25-09-81  
if (is.na(cellVector[1])) {return(NA)}
  t <- cellVector[1]
  r <- cellVector[2]
#  if (is.nan(t[1][1])) { return(NA) }
  
  u <- t - 6                                                             
  d <- t # to initialize d
  # # Ed Linacre's table can't handle temperatures less than
  # # 6 degrees. Do the most conservative thing
  # if (t < 6) d = t/2
  # if(t < 0)
  # d = t		
  # return(d)
  
  d[t < 6] <- t/2
  d[t < 0 ] <- t
  # NOTE dewpoint does not really become negative, but this
  # this fits with the function edevap where we want
  # the term 15.0*(T-DEWTAB(T,R) to become zero for negative
  # temperatures. Otherwise we get excessively negative
  # evaps.
  v <- r - 6                                                              
  d <- 3 + 0.0677 * v * u     
  d[d/(4.518 - .3752*v) > 1] <- 3 + (1.4954*v + (.5774 + (.5444+.0419*u)*v)*u - 6.047) / (u + 5.54)
  d <- t-max(d,0.0) # copes with pathological diurnal temperate ranges
  return(d)
}

tmax81 <- rast("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/ensembleMn_mnthMn_10Yr_historical_tasmax_1981_1990.tif")
tmax91 <- rast("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/ensembleMn_mnthMn_10Yr_historical_tasmax_1991_2000.tif")
tmax01 <- rast("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/ensembleMn_mnthMn_10Yr_historical_tasmax_2001_2010.tif")

tmaxave <- c(tmax81, tmax91, tmax01)
tmaxave_IM <- tapp(tmaxave, names(tmaxave), mean)


tmin81 <- rast("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/ensembleMn_mnthMn_10Yr_historical_tasmin_1981_1990.tif")
tmin91 <- rast("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/ensembleMn_mnthMn_10Yr_historical_tasmin_1991_2000.tif")
tmin01 <- rast("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/ensembleMn_mnthMn_10Yr_historical_tasmin_2001_2010.tif")

tminave <- c(tmin81, tmin91, tmin01)
tminave_IM <- tapp(tminave, names(tminave), mean)

# test
# tmin <- 10
# tmax <- 30

r <- tmaxave_IM - tminave_IM
t <- (tmaxave_IM + tminave_IM)/2
cellVector <- sds(t, r)
print(system.time(dewPtTemp <- app(cellVector, dewtab))); flush.console()
vapPres_pa_dewPtTemp <- vapres(dewPtTemp)

RHmin <- 1000.0*vapPres_pa_dewPtTemp/vapres(tmaxave_IM)
RHmax <- 1000.0*vapPres_pa_dewPtTemp/vapres(tminave_IM)
RHmin[RHmin > 1000] <- 1000
RHmax[RHmax > 1000] <- 1000

RHmin <- RHmin/10
RHmax <- RHmax/10

RHmin_jan <- subset(RHmin, 1)
RHmin_jul <- subset(RHmin, 7)

RHmax_jan <- subset(RHmax, 1)
RHmax_jul <- subset(RHmax, 7)

vapPresTmax_1981_2010 <- vapres(tmaxave_IM)
print(system.time(writeRaster(vapPresTmax_1981_2010, "vapPresTmax_1981_2010.tif",  wopt= woptList, overwrite = TRUE))); flush.console()
print(system.time(writeRaster(dewPtTemp, "dewPtTemp.tif",  wopt= woptList, overwrite = TRUE))); flush.console()


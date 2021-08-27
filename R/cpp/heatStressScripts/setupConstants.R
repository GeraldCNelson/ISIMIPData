library(terra)
library(HeatStress)
library(Rcpp)
sourceCpp("R/cpp/wbgtFunctions.cpp", rebuild = TRUE)

# example data from HeatStress package
data("data_obs") 
head(data_obs)
dates_test <- data_obs$Dates
tas_test <- data_obs$tasmean
dewp_test <- data_obs$dewp
speed_test <- data_obs$wind
radiation_test <- data_obs$solar

for (i in 1:length(dewp_test)){
  if(is.na(dewp_test[i])) dewp_test[i] <- tas_test[i] - 10.5
  if(is.na(radiation_test[i])) radiation_test[i] <- 359.5
}

hurs_test =100*(exp((17.625*dewp_test)/(243.04+dewp_test))/exp((17.625*tas_test)/(243.04+tas_test))) 
dewp_test_compare = 243.04*(log(hurs_test/100)+((17.625*tas_test)/(243.04+tas_test)))/(17.625-log(hurs_test/100)-((17.625*tas_test)/(243.04+tas_test))) # should be the same as dewp_test

RH_test <- hurs_test * 0.01

r <- rast(nrows=5, ncols=5, xmin = -180, xmax = 180, ymin = -90, ymax = 90, nlyrs = 92) 
lat <- init(r, "y")
lon <- init(r, "x")
lat_test <- values(lat)[,1]
lon_test <- values(lon)[,1]
lat_test <- c(lat_test, lat_test, lat_test, lat_test[1:17])
lon_test <- c(lon_test, lon_test, lon_test, lon_test[1:17])
rads_test = seq(1,3, .3)
# constants- I'm not sure these are needed for the functions created in the cpp file
kVal = 273.15
Tk_test <- tas_test + kVal
Pair = 1010
min_speed = 0.1
# Globe constants
emis.globe <- 0.95 # emissivity
alb.globe <- 0.05 # albedo
#diam.globe <- 0.05 #0.05 = 50mm diam globe, in fTg function
diam.globe = 0.0508 # in fTnwb.R 
diam.wick = 0.007

# Surface constants
stefanb <- 0.000000056696
emis.sfc <- 0.999
alb.sfc <- SurfAlbedo <- 0.4
pcrit13 =  19.94585 #(36.4 * 218) ^ (1 / 3);
tcrit512 = 113.4662 #(132 * 647.3) ^ (5 / 12);
Tcrit12 = 292.3074 #(132 * 647.3) ^ 0.5;
Mmix =  0.3000463 #(1 / 28.97 + 1 / 18.015) ^ 0.5
r_gas = 8314.34
m_air = 28.97
cp <- 1003.5 # heat capacity at constant pressure of dry air
tolerance <- 1e-4
r_gas = 8314.34
m_air = 28.97
r_air <- r_gas / m_air
propDirect = 0.8
min.speed = 0.1
# units
#Tair and Tdew - temperature in Kelvin
# tas - temperature in C
# dewp vector of dewpoint temperature in degC.
# wind vector of wind speed in m/s.
# relh vector of relative humidity in \%.
# radiation vector of solar shortwave downwelling radiation in W/m2.
# propDirect proportion of direct radiation = direct/(diffuse + direct).
# zenith zenith angle in radians.
# SurfAlbedo (optional) albedo in the surface. Default: 0.4.
# RH - relh * 0.01; relh and hurs are in %# tas vector of temperature in degC.
# tolerance (optional) tolerance value for the iteration. Default: 1e-4.
# irad (optional): include radiation (1) or not (irad=0, psychrometric web bulb temp). Default: 1.
# diffusivity of water vapor in air, m2/s. See https:#www.sciencedirect.com/topics/earth-and-planetary-sciences/diffusion-coefficient or BSL, page 505
# h_cylinder_in_air - Convective heat transfer coefficient for a long cylinder, W/(m2 K)
# h_sphere_in_air - Convective heat transfer coefficient for flow around a sphere, W/(m2 K)
# h_evap - J/(kg K), for temperature in the range 283-313 K
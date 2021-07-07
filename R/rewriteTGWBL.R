
# all constants, raw or derived
propDirect <- 0.8
Pair <- 1010
MinWindSpeed <- 0.1
# Physical constants
stefanb <- 5.6696e-08

cp <- 1003.5 # heat capacity at constant pressure of dry air
m.air <- 28.97
m.h2o <- 18.015
r.gas <- 8314.34
r.air <- r.gas / m.air
ratio <- cp * m.air/ m.h2o
Pr <- cp / (cp + (1.25 * r.air))

# Globe constants
emis.globe <- 0.95 # emissivity
alb.globe <- 0.05 # albedo
diam.globe <- 0.0508 #0.05 = 50mm diam globe

# Surface constants
emis.sfc <- 0.999

emis.wick <- 0.95
alb.wick <- 0.4
diam.wick <- 0.007
len.wick <- 0.0254


wbgt.Liljegren <- function(tas, dewp, relh, wind, radiation, dates, lon, lat, tolerance = 1e-04, noNAs = TRUE, swap = FALSE, hour = FALSE) {
  ndates <- length(tas)
  Tnwb <- rep(NA, ndates)
  Tg <- rep(NA, ndates)
  xmask <- !is.na(tas + dewp + wind + radiation)
  if (noNAs & swap) {
    tastmp <- pmax(tas, dewp)
    tas <- tastmp
  }
  
  for (i in which(xmask)) {
    zenithDeg <- calZenith(dates[i], lon, lat, hour)
    ZenithAngle <- degToRad(zenithDeg)
    Tg[i] <- fTg(tas[i], relh[i], Pair, wind[i], MinWindSpeed, 
                 radiation[i], propDirect, ZenithAngle, tolerance = tolerance)
    
    Tnwb[i] <- fTnwb(tas[i], dewp[i], relh[i], Pair, wind[i], 
                     MinWindSpeed, radiation[i], propDirect, ZenithAngle, 
                     tolerance = tolerance)
    rm(zenithDeg, ZenithAngle)
  }
  wbgt <- list(data = 0.7 * Tnwb + 0.2 * Tg + 0.1 * tas, Tnwb = Tnwb, Tg = Tg)
  return(wbgt)
}

fTg <- function (tas, relh, Pair, wind, min.speed, radiation, propDirect, zenith, SurfAlbedo = 0.4, tolerance = 1e-04) {
  alb.sfc <- SurfAlbedo
  if (zenith <= 0) zenith <- 1e-10
  if (radiation > 0 & zenith > 1.57) zenith <- 1.57
  if (radiation > 15 & zenith > 1.54) zenith <- 1.54
  if (radiation > 900 & zenith > 1.52) zenith <- 1.52
  if (radiation < 10 & zenith == 1.57) radiation <- 0
  Tair <- tas + 273.15
  RH <- relh * 0.01
  cza <- cos(zenith)
  Tsfc <- Tair
  
  # Minimization (iteratively)
  opt <- stats::optimize(fr, range(Tair-2, Tair+10),Tair,Pair, tol=tolerance)
  Tg <- opt$minimum - 273.15
  return(Tg)
}

fTnwb <- function (tas, dewp, relh, Pair, wind, min.speed, radiation, propDirect, zenith, irad = 1, SurfAlbedo = 0.4, tolerance = 1e-04) {
  alb.sfc <- SurfAlbedo
  if (zenith <= 0) zenith <- 1e-10
  if (radiation > 0 & zenith > 1.57) zenith <- 1.57
  if (radiation > 15 & zenith > 1.54) zenith <- 1.54
  if (radiation > 900 & zenith > 1.52) zenith <- 1.52
  if (radiation < 10 & zenith == 1.57) radiation <- 0
  Tdew <- dewp + 273.15
  Tair <- tas + 273.15
  RH <- relh * 0.01
  eair <- RH * esat(Tair)
  emis.atm <- emis_atm(Tair, RH)
  Tsfc <- Tair
  density <- Pair * 100/(Tair * r.air)
  opt <- stats::optimize(fr, range(Tdew - 1, Tair + 1), Tair, Pair, tol = tolerance)
  Tnwb <- opt$minimum - 273.15
  return(Tnwb)
}

degToRad <- function(angleDeg){
  degToRad <- pi * angleDeg / 180
  return(degToRad)
}

calZenith <- function (dates, lon, lat, hour = FALSE) {
  EQTIME1 <- 229.18
  EQTIME2 <- 7.5e-05
  EQTIME3 <- 0.001868
  EQTIME4 <- 0.032077
  EQTIME5 <- 0.014615
  EQTIME6 <- 0.040849
  DECL1 <- 0.006918
  DECL2 <- 0.399912
  DECL3 <- 0.070257
  DECL4 <- 0.006758
  DECL5 <- 0.000907
  DECL6 <- 0.002697
  DECL7 <- 0.00148
  if (hour) {
    d0 <- strftime(dates, format = "%Y-%m-%d %H:%M:%S", usetz = TRUE, tz = "UTC")
    d1 <- strptime(d0, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    utc.hour <- as.numeric(format(d1, "%H"))
  }
  else {
    d1 <- strptime(dates, format = "%Y-%m-%d")
    utc.hour <- 12
  }
  year <- as.numeric(format(d1, "%Y"))
  doy <- as.numeric(strftime(d1, format = "%j"))
  if (is.leapyear(year)) 
    dpy = 366
  else dpy = 365
  RadLon <- degToRad(lon)
  RadLat <- degToRad(lat)
  Gamma <- 2 * pi * ((doy - 1) + (utc.hour/24))/dpy
  EquTime <- EQTIME1 * (EQTIME2 + EQTIME3 * cos(Gamma) - EQTIME4 * 
                          sin(Gamma) - EQTIME5 * cos(2 * Gamma) - EQTIME6 * sin(2 * Gamma))
  Decli <- DECL1 - DECL2 * cos(Gamma) + DECL3 * sin(Gamma) - 
    DECL4 * cos(2 * Gamma) + DECL5 * sin(2 * Gamma) - DECL6 * cos(3 * Gamma) + DECL7 * sin(3 * Gamma)
  TimeOffset <- 0
  TrueSolarTime <- (utc.hour * 60) + TimeOffset
  HaDeg <- ((TrueSolarTime/4) - 180)
  HaRad <- degToRad(HaDeg)
  CosZen <- (sin(RadLat) * sin(Decli) + cos(RadLat) * cos(Decli) * cos(HaRad))
  if (CosZen > 1) CosZen <- 1
  if (CosZen < -1) CosZen <- -1
  SZARad <- acos(CosZen)
  SZA <- radToDeg(SZARad)
  return(SZA)
}

is.leapyear <- function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

h_sphere_in_air <- function(Tk, Pair, speed, min.speed, diam.globe) {
  # Calculate the thermal conductivity of air, W/(m K)
  therm.con <- thermal_cond(Tk)
  
  # Density of the air
  density <- Pair * 100 / (r.air * Tk)
  if(speed < min.speed) speed <- min.speed
  
  # Reynolds number
  Re <- speed * density * diam.globe / viscosity(Tk)
  
  # Nusselt number
  Nu <- 2 + 0.6 * Re ^ 0.5 * Pr ^ 0.3333
  
  # Convective heat tranfer coefficient for flow around a sphere, W/(m2 K)
  h_sphere_in_air <- Nu * therm.con / diam.globe 
  
  return(h_sphere_in_air)
}

viscosity <- function(Tk){
  omega <- (Tk / 97 - 2.9) / 0.4 * (-0.034) + 1.048
  viscosity <- 0.0000026693 * (28.97 * Tk) ^ 0.5 / (3.617 ^ 2 * omega)
  return(viscosity)
}

h_evap <- function(Tk) {
evap <- (313.15 - Tk) / 30 * (-71100) + 2407300
return(evap)
}

esat <- function(Tk) {
  esat <- 6.1121 * exp(17.502 * (Tk - 273.15) / (Tk - 32.18))
  esat <- 1.004 * esat  #correction for moist air, if pressure is not available; for pressure > 800 mb
  return(esat)
}

h_cylinder_in_air <- function(Tk, Pair, speed, min.speed, diam.wick) {
  # Calculate the thermal conductivity of air, W/(m K)
  therm.con <- thermal_cond(Tk)
  
  # Density of the air
  density <- Pair * 100 / (r.air * Tk)
  if(speed < min.speed) speed <- min.speed
  
  # Reynolds number
  Re <- speed * density * diam.wick / viscosity(Tk)
  
  # Nusselt number
  Nu <- 0.281 * Re ^ 0.6 * Pr ^ 0.44
  
  # Convective heat transfer coefficient in W/(m2 K) for a long cylinder in cross flow
  h_cylinder_in_air <- Nu * therm.con / diam.wick  
  return(h_cylinder_in_air)
}

thermal_cond <- function(Tk) {
  # Calculate the thermal conductivity of air, W/(m K)
  therm.con <- (cp + 1.25 * r.air) * viscosity(Tk)
  return(therm.con)
}

diffusivity <- function(Tk, Pair) {
  diffusivity <- 0.000364 * (Tk / Tcrit12) ^ 2.334 * pcrit13 * tcrit512 * Mmix / (Pair / 1013.25) * 0.0001
  return(diffusivity)
}

fr <- function(Twb_prev, Tair, Pair, emis.wick, emis.sfc, radiation, wind, zenith, density, min.speed) {
  Tref <- 0.5 * (Twb_prev + Tair)
  Fatm <- stefanb * emis.wick * (0.5 * (emis.atm * Tair^4 + emis.sfc * Tsfc^4) - Twb_prev^4) + (1 - alb.wick) * 
    radiation * ((1 - propDirect) * (1 + 0.25 * diam.wick/len.wick) + ((tan(zenith)/3.1416) + 0.25 * diam.wick/len.wick) * propDirect + alb.sfc)
  Sc <- viscosity(Tair)/(density * diffusivity(Tref, Pair))
  h <- h_cylinder_in_air(Twb_prev, Pair, wind, min.speed, diam.wick)
  ewick <- esat(Twb_prev)
  evap <- h_evap(Twb_prev)
  Twb <- Tair - evap/ratio * (ewick - eair)/(Pair - ewick) * (Pr/Sc)^0.56 + Fatm/h * irad
  return(abs(Twb - Twb_prev))
}

emis_atm <- function(Tk, RH) {
  e <- RH * esat(Tk)
  emis_atm <- 0.575 * e ^ 0.143
  return(emis_atm)
}

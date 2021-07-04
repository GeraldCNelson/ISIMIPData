


wbgt.Liljegren <- function (tas, dewp, relh, wind, radiation, dates, lon, lat, tolerance = 1e-04, noNAs = TRUE, swap = FALSE, hour = FALSE) {
  propDirect <- 0.8
  Pair <- 1010
  MinWindSpeed <- 0.1
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
  stefanb <- 5.6696e-08
  cp <- 1003.5
  m.air <- 28.97
  m.h2o <- 18.015
  r.gas <- 8314.34
  r.air <- r.gas/m.air
  ratio <- cp * m.air/m.h2o
  Pr <- cp/(cp + (1.25 * r.air))
  emis.globe <- 0.95
  alb.globe <- 0.05
  diam.globe <- 0.05
  emis.sfc <- 0.999
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
  
  fr <- function(Tglobe_prev, Tair, Pair) {
    Tref <- 0.5 * (Tglobe_prev + Tair)
    h <- h_sphere_in_air(Tref, Pair, wind, min.speed, diam.globe)
    Tglobe <- (0.5 * (emis_atm(Tair, RH) * Tair^4 + emis.sfc * Tsfc^4) - h/(emis.globe * stefanb) * (Tglobe_prev - Tair) + 
                 radiation/(2 * emis.globe * stefanb) * (1 - alb.globe) * (propDirect * (1/(2 * cza) - 1) + 1 + alb.sfc))^0.25
    abs(Tglobe - Tglobe_prev)
  }
  opt <- stats::optimize(fr, range(Tair - 2, Tair + 10), Tair, Pair, tol = tolerance)
  Tg <- opt$minimum - 273.15
  return(Tg)
}

fTnwb <- function (tas, dewp, relh, Pair, wind, min.speed, radiation, propDirect, zenith, irad = 1, SurfAlbedo = 0.4, tolerance = 1e-04) {
  stefanb <- 5.6696e-08
  cp <- 1003.5
  m.air <- 28.97
  m.h2o <- 18.015
  r.gas <- 8314.34
  r.air <- r.gas/m.air
  ratio <- cp * m.air/m.h2o
  Pr <- cp/(cp + (1.25 * r.air))
  emis.wick <- 0.95
  alb.wick <- 0.4
  diam.wick <- 0.007
  len.wick <- 0.0254
  emis.globe <- 0.95
  alb.globe <- 0.05
  diam.globe <- 0.0508
  emis.sfc <- 0.999
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
  fr <- function(Twb_prev, Tair, Pair) {
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
  opt <- stats::optimize(fr, range(Tdew - 1, Tair + 1), Tair, Pair, tol = tolerance)
  Tnwb <- opt$minimum - 273.15
  return(Tnwb)
}
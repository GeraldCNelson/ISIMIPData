#' Calculation of the natural wet bulb temperature.
#' 
#' @param tas vector of temperature in degC.
#' @param dewp vector of dewpoint temperature in degC.
#' @param wind vector of wind speed in m/s.
#' @param relh vector of relative humidity in \%.
#' @param radiation vector of solar shortwave downwelling radiation in W/m2.
#' @param propDirect proportion of direct radiation = direct/(diffuse + direct).
#' @param zenith zenith angle in radians.
#' @param SurfAlbedo (optional) albedo in the surface. Default: 0.4.
#' @param tolerance (optional) tolerance value for the iteration. Default: 1e-4.
#' @param irad (optional): include radiation (1) or not (irad=0, psychrometric web bulb temp). Default: 1.
#' @inheritParams h_cylinder_in_air
#' 
#' @return Natural wet bulb globe temperature in degC.
#' @author Ana Casanueva (05.01.2017).
#' @details Original fortran code by James C. Liljegren, translated by Bruno Lemke into Visual Basic (VBA).
#' @export
#' 

fTnwbR <- function(tas, dewp, relh, Pair, wind, min.speed, radiation, propDirect, zenith, irad=1, SurfAlbedo=0.4, tolerance=1e-4){
  
  # Physical constants
  stefanb <- 0.000000056696
  cp <- 1003.5 # heat capaticy of dry air at constant pressure 
  m.air <- 28.97
  m.h2o <- 18.015
  r.gas <- 8314.34
  r.air <- r.gas / m.air
  ratio <- cp * m.air/ m.h2o
  Pr <- cp / (cp + (1.25 * r.air))
  
  # Wick constants
  emis.wick <- 0.95 # emissivity
  alb.wick <- 0.4 # albedo
  diam.wick <- 0.007 # diameter (in m)
  len.wick <- 0.0254 # length (in m)
  
  # Globe constants
  emis.globe <- 0.95 # emissivity
  alb.globe <- 0.05 # albedo
  diam.globe <- 0.0508 # diameter (in m)
  
  # Surface constants
  emis.sfc <- 0.999
  alb.sfc <- SurfAlbedo
  
  # Fix up out-of bounds problems with zenith
  # if(zenith <= 0) zenith <- 0.0000000001
  # if(radiation > 0 & zenith > 1.57) zenith <- 1.57 # 90°
  # if(radiation > 15 & zenith > 1.54)  zenith <- 1.54 # 88°
  # if(radiation > 900 & zenith > 1.52) zenith <- 1.52 # 87°
  # if(radiation < 10 & zenith == 1.57) radiation <- 0
  
  zenith[zenith <= 0] <- 0.0000000001
  zenith[radiation > 0 & zenith > 1.57]  <- 1.57 # 90°
  zenith[radiation > 15 & zenith > 1.54]   <- 1.54 # 88°
  zenith[radiation > 900 & zenith > 1.52]  <- 1.52 # 87°
  zenith[radiation < 10 & zenith == 1.57]  <- 0
  
  
  # Change units
  Tdew <- dewp + 273.15 # to Kelvin
  Tair <- tas + 273.15 # to Kelvin
  RH <- relh * 0.01 # to fraction
  
  # Calculate vapour pressure
  eair <- RH * esat(Tair) 
  
  # Calculate the atmospheric emissivity
  emis.atm <- emis_atm(Tair, RH)
  
  # Set values for iteration
  Tsfc <- Tair
  # Density of the air
  density <- Pair * 100 / (Tair * r.air)
  
  # Function to minimize
  fr <- function(Twb_prev, Tair, Pair, i) {  
    Tref <- 0.5 * (Twb_prev + Tair) # Evaluate properties at the average temperature
    
    # Radiative heating term	
    Fatm <- stefanb * emis.wick * (0.5 * (emis.atm * Tair ^ 4 + emis.sfc * Tsfc ^ 4) - Twb_prev ^ 4) + (1 - alb.wick) * radiation[i] * ((1 - propDirect) * (1 + 0.25 * diam.wick / len.wick) + ((tan(zenith[i]) / 3.1416) + 0.25 * diam.wick / len.wick) * propDirect + alb.sfc)
    
    # Schmidt number
    Sc <- viscosityR(Tair) / (density * diffusivityR(Tref, Pair)) 
    
    # Calculate the convective heat transfer coefficient for a long cylinder in cross flow
    h <- h_cylinder_in_airR(Twb_prev, Pair, wind, min.speed, diam.wick)
    
    # Calculate the saturation vapor pressure (hPa) over liquid water
    ewick <- esatR(Twb_prev)
 #   print(paste0("ewick" , ewick))
    
    # Calculate the heat of evaporation, J/(kg K)
    evap <- h_evapR(Twb_prev)
    
    # Calculate the natural wet bulb temperature
    Twb <- Tair - evap / ratio * (ewick - eair[i]) / (Pair - ewick) * (Pr / Sc[i]) ^ 0.56 + Fatm[i] / h[i] * irad
    # print(paste0("Tair" , Tair))
    # print(paste0("Twb_prev" , Twb_prev))
    # print(paste0("evap" , evap))
    # print(paste0("eair[i])" , eair[i]))
    # print(paste0("ewick[i])" , ewick[i]))
    # print(paste0("Pr " , Pr ))
    # print(paste0("Sc[i]" , Sc[i]))
    # print(paste0("irad" , irad))
    # 
    abs(Twb - Twb_prev)
    
  }
  
  # Minimization (iteratively)
  for (i in length(Tair)) {
    # print(paste0("range: ", range(Tdew[i]-1, Tair[i]+1)))
    # print(paste0("Tair[i]: ", Tair[i]))
    
    opt <- stats::optimize(fr, range(Tdew[i]-1, Tair[i]+1), Tair[i], Pair, i, tol=tolerance)
    opt <- opt$minimum - kVal
    return(opt)
  }
}
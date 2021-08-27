#' Calculation of the globe temperature.
#' 
#' Calculation of the globe temperature.
#' 
#' @inheritParams fTnwb 
#' @return Globe temperature in degC.
#' 
#' @author Ana Casanueva (05.01.2017).
#' @details Original fortran code by James C. Liljegren, translated by Bruno Lemke into Visual Basic (VBA).
#' @export
#' 


fTgR <- function(tas, relh, Pair, wind, min.speed, radiation, propDirect, 
                 zenith, SurfAlbedo=0.4, tolerance=1e-4){
  # Physical constants
  stefanb <- 0.000000056696
  cp <- 1003.5 # heat capaticy at constant pressure of dry air
  m.air <- 28.97
  m.h2o <- 18.015
  r.gas <- 8314.34
  r.air <- r.gas / m.air
  ratio <- cp * m.air/ m.h2o
  Pr <- cp / (cp + (1.25 * r.air))
  
  # Globe constants
  emis.globe <- 0.95 # emissivity
  alb.globe <- 0.05 # albedo
  # diam.globe <- 0.05 #0.05 = 50mm diam globe - original
  diam.globe <- 0.0508 
  
  # Surface constants
  emis.sfc <- 0.999
  alb.sfc <- SurfAlbedo
  
  # Fix up out-of bounds problems with zenith
  zenith[zenith <= 0] <- 0.0000000001
  zenith[radiation > 0 & zenith > 1.57]  <- 1.57 # 90°
  zenith[radiation > 15 & zenith > 1.54]   <- 1.54 # 88°
  zenith[radiation > 900 & zenith > 1.52]  <- 1.52 # 87°
  zenith[radiation < 10 & zenith == 1.57]  <- 0
  
  #  if(zenith <= 0) zenith <- 0.0000000001 - original
  # if(radiation > 0 & zenith > 1.57) zenith <- 1.57 # 90°
  # if(radiation > 15 & zenith > 1.54)  zenith <- 1.54 # 88°
  # if(radiation > 900 & zenith > 1.52) zenith <- 1.52 # 87°
  # if(radiation < 10 & zenith == 1.57) radiation <- 0 
  # 
  
  # Change units
  Tair <- tas + 273.15
  RH <- relh * 0.01
  
  # cosine of zenith angle
  cza <- cos(zenith)
  
  # Set values for iteration
  Tsfc <- Tair
  
  # Function to minimize
  #  fr <- function(Tglobe_prev,Tair,Pair) {  old
  fr <- function(Tglobe_prev,Tair, Pair, i) {  
    Tref <- 0.5 * (Tglobe_prev + Tair) # Evaluate properties at the average temperature
    # Calculate the convective heat transfer coefficient, W/(m2 K) for flow around a sphere.
    h <- h_sphere_in_airR(Tref, Pair, wind, min.speed, diam.globe)
    # Calculate the globe temperature
    Tglobe <- (0.5 * (emis_atm(Tair, RH[i]) * Tair ^ 4 + emis.sfc * Tsfc[i] ^ 4) - h[i] / (emis.globe * stefanb) * (Tglobe_prev - Tair) + radiation[i] / 
                 (2 * emis.globe * stefanb) * (1 - alb.globe) * (propDirect * (1 / (2 * cza[i]) - 1) + 1 + alb.sfc)) ^ 0.25
    
    returnVal <- abs(Tglobe - Tglobe_prev)
    # print(paste0("emis_atmR(Tair, RH[i]" , emis_atmR(Tair, RH[i])))
    # print(paste0("Tsfc[i]" , Tsfc[i]))
    # print(paste0("Tair ", Tair))
    # print(paste0("radiation[i] ", radiation[i]))
    # print(paste0("radiation[i] ", radiation[i]))
    # print(paste0("radiation[i] ", radiation[i]))
    # print(paste0("radiation[i] ", radiation[i]))
    # print(paste0("radiation[i] ", radiation[i]))
    # print(returnVal)
    return(returnVal)
  }
  
  # Minimization (iteratively)
  # original
  # opt <- stats::optimize(fr, range(Tair-2, Tair+10),Tair,Pair, tol=tolerance)
  # Tg <- opt$minimum - 273.15
  # 
  
  # opt <- vector(mode="numeric", length = length(Tair))
  for (i in length(Tair)) {
    # print(paste0("range: ", range(Tair[i]-2, Tair[i]+10)))
    # print(paste0("Tair[i]: ", Tair[i]))
    # 
    opt <- stats::optimize(fr, range(Tair[i]-2, Tair[i]+10),Tair[i], Pair, i, tol=tolerance)
    # print(paste0("opt ", opt))
    opt <- opt$minimum - kVal
    # print(paste0("opt$minimum", opt$minimum))
  }
  return(opt)
}


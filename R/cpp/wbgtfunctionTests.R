miceadds::source.all("R/cpp/heatStressScripts")

compareOutputs <- function(cpp_value, r_value) {
  if (FALSE %in% unique(cpp_value == r_value)) {
    cpp_value_false <- cpp_value[!cpp_value == r_value]
    r_value_false <- r_value[!cpp_value == r_value]
    emis_atm_out_diff_share <- (r_value_false - cpp_value_false)/r_value_false
    print("% differences")
    print(round(emis_atm_out_diff_share/100, 3))
    # ifelse(test = emis_atm_out_diff_share < .01, 
    #         yes = print(paste0("Difference less than .01", round(emis_atm_out_diff_share, 2))),
    #         no = print("Difference greater than .01")
    #)}
  } else {
    print("Identical values.")
  }
}
# test checkLeapYear ----
#  is_leapyear(dates_test) no longer a separate function
is_leapyearR(dates_test)

# test radToDegree -----
radToDeg(rads_test)

# test degToRad -----
degToRad(radToDeg(rads_test))

#test get_days -----
get_days(dates_test)

#test get_years -----
get_years(dates_test)

#test esat -----
# esat(Tk_test)
# esatR(Tk_test)
all.equal(esat(Tk_test), esatR(Tk_test))
  
# test emis_atm - needs kelvin temp -----
# emis_atm_out_c <- emis_atm(Tk_test, RH_test)
# emis_atm_out_r <- emis_atmR(Tk_test, RH_test)
all.equal( emis_atm(Tk_test, RH_test), emis_atmR(Tk_test, RH_test))

# test diffusivity - needs kelvin temp ------
# diffusivity(Tk_test)
# diffusivityR(Tk_test, Pair)
all.equal(diffusivity(Tk_test), diffusivityR(Tk_test, Pair))

# test viscosity -----
# viscosity(Tk_test)
# viscosityR(Tk_test)
all.equal(viscosity(Tk_test), viscosityR(Tk_test))
# test thermal_cond -----
# thermal_cond(Tk_test)
# thermal_condR(Tk_test)
all.equal(thermal_cond(Tk_test), thermal_condR(Tk_test))

# test h_sphere_in_air -----
# h_sphere_in_air(Tk_test, speed_test)
# h_sphere_in_airR(Tk_test, Pair, speed = speed_test, min.speed, diam.globe)
all.equal(h_sphere_in_air(Tk_test, speed_test), h_sphere_in_airR(Tk_test, Pair, speed = speed_test, min.speed, diam.globe))

# test h_cylinder_in_air -----
# h_cylinder_in_air(Tk_test, speed_test)
# h_cylinder_in_airR(Tk_test, Pair, speed = speed_test, min.speed, diam.wick)
all.equal(h_cylinder_in_air(Tk_test, speed_test), h_cylinder_in_airR(Tk_test, Pair, speed = speed_test, min.speed, diam.wick))

# test h_evap ------
# h_evap(Tk_test)
# h_evapR(Tk_test)
all.equal(h_evap(Tk_test), h_evapR(Tk_test))
          
# test calZenith -----
# calZenith(dates_test, lon_test, lat_test)
# calZenithR(dates_test, lon_test, lat_test)
all.equal(calZenith(dates_test, lon_test, lat_test), calZenithR(dates_test, lon_test, lat_test))

  
Twb_prev  <- 293.0 # semi-random choice so that fr_tnwb has a starting value
#fr_tnwb(Twb_prev, eair_test, tas_test, dewp_test, hurs_test, radiation_test, zenith_test, density_test, speed_test)
fr_tnwb(Twb_prev, tas_test, dewp_test, hurs_test, radiation_test, zenith_test, speed_test)

# optimization, fTnwb -----
opt_fTnwb <- function(dates_test, lon_test, lat_test, tas_test,  dewp_test,  hurs_test,  radiation_test,  speed_test) {
  zenith_test <- calZenith(dates_test, lon_test, lat_test)
  Tair_test = tas_test + kVal
  # temp <- fTnwb_prep(tas_test,  dewp_test,  hurs_test,  radiation_test,  speed_test,  zenith_test)
  # emis_atm_out <- temp$emis_atm_out
  # RH <- temp$RH
  # zenith <- temp$ZenithAngle
  # radiation <- temp$radiationMod
  # Tdew <- temp$Tdew
  # Tair <- temp$Tair
  eair_test <- RH_test * esat(Tk_test)
  density_test = Pair * 100 / (r_air * Tk_test);
  opt <- vector(mode="numeric", length = length(dates_test))
  for (i in 1: length(dates_test)) {
    temp <- stats::optimize(f = fr_tnwb, interval = range(dewp_test[i] - 1.0, Tair_test[i] + 1.0), tol = tolerance,   eair_test[i],  tas_test[i],  dewp_test[i],  hurs_test[i],  radiation_test[i],  zenith_test[i],  density_test[i],  speed_test[i])
    opt[i] <- temp$minimum - kVal
  }
  return(opt)
}

opt_fTnwb_out <- opt_fTnwb(dates_test, lon_test, lat_test, tas_test,  dewp_test,  hurs_test,  radiation_test,  speed_test)

# optimization, fTg -----
opt_fTg <- function(dates_test, lon_test, lat_test, speed_test, tas_test, hurs_test, radiation_test) {
  zenith_test <- calZenith(dates_test, lon_test, lat_test)
  Tair_test = tas_test + kVal
  # temp <- fTg_prep(tas_test, hurs_test, radiation_test, zenith_test)
  # cza <- temp$cza
  # RH <- temp$RH
  # Tair <- temp$Tair
  # radiationMod <- temp$radiationMod
  
  #  opt <- vector(mode="numeric", length = length(tas_test))
  for (i in 1: length(tas_test)) {
    print(paste0("range: ", range(Tair_test[i] - 2, Tair_test[i] + 10)))
    temp <- stats::optimize(f = fr_tg, interval = range(Tair_test[i] - 2, Tair_test[i] + 10), speed_test, tas_test, hurs_test, radiation_test, zenith_test, tol = tolerance)
    opt <- temp$minimum - kVal
  }
  return(opt)
}

# test fr_tg -----
# fr_tg is the function to be minimized in fTg.R
# Tglobe_prev is the value of Tair over which the optimization occurs. The range is Tair-2, Tair+10
Tglobe_prev <- 293.0 -  kVal  # semi-random choice so that fr_tg has a starting value for this test
zenith_test <- calZenith(dates_test, lon_test, lat_test)
fr_tg(Tglobe_prev, tas_test, hurs_test, speed_test, radiation_test, zenith_test)
fTgR(tas_test, hurs_test, Pair, wind = speed_test, min.speed, radiation_test, propDirect, zenith_test, SurfAlbedo=0.4, tolerance=1e-4)

# test fTnwbR -----
fTnwb(tas_test, dewp_test, hurs_test, Pair, wind = speed_test, min.speed, radiation_test, propDirect, zenith_test, irad=1, SurfAlbedo=0.4, tolerance=1e-4)
fTnwbR(tas_test, dewp_test, hurs_test, Pair, wind = speed_test, min.speed, radiation_test, propDirect, zenith_test, irad=1, SurfAlbedo=0.4, tolerance=1e-4)

# test wbgt.Liljegren -----
system.time(wbgt.Liljegren(tas_test, dewp=dewp_test, wind=speed_test, radiation=radiation_test, dates= dates_test, lon=lon_test, lat=lat_test))
system.time(wbgt.LiljegrenR(tas = tas_test, dewp = dewp_test, relh = hurs_test, wind = speed_test, radiation_test, dates_test, lon_test, lat_test, tolerance=1e-4, noNAs=TRUE, swap=FALSE, hour=FALSE))

#results using heatstress code
system.time(wbgt.LiljegrenNew(tas_test[1], dewp=dewp_test[1], relh = hurs_test[1], wind=speed_test[1], radiation=radiation_test[1], dates= dates_test[1], lon=lon_test[1], lat=lat_test[1], tolerance=1e-4, noNAs=TRUE, swap=FALSE, hour=FALSE))

# for comparison
system.time(wbgt.LiljegrenR(tas_test[1], dewp=dewp_test[1], relh = hurs_test[1], wind=speed_test[1], radiation=radiation_test[1], dates= dates_test[1], lon=lon_test[1], lat=lat_test[1], tolerance=1e-4, noNAs=TRUE, swap=FALSE, hour=FALSE))

# now try to run this with lapp

#test data
l <- 2041
k <- "ssp585"
yearRange <- 19
yearSpan <- paste0(l, "_", l + yearRange)
modelChoice <- "GFDL-ESM4"
modelChoice_lower <- tolower(modelChoice)
fileName_tas <- paste0(locOfClimFiles,  modelChoice_lower, "_tas_", k, "_", yearSpan, ".tif")
fileName_dewp <-  paste0(locOfClimFiles,  modelChoice_lower, "dewp", k, "_", yearSpan, ".tif")
fileName_hurs <-  paste0(locOfClimFiles,  modelChoice_lower, "_hurs_", k, "_", yearSpan, ".tif")
fileName_speed <-  paste0(locOfClimFiles,  modelChoice_lower, "_sfc_wind_", k, "_", yearSpan, ".tif")
fileName_radiation <-  paste0(locOfClimFiles,  modelChoice_lower, "_rsds_", k, "_", yearSpan, ".tif")

tas <- rast(fileName_tas)
dewp <- rast(fileName_dewp)
hurs <- rast(fileName_hurs)
speed <- rast(fileName_speed)
radiation <- rast(fileName_radiation)

wbgt_sds <- sds(tas, dewp, hurs, speed, radiation, dates, lon, lat)


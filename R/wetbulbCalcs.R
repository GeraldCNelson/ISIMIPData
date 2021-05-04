# working on wet bulb temperature
{
  library(terra)
  library(wbgt)
  
  get_os <- function() {
    sysinf <- Sys.info()
    if (!is.null(sysinf)) {
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else {## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  if (get_os() %in% "osx") {
    terraOptions(memfrac = 2, progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path, memfrac = .9,
  }else{
    terraOptions(memfrac = .6,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
  }
  
  terraOptions(memfrac = 2, progress = 0, tempdir =  "data/ISIMIP", verbose = FALSE)
  woptList <- list(gdal=c("COMPRESS=LZW"))
  woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))
  
  #locOfFiles <- locOfCMIP6tifFiles
  locOfFiles <- "data/bigFiles/"
  speciesChoice <- c("humans", "cattle", "goat", "pigs", "chicken", "sheep") 
  speciesChoice <- c("pigs", "sheep")
  sspChoices <- c("ssp126", "ssp585") 
  #sspChoices <- c("ssp585") 
  modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
  startYearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
  startYearChoices_historical <- c(1991)
  scenarioChoicesEnsemble <- c("historical", sspChoices)
  ext_noAntarctica <- ext(-180, 180, -60, 90)
  yearRange <- 19
  
  colorList <- (RColorBrewer::brewer.pal(5, "RdYlGn"))
  
  #test values
  i <- "UKESM1-0-LL"
  k <- "ssp585"
  l <- 2081
  yearNumber <- 2043
  
  # convert longitude to time difference to GMT
  td <- function(lon) {
    sec <- lon / 0.004167
    hrs <- round(sec/3600, 0)
  }
}
for (k in sspChoices) {
  #    k = "ssp126"
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices <- paste0("X", as.character(indices))
    indices_day <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
    indices_day <- as.numeric(indices_day)
    #    indices_month <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%b") # %j is month of the day of the year
    dayAll <- unique(indices_day)
    year <- rep(l, length(day))
    month <- rep(0, length(day))
    hour <- rep(12, length(day))
    urban <- rep(0, length(day))
    zspeed <- rep(2, length(day))
    dT <- rep(0, length(day))
    
    for (i in modelChoices) {
      modelName.lower <- tolower(i)
      # fileName_rh_mean <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      # # fileName_tmax_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      # # fileName_tmin_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      # fileName_tas_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
      # fileName_ps_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_ps_global_daily_", yearSpan, ".tif")
      # fileName_sfcWind_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_sfcWind_global_daily_", yearSpan, ".tif")
      # fileName_rsds_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_rsds_global_daily_", yearSpan, ".tif")
      
      rh <- rast(fileName_rh_mean)
      rh_df <- data.frame(x = as.numeric(), y = as.numeric(), relhum = as.numeric())
      for (i in 1:nlyr(rh)) {
        r <- rh[[i]]
        r_df <- as.data.frame(r, xy = TRUE)
        year <- rep(l, nrow(r_df))
        month <- rep(0, nrow(r_df))
        year <- rep(l, nrow(r_df))
        day <- rep(dayAll[i],nrow(r_df))
        hour <- rep(12, nrow(r_df))
        urban <- rep(0, nrow(r_df))
        zspeed <- rep(2, nrow(r_df))
        dT <- rep(0, nrow(r_df))
        
        names(r_df) <- c("lon", "lat", "relhum")
        r_df <- cbind(r_df, day, year, month, hour, urban, zspeed, dT)
        rh_df <- rbind(rh_df, r_df)
      }
    }
  }
}

#solar
solar <- rast(fileName_rsds_mean)

# UTCI calculation -----

# utci function from CimInd
#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2081
yearSpan <- paste0(l, "_", l + yearRange)

modelName.lower <- tolower(i)
locOfClimFiles <- "data/bigFiles/"
fileName_rh_mean <- paste0(locOfClimFiles, "dailyMn_20Yr_", modelName.lower, "_", k, "_hurs_", yearSpan, ".tif")
# fileName_tmax_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tasmax_", yearSpan, ".tif")
# fileName_tmin_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tasmin_", yearSpan, ".tif")
fileName_tas_mean <- paste0(locOfClimFiles,  "dailyMn_20Yr_", modelName.lower, "_", k, "_tas_", yearSpan, ".tif")
fileName_ps_mean <- paste0(locOfClimFiles,  "dailyMn_20Yr_", modelName.lower, "_", k, "_ps_", yearSpan, ".tif")
fileName_sfcWind_mean <- paste0(locOfClimFiles,  "dailyMn_20Yr_", modelName.lower, "_", k, "_sfcWind_", yearSpan, ".tif")
fileName_rsds_mean <- paste0(locOfClimFiles,  "dailyMn_20Yr_", modelName.lower, "_", k, "_rsds_", yearSpan, ".tif")

ta <- rast(fileName_tas_mean)
rh <- rast(fileName_rh_mean)
wind <- rast(fileName_sfcWind_mean)
indices_366 <- seq(1,366, 1)
indices_366 <- paste0("day_", indices_366)
names(ta) <- names(rh) <- names(wind) <- indices_366
fileName_out <- "temp.tif"
comb_clim <- sds(ta, rh, wind)
print(system.time(r_out <- lapp(comb_clim, f_utci, filename = fileName_out, overwrite = TRUE, wopt = woptList)))

# Ta       : air temperature, degree Celsius. This has a water saturation vapor pressure of 4.248kPA or 4,248 hPA
# according to https://www.weather.gov/epz/wxcalc_vaporpressure, if the air temp is 40 C, the saturated vapor pressure is 73.77hPa
# ehPa    : water vapour presure, hPa=hecto Pascal
# Tmrt   : mean radiant temperature, degree Celsius
# va10m  : wind speed 10 m above ground level in m/s

#find a cell with values. use click with cell = TRUE. One cell with values is 57,761

ta_cell <- unlist(ta[57761])
rh_cell <- unlist(rh[57761])
wind_cell <- unlist(wind[57761])
test <- f_utci(ta_cell, rh_cell, wind_cell, na.rm = FALSE)

f_utci <- function (ta, rh, wind, na.rm = FALSE) {
  #  print(paste0("ta1: ", ta[1]))
  #  if (is.na(ta[1])) {return(NA)}
  
  satVapPres <- es(ta) # in hPa
  pa <- satVapPres * rh/100 # vapor pressure in hPa
  # print(paste0("VapPres: ", round(pa[1],2)))
  pa <- pa/10 # vapor pressure in kPa
  # print(paste0("rh: ", rh[1]))
  # 
  va = wind 
  
  va[va < 0.5] = 0.5
  va[va > 17] = 17
  dtm = 0 # when no data for tmrt
  utci <- 
    ta + 
    0.607562052 + 
    -0.0227712343 * ta + 
    0.000806470249 * ta^2 + 
    -0.000154271372 * ta^3 + 
    -3.24651735e-06 * ta^4 + 
    7.32602852e-08 * ta^5 + 
    1.35959073e-09 * ta^6 + 
    -2.2583652 * va + 
    0.0880326035 * ta * va + 
    0.00216844454 * ta^2 * va + 
    -1.53347087e-05 * ta^3 * va + 
    -5.72983704e-07 * ta^4 * va + 
    -2.55090145e-09 * ta^5 * va + 
    -0.751269505 * va^2 + 
    -0.00408350271 * ta * va^2 + 
    -5.21670675e-05 * ta^2 * va^2 + 
    1.94544667e-06 * ta^3 * va^2 + 
    1.14099531e-08 * ta^4 * va^2 + 
    0.158137256 * va^3 + 
    -6.57263143e-05 * ta * va^3 + 
    2.22697524e-07 * ta^2 * va^3 + 
    -4.16117031e-08 * ta^3 * va^3 + 
    -0.0127762753 * va^4 + 
    9.66891875e-06 * ta * va^4 + 
    2.52785852e-09 * ta^2 * va^4 + 
    0.000456306672 * va^5 + 
    -1.74202546e-07 * ta * va^5 + 
    -5.91491269e-06 * va^6 + 
    0.398374029 * dtm + 
    0.000183945314 * ta * dtm + 
    -0.00017375451 * ta^2 * dtm + 
    -7.60781159e-07 * ta^3 * dtm + 
    3.77830287e-08 * ta^4 * dtm + 
    5.43079673e-10 * ta^5 * dtm + 
    -0.0200518269 * va * dtm + 
    0.000892859837 * ta * va * dtm + 
    3.45433048e-06 * ta^2 * va * dtm + 
    -3.77925774e-07 * ta^3 * va * dtm + 
    -1.69699377e-09 * ta^4 * va * dtm + 
    0.000169992415 * va^2 * dtm + 
    -4.99204314e-05 * ta * va^2 * dtm + 
    2.47417178e-07 * ta^2 * va^2 * dtm + 
    1.07596466e-08 * ta^3 * va^2 * dtm + 
    8.49242932e-05 * va^3 * dtm + 
    1.35191328e-06 * ta * va^3 * dtm + 
    -6.21531254e-09 * ta^2 * va^3 * dtm + 
    -4.99410301e-06 * va^4 * dtm + 
    -1.89489258e-08 * ta * va^4 * dtm + 
    8.15300114e-08 * va^5 * dtm + 
    0.00075504309 * dtm^2 + 
    -5.65095215e-05 * ta * dtm^2 + 
    -4.52166564e-07 * ta^2 * dtm^2 + 
    2.46688878e-08 * ta^3 * dtm^2 + 
    2.42674348e-10 * ta^4 * dtm^2 + 
    0.00015454725 * va * dtm^2 + 
    5.2411097e-06 * ta * va * dtm^2 + 
    -8.75874982e-08 * ta^2 * va * dtm^2 + 
    -1.50743064e-09 * ta^3 * va * dtm^2 + 
    -1.56236307e-05 * va^2 * dtm^2 + 
    -1.33895614e-07 * ta * va^2 * dtm^2 + 
    2.49709824e-09 * ta^2 * va^2 * dtm^2 + 
    6.51711721e-07 * va^3 * dtm^2 + 
    1.94960053e-09 * ta * va^3 * dtm^2 + 
    -1.00361113e-08 * va^4 * dtm^2 + 
    -1.21206673e-05 * dtm^3 + 
    -2.1820366e-07 * ta * dtm^3 + 
    7.51269482e-09 * ta^2 * dtm^3 + 
    9.79063848e-11 * ta^3 * dtm^3 + 
    1.25006734e-06 * va * dtm^3 + 
    -1.81584736e-09 * ta * va * dtm^3 + 
    -3.52197671e-10 * ta^2 * va * dtm^3 + 
    -3.3651463e-08 * va^2 * dtm^3 + 
    1.35908359e-10 * ta * va^2 * dtm^3 + 
    4.1703262e-10 * va^3 * dtm^3 + 
    -1.30369025e-09 * dtm^4 + 
    4.13908461e-10 * ta * dtm^4 + 
    9.22652254e-12 * ta^2 * dtm^4 + 
    -5.08220384e-09 * va * dtm^4 + 
    -2.24730961e-11 * ta * va * dtm^4 + 
    1.17139133e-10 * va^2 * dtm^4 + 
    6.62154879e-10 * dtm^5 + 
    4.0386326e-13 * ta * dtm^5 + 
    1.95087203e-12 * va * dtm^5 + 
    -4.73602469e-12 * dtm^6 + 
    5.12733497 * pa + 
    -0.312788561 * ta * pa + 
    -0.0196701861 * ta^2 * pa + 
    0.00099969087 * ta^3 * pa + 
    9.51738512e-06 * ta^4 * pa + 
    -4.66426341e-07 * ta^5 * pa + 
    0.548050612 * va * pa + 
    -0.00330552823 * ta * va * pa + 
    -0.0016411944 * ta^2 * va * pa + 
    -5.16670694e-06 * ta^3 * va * pa + 
    9.52692432e-07 * ta^4 * va * pa + 
    -0.0429223622 * va^2 * pa + 
    0.00500845667 * ta * va^2 * pa + 
    1.00601257e-06 * ta^2 * va^2 * pa + 
    -1.81748644e-06 * ta^3 * va^2 * pa + 
    -0.00125813502 * va^3 * pa + 
    -0.000179330391 * ta * va^3 * pa + 
    2.34994441e-06 * ta^2 * va^3 * pa + 
    0.000129735808 * va^4 * pa + 
    1.2906487e-06 * ta * va^4 * pa + 
    -2.28558686e-06 * va^5 * pa + 
    -0.0369476348 * dtm * pa + 
    0.00162325322 * ta * dtm * pa + 
    -3.1427968e-05 * ta^2 * dtm * pa + 
    2.59835559e-06 * ta^3 * dtm * pa + 
    -4.77136523e-08 * ta^4 * dtm * pa + 
    0.0086420339 * va * dtm * pa + 
    -0.000687405181 * ta * va * dtm * pa + 
    -9.13863872e-06 * ta^2 * va * dtm * pa + 
    5.15916806e-07 * ta^3 * va * dtm * pa + 
    -3.59217476e-05 * va^2 * dtm * pa + 
    3.28696511e-05 * ta * va^2 * dtm * pa + 
    -7.10542454e-07 * ta^2 * va^2 * dtm * pa + 
    -1.243823e-05 * va^3 * dtm * pa + 
    -7.385844e-09 * ta * va^3 * dtm * pa + 
    2.20609296e-07 * va^4 * dtm * pa + 
    -0.00073246918 * dtm^2 * pa + 
    -1.87381964e-05 * ta * dtm^2 * pa + 
    4.80925239e-06 * ta^2 * dtm^2 * pa + 
    -8.7549204e-08 * ta^3 * dtm^2 * pa + 
    2.7786293e-05 * va * dtm^2 * pa + 
    -5.06004592e-06 * ta * va * dtm^2 * pa + 
    1.14325367e-07 * ta^2 * va * dtm^2 * pa + 
    2.53016723e-06 * va^2 * dtm^2 * pa + 
    -1.72857035e-08 * ta * va^2 * dtm^2 * pa + 
    -3.95079398e-08 * va^3 * dtm^2 * pa + 
    -3.59413173e-07 * dtm^3 * pa + 
    7.04388046e-07 * ta * dtm^3 * pa + 
    -1.89309167e-08 * ta^2 * dtm^3 * pa + 
    -4.79768731e-07 * va * dtm^3 * pa + 
    7.96079978e-09 * ta * va * dtm^3 * pa + 
    1.62897058e-09 * va^2 * dtm^3 * pa + 
    3.94367674e-08 * dtm^4 * pa + 
    -1.18566247e-09 * ta * dtm^4 * pa + 
    3.34678041e-10 * va * dtm^4 * pa + 
    -1.15606447e-10 * dtm^5 * pa + 
    -2.80626406 * pa^2 + 
    0.548712484 * ta * pa^2 + 
    -0.0039942841 * ta^2 * pa^2 + 
    -0.000954009191 * ta^3 * pa^2 + 
    1.93090978e-05 * ta^4 * pa^2 + 
    -0.308806365 * va * pa^2 + 
    0.0116952364 * ta * va * pa^2 + 
    0.000495271903 * ta^2 * va * pa^2 + 
    -1.90710882e-05 * ta^3 * va * pa^2 + 
    0.00210787756 * va^2 * pa^2 + 
    -0.000698445738 * ta * va^2 * pa^2 + 
    2.30109073e-05 * ta^2 * va^2 * pa^2 + 
    0.00041785659 * va^3 * pa^2 + 
    -1.27043871e-05 * ta * va^3 * pa^2 + 
    -3.04620472e-06 * va^4 * pa^2 + 
    0.0514507424 * dtm * pa^2 + 
    -0.00432510997 * ta * dtm * pa^2 + 
    8.99281156e-05 * ta^2 * dtm * pa^2 + 
    -7.14663943e-07 * ta^3 * dtm * pa^2 + 
    -0.000266016305 * va * dtm * pa^2 + 
    0.000263789586 * ta * va * dtm * pa^2 + 
    -7.01199003e-06 * ta^2 * va * dtm * pa^2 + 
    -0.000106823306 * va^2 * dtm * pa^2 + 
    3.61341136e-06 * ta * va^2 * dtm * pa^2 + 
    2.29748967e-07 * va^3 * dtm * pa^2 + 
    0.000304788893 * dtm^2 * pa^2 + 
    -6.42070836e-05 * ta * dtm^2 * pa^2 + 
    1.16257971e-06 * ta^2 * dtm^2 * pa^2 + 
    7.68023384e-06 * va * dtm^2 * pa^2 + 
    -5.47446896e-07 * ta * va * dtm^2 * pa^2 + 
    -3.5993791e-08 * va^2 * dtm^2 * pa^2 + 
    -4.36497725e-06 * dtm^3 * pa^2 + 
    1.68737969e-07 * ta * dtm^3 * pa^2 + 
    2.67489271e-08 * va * dtm^3 * pa^2 + 
    3.23926897e-09 * dtm^4 * pa^2 + 
    -0.0353874123 * pa^3 + 
    -0.22120119 * ta * pa^3 + 
    0.0155126038 * ta^2 * pa^3 + 
    -0.000263917279 * ta^3 * pa^3 + 
    0.0453433455 * va * pa^3 + 
    -0.00432943862 * ta * va * pa^3 + 
    0.000145389826 * ta^2 * va * pa^3 + 
    0.00021750861 * va^2 * pa^3 + 
    -6.66724702e-05 * ta * va^2 * pa^3 + 
    3.3321714e-05 * va^3 * pa^3 + 
    -0.00226921615 * dtm * pa^3 + 
    0.000380261982 * ta * dtm * pa^3 + 
    -5.45314314e-09 * ta^2 * dtm * pa^3 + 
    -0.000796355448 * va * dtm * pa^3 + 
    2.53458034e-05 * ta * va * dtm * pa^3 + 
    -6.31223658e-06 * va^2 * dtm * pa^3 + 
    0.000302122035 * dtm^2 * pa^3 + 
    -4.77403547e-06 * ta * dtm^2 * pa^3 + 
    1.73825715e-06 * va * dtm^2 * pa^3 + 
    -4.09087898e-07 * dtm^3 * pa^3 + 
    0.614155345 * pa^4 + 
    -0.0616755931 * ta * pa^4 + 
    0.00133374846 * ta^2 * pa^4 + 
    0.00355375387 * va * pa^4 + 
    -0.000513027851 * ta * va * pa^4 + 
    0.000102449757 * va^2 * pa^4 + 
    -0.00148526421 * dtm * pa^4 + 
    -4.11469183e-05 * ta * dtm * pa^4 + 
    -6.80434415e-06 * va * dtm * pa^4 + 
    -9.77675906e-06 * dtm^2 * pa^4 + 
    0.0882773108 * pa^5 + 
    -0.00301859306 * ta * pa^5 + 
    0.00104452989 * va * pa^5 + 
    0.000247090539 * dtm * pa^5 + 
    0.00148348065 * pa^6
  return(utci)
}

# saturation vapor pressure, modified from https://www.thunderscientific.com/tech_info/reflibrary/its90formulas.pdf,  page 2

es <- function(ta) {
  g <- c(-2.8365744E3, -6.028076559E3, 1.954263612E1, -2.737830188E-2, 1.6261698E-5, 7.0229056E-10, -1.8680009E-13, 2.7150305) # Wexler's coefficient to new ITS-90 scale
  tk <- ta + 273.15 #convert to kelvin
  ln_es <- g[8] * log(tk)
  for (i in 1:7) {
    ln_es <- ln_es  + g[i] * tk^(i-3) 
    #   print(ln_es)
  }
  return(exp(ln_es) * 0.01) #returns saturation vapor pressure in hPa
}

# alternate es formulation from https://journals.ametsoc.org/view/journals/apme/57/6/jamc-d-17-0334.1.xml
es <- 610.94 * exp((17.625*ta)/(ta + 243.04)) # the improved Magnus formula is in celsius , units are Pa

# utci, scenarios -----
indices_366 <- seq(1,366, 1)
indices_366 <- paste0("day_", indices_366)
locOfClimFiles <- "data/bigFiles/"
for (k in sspChoices) {
  #    k = "ssp126"
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_rh_mean <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr", "_", k, "_hurs_", yearSpan, ".tif")
    # fileName_tmax_mean <- paste0(locOfFiles,  "ensembleMn_dailyMn_20Yr", "_", k, "_tasmax_", yearSpan, ".tif")
    # fileName_tmin_mean <- paste0(locOfFiles,  "ensembleMn_dailyMn_20Yr", "_", k, "_tasmin_", yearSpan, ".tif")
    fileName_tas_mean <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr", "_", k, "_tas_", yearSpan, ".tif")
    fileName_ps_mean <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr", "_", k, "_ps_", yearSpan, ".tif")
    fileName_sfcWind_mean <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr", "_", k, "_sfcWind_", yearSpan, ".tif")
    fileName_rsds_mean <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr", "_", k, "_rsds_", yearSpan, ".tif")
    
    ta <- rast(fileName_tas_mean)
    rh <- rast(fileName_rh_mean)
    wind <- rast(fileName_sfcWind_mean)
    names(ta) <- names(rh) <- names(wind) <- indices_366
    fileName_out <-  paste0("data/cmip6/THI/utci_ensembleMean_",  k, "_",  yearSpan, ".tif") 
    print(paste0("fileName out: ", fileName_out))
    comb_clim <- sds(ta, rh, wind)
    print(system.time(r_out <- lapp(comb_clim, f_utci, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
  }
}

# utci, historical -----
k = "historical"
l = 1991
yearSpan <- paste0(l, "_", l + yearRange)
fileName_rh_mean <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr", "_", k, "_hurs_", yearSpan, ".tif")
# fileName_tmax_mean <- paste0(locOfFiles,  "ensembleMn_dailyMn_20Yr", "_", k, "_tasmax_", yearSpan, ".tif")
# fileName_tmin_mean <- paste0(locOfFiles,  "ensembleMn_dailyMn_20Yr", "_", k, "_tasmin_", yearSpan, ".tif")
fileName_tas_mean <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr", "_", k, "_tas_", yearSpan, ".tif")
fileName_ps_mean <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr", "_", k, "_ps_", yearSpan, ".tif")
fileName_sfcWind_mean <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr", "_", k, "_sfcWind_", yearSpan, ".tif")
fileName_rsds_mean <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr", "_", k, "_rsds_", yearSpan, ".tif")

ta <- rast(fileName_tas_mean)
rh <- rast(fileName_rh_mean)
wind <- rast(fileName_sfcWind_mean)
names(ta) <- names(rh) <- names(wind) <- indices_366
fileName_out <-  paste0("data/cmip6/THI/utci_ensembleMean_",  k, "_",  yearSpan, ".tif") 
print(paste0("fileName out: ", fileName_out))
comb_clim <- sds(ta, rh, wind)
print(system.time(r_out <- lapp(comb_clim, f_utci, filename = fileName_out, overwrite = TRUE, wopt = woptList)))

#means, scenarios -----
ext_farNorth <- ext(-180, 180, 50, 90)
ext_NH <- ext(-180, 180, 0, 60)
ext_SH <- ext(-180, 180, -60, 0)
metricChoice <- "pwc"
metricChoice <- "utcTopwc"
{f_utcTopwc <- function(r) {100/(1 + (45.33/r)^-4.30)}
  locOfDataFiles <- "data/cmip6/THI/"
  r_global <- data.frame(row.names = seq(1, 183, 1))
  for (m in c("NH", "SH")) {
    # start with historical
    k = "historical"
    l = 1991
    yearSpan <- paste0(l, "_", l + yearRange)
    hemi <- paste0("ext_", m)
    if (m == "NH") subsetWindow = 1:183
    if (m == "SH") subsetWindow = 184:366
    
    if (metricChoice == "utci") fileName_in <- paste0("data/cmip6/THI/utci_ensembleMean_",  k, "_",  yearSpan, ".tif") 
    if (metricChoice == "pwc") fileName_in <- paste0(locOfDataFiles, "ensemble_thi.", "humans", "_", k, "_", yearSpan, ".tif")
    if (metricChoice == "utcTopwc") fileName_in <- paste0("data/cmip6/THI/utci_ensembleMean_",  k, "_",  yearSpan, ".tif") 
    
    print(paste0("fileName in: ", fileName_in))
    r <- rast(fileName_in)
    r <- crop(r, get(hemi))
    r <- subset(r, subsetWindow)
    print(system.time(r <- app(r, fun = f_utcTopwc)))
    print(system.time(r_mean <- global(r, "mean", na.rm = TRUE)))
    # r_max <- global(r, "max", na.rm = TRUE)
    # r_min <- global(r, "min", na.rm = TRUE)}))
    #       r_combined <- cbind(m, r_min, r_mean, r_max)
    # names(r_combined) <- c("hemisphere", paste0("utci", "_", k, "_", l, "_min"), paste0("utci", "_", k, "_", l, "_mean"), paste0("utci", "_", k, "_", l, "_max"))
    r_combined <- cbind(m, r_mean)
    if (metricChoice == "utci") names(r_combined) <- c("hemisphere", paste0("utci", "_", k, "_", l, "_mean"))
    if (metricChoice == "pwc") names(r_combined) <- c("hemisphere", paste0("pwc", "_", k, "_", l, "_mean"))
    if (metricChoice == "utcTopwc") names(r_combined) <- c("hemisphere", paste0("utcTopwc", "_", k, "_", l, "_mean"))
    r_global <- cbind(r_global, r_combined)
    
    # now do scenarios for a hemisphere
    for (k in sspChoices) {
      for (l in startYearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        if (metricChoice == "utci") fileName_in <- paste0("data/cmip6/THI/utci_ensembleMean_",  k, "_",  yearSpan, ".tif") 
        if (metricChoice == "pwc") fileName_in <- paste0(locOfDataFiles, "ensemble_thi.", "humans", "_", k, "_", yearSpan, ".tif")
        if (metricChoice == "utcTopwc") fileName_in <- paste0("data/cmip6/THI/utci_ensembleMean_",  k, "_",  yearSpan, ".tif") 
        print(paste0("fileName in: ", fileName_in))
        r <- rast(fileName_in)
        r <- crop(r, get(hemi))
        r <- subset(r, subsetWindow)
        print(system.time(r <- app(r, fun = f_utcTopwc)))
        print(system.time(r_mean <- global(r, "mean", na.rm = TRUE)))
        # r_max <- global(r, "max", na.rm = TRUE)
        # r_min <- global(r, "min", na.rm = TRUE)}))
        #       r_combined <- cbind(m, r_min, r_mean, r_max)
        # names(r_combined) <- c("hemisphere", paste0("utci", "_", k, "_", l, "_min"), paste0("utci", "_", k, "_", l, "_mean"), paste0("utci", "_", k, "_", l, "_max"))
        r_combined <- cbind(m, r_mean)
        if (metricChoice == "utci") names(r_combined) <- c("hemisphere", paste0("utci", "_", k, "_", l, "_mean"))
        if (metricChoice == "pwc") names(r_combined) <- c("hemisphere", paste0("pwc", "_", k, "_", l, "_mean"))
        if (metricChoice == "utcTopwc") names(r_combined) <- c("hemisphere", paste0("utcTopwc", "_", k, "_", l, "_mean"))
        r_global <- cbind(r_global, r_combined)
      }    
    }
  }
  
  if (metricChoice == "utci") fileName_out <- paste0(locOfDataFiles, "utci_hemispheres.csv")
  if (metricChoice == "pwc")  fileName_out <- paste0(locOfDataFiles, "pwc_hemispheres.csv")
  if (metricChoice == "utcTopwc") fileName_out <- paste0(locOfDataFiles, "utcTopwc_hemispheres.csv")
  write.csv(r_global, fileName_out)
}
  
  

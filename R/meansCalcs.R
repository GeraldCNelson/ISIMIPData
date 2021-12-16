# climate data means
{
  require("terra") # always used so load it here
  woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL = 6", "NUM_THREADS=ALL_CPUS"))
  
  # try a larger memory allowance, for macs only
  terraOptions(memfrac = 2,  ncopies = 1, progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
  # source("R/ISIMIPconstants.R")
  # source("R/ISIMIPspatialConstants.R")
  locOfClimFiles <- "/Volumes/ExtremeSSD2/ISIMIP/cmip6/"
  yearRange <- 19
  climateVars <- c("prsn", "hurs" , "sfcwind", "rsds", "tas","tasmax", "tasmin","pr") #, "tasmax", "tasmin", "hurs") #, "pr", "tas" , "tasmax", "tasmin" "hurs" , "sfcwind", "rsds"
  meanChoices <- c("daily", "monthly", "annual")
  meanChoices <- c("monthly")
  
  # test values
  climateVar <- "hurs"
  k <- "ssp585"
  l <- 2041
  modelChoice <- "IPSL-CM6A-LR"
  meanChoice <- "monthly"
  
  # f_readRast is for the ensemble calcs
  f_readRast <- function(modelChoice, k, l, climateVar, meanChoice) {
    modelChoice_lower <- tolower(modelChoice)
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_in <- paste0(locOfClimFiles, "mean_", meanChoice, "/", "mean_", meanChoice, "_", climateVar, "_", modelChoice_lower, "_", k,  "_", yearSpan, ".tif")
    print(paste0("climateVar: ", climateVar, ", k: ", k, ", model: ", modelChoice_lower, ", meanChoice: ", meanChoice, ", fileName in: ", fileName_in))
    r <- rast(fileName_in)
    if (meanChoice == "monthly") names(r) <- month.abb
    return(r)
  }
  
  f_means <- function(k, l, modelChoice, climateVar, meanChoice) {
    gc()
    modelChoice_lower <- tolower(modelChoice)
    yearSpan <- paste0(l, "_", l + yearRange)
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices_date  <- paste0("X", as.character(indices))
    indices_month <- as.numeric(format(indices, format = "%m"))
    indices_day <- as.numeric(format(indices, format = "%j"))
    print(paste0("working on start year: ", l, ", variable: ", climateVar, ", ssp choice: ", k, ", modelChoice: ", modelChoice, ", period choice: ", meanChoice))
    fileName_in <- paste0(locOfClimFiles, modelChoice_lower, "_", climateVar, "_", k, "_", yearSpan, ".tif") 
    r_in <- rast(fileName_in)
    fileName_mean_daily_out <- paste0(locOfClimFiles, "mean_daily/mean_daily_", climateVar, "_", modelChoice_lower, "_", k,  "_", yearSpan, ".tif")
    fileName_mean_monthly_out <- paste0(locOfClimFiles, "mean_monthly/mean_monthly_", climateVar, "_", modelChoice_lower, "_", k,  "_", yearSpan, ".tif")
    if (climateVar == "pr" & meanChoice == "annual") { 
      fileName_annualSum_out <- paste0(locOfClimFiles, "mean_annual/ precip_annualSum_", climateVar, "_", modelChoice_lower, "_", k,  "_", yearSpan, ".tif")
      print(system.time(r_annualSum <- app(r_in, fun = sum)))
      r_annualSum <- r_annualSum/(yearRange + 1)
      print(system.time(writeRaster(r_annualSum, filename = fileName_annualSum_out, overwrite = TRUE, wopt = woptList)))
      #     print(r_annualSum)
    }
    if (climateVar == "pr" & meanChoice == "monthly") { 
      fileName_monthlySum_out <- paste0(locOfClimFiles, "mean_monthly/precip_monthlySum_", climateVar, "_", modelChoice_lower, "_", k,  "_", yearSpan, ".tif")
      print(system.time(r_monthlySum <- tapp(r_in, indices_month, fun = sum)))
      r_monthlySum <- r_monthlySum/(yearRange + 1)
      print(system.time(writeRaster(r_monthlySum, filename = fileName_monthlySum_out, overwrite = TRUE, wopt = woptList)))
      print(paste0("fileName out, monthly sum: ", fileName_monthlySum_out))
    }
    if (!climateVar == "pr" & meanChoice == "daily") {
      print(system.time(r_mean_daily <- tapp(r_in, indices_day, fun = mean)))
      names(r_mean_daily) <- indices_date
      writeRaster(r_mean_daily, filename = fileName_out_mean_daily, overwrite = TRUE, wopt = list()) # daily mean over the data set, output has 366 layers  
    }
    if (!climateVar == "pr" & meanChoice == "monthly") {
      print(system.time(r_mean_monthly <- tapp(r_in, indices_month, fun = mean, filename = fileName_mean_monthly_out, overwrite = TRUE, wopt = list()))) # Output has 12 layers
      print(paste0("fileName out, monthly mean: ", fileName_mean_monthly_out))
    } 
    if (!climateVar == "pr" & meanChoice == "annual") {
      fileName_mean_annual_out <- paste0(locOfClimFiles, "mean_annual/mean_annual_", climateVar, "_", modelChoice_lower, "_", k,  "_", yearSpan, ".tif")
      print(system.time(r_mean_annual <- app(r_in, fun = mean, filename = fileName_mean_annual_out, overwrite = TRUE, wopt = list())))
      print(paste0("fileName out, annual mean: ", fileName_mean_annual_out))
    }
  }
}
f_annualMonth_means <- function(k, l, yearRange, climVars) {
  print(l)
  print(yearRange)
  yearSpan <- paste0(l, "_", l + yearRange)
  for (climateVar in climVars) {
    for (modelChoice in modelChoices) {
      modelChoice_lower <- tolower(modelChoice)
      startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices_date  <- paste0("X", as.character(indices))
      indices_month <- as.numeric(format(indices, format = "%m"))
      indices_day <- as.numeric(format(indices, format = "%j"))
      print(paste0("working on start year: ", l, ", variable: ", climateVar, ", ssp choice: ", k, ", modelChoice: ", modelChoice, ", period choice: ", meanChoice))
      fileName_in <- paste0(locOfClimFiles, modelChoice_lower, "_", climateVar, "_", k, "_", yearSpan, ".tif") 
      r_in <- rast(fileName_in)
      #      names(r_in) <- indices_date
      
      # get a year at a time
      for (yr in l:(l + yearRange)) {
        startDate <- paste0(yr, "-01-01"); endDate <- paste0(yr, "-12-31")
        indices_yr <- seq(as.Date(startDate), as.Date(endDate), 1)
        indices_month_yr <- as.numeric(format(indices_yr, format = "%m"))
        indices_date_yr <- paste0("X", as.character(indices_yr))
        yr_data_in <- subset(r_in, indices_date_yr)
        fileName_out <- paste0("data/bigFiles/mean_monthly/monthlyMn_Yr_",climateVar, "_", modelChoice, "_", k,  "_", yr, ".tif")
        print(system.time(yr_mean_monthly <- tapp(yr_data_in, indices_month_yr, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
      }
    }
  }
}

f_convert_nc_to_tif <- function(k, l, yearRange, climVars, locOfNCFiles) {
  yearSpan <-paste0(l, "_", l + yearRange)
  for (modelChoice in modelChoices) {
    modelChoice_lower <- tolower(modelChoice)
    for (climateVar in climVars) {
      fillerText <- "_r1i1p1f1_w5e5_"
      if (modelChoice %in% "UKESM1-0-LL") fillerText <- "_r1i1p1f2_w5e5_"
      fileName_in <- paste0(locOfNCFiles, k, "/", modelChoice, "/", modelChoice_lower, fillerText, k, "_", climateVar, "_global_daily_", yearSpan, ".nc")
      fileName_out <- paste0(locOfClimFiles, modelChoice_lower, "_", climateVar, "_", k, "_", yearSpan, ".tif") 
      r_in <- rast(fileName_in)
      r_out <- r_in
      if (climateVar %in% c("pr", "prsn")) r_out <- r_in * 86400
      if (climateVar %in% c("tas", "tasmin", "tasmax")) r_out <- r_in - 273.15
      startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices_date  <- paste0("X", as.character(indices))
      names(r_out) <- indices_date
      setMinMax((r_out))
      print(system.time(writeRaster(r_out, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
    }
  }
}

# means, scenarios -----
for (modelChoice in modelChoices) {
  for (k in sspChoices) {
    for (l in startYearChoices) {
      for (climateVar in climateVars) {
        gc()
        f_means(k, l, modelChoice, climateVar, meanChoice)
      }
    }
  }
}

# means, historical -----
k <- "historical"
l <- 1991
for (modelChoice in modelChoices) {
  for (climateVar in climateVars) {
    gc()
    f_means(k, l, modelChoice, climateVar, meanChoice)
  }
}

f_ensemble_calcs <- function(k, l) {
  yearSpan <- paste0(l, "_", l + yearRange)
  indices_day <- paste0("X", seq(1,366,1))
  indices_month <- paste0("X", seq(1, 12, 1))
  for (climateVar in climateVars) {
    # meanName <- paste0("mean_", meanChoice, "/", "mean_", meanChoice, "_")
    # fileName_in <- paste0(locOfClimFiles, meanName, climateVar, "_", modelChoice_lower, "_", k,  "_", yearSpan, ".tif")
    for (meanChoice in meanChoices) {
      x <- lapply(modelChoices, f_readRast, k, l, climateVar, meanChoice)
      r <- rast(x)
      for (hem in hemispheres) {
        if (hem == "NH") hemExt <- extent_NH
        if (hem == "SH") hemExt <- extent_SH
        r_hem <- crop(r, hemExt)
        if (meanChoice == "daily") {
          fileName_out <- paste0("data/bigFiles/ensembleMn_dailyMn_20Yr_", k,  "_", hem, "_", climateVar, "_", yearSpan, ".tif") 
          print(system.time(r_hem <- tapp(r_hem, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
        }
        
        if (meanChoice == "monthly") {
          fileName_out <- paste0("data/bigFiles/ensembleMn_monthlyMn_20Yr_", k,  "_", hem, "_", climateVar, "_", yearSpan, ".tif")
          print(system.time(r_hem <- tapp(r_hem, indices_month, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
        }
        
        print(paste0("fileName out: ", fileName_out))
      }
    }
  }
}

#  ensemble calcs -----
# ensembles, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    f_ensemble_calcs(k, l)
  }
}

# ensembles, historical -----
k = "historical"
l = 1991
f_ensemble_calcs(k, l)

# combine hemisphere results, monthly means ----
for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (climateVar in climateVars) {
      fileName_in_NH <- paste0("data/bigFiles/ensembleMn_monthlyMn_20Yr_", k,  "_", "NH", "_", climateVar, "_", yearSpan, ".tif")
      fileName_in_SH <- paste0("data/bigFiles/ensembleMn_monthlyMn_20Yr_", k,  "_", "SH", "_", climateVar, "_", yearSpan, ".tif")
      fileName_out <- paste0("data/bigFiles/ensembleMn_monthlyMn_20Yr_", k,  "_", climateVar, "_", yearSpan, ".tif")
      r_NH <- rast(fileName_in_NH)
      r_SH <- rast(fileName_in_SH)
      r_out <- merge(r_NH, r_SH)
      print(system.time(writeRaster(r_out, fileName_out, overwrite = TRUE, wopt = woptList)))
    }
  }
}

k = "historical"
l = 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (climateVar in climateVars) {
  fileName_in_NH <- paste0("data/bigFiles/ensembleMn_monthlyMn_20Yr_", k,  "_", "NH", "_", climateVar, "_", yearSpan, ".tif")
  fileName_in_SH <- paste0("data/bigFiles/ensembleMn_monthlyMn_20Yr_", k,  "_", "SH", "_", climateVar, "_", yearSpan, ".tif")
  fileName_out <- paste0("data/bigFiles/ensembleMn_monthlyMn_20Yr_", k,  "_", climateVar, "_", yearSpan, ".tif")
  r_NH <- rast(fileName_in_NH)
  r_SH <- rast(fileName_in_SH)
  r_out <- merge(r_NH, r_SH)
  print(system.time(writeRaster(r_out, fileName_out, overwrite = TRUE, wopt = woptList)))
}

# annual monthly means -----
climVars <- c("pr"s, "tas" , "tasmax", "tasmin", "hurs")
yearRange <- 19
for (k in sspChoices) {
  for (l in startYearChoices) {
    f_annualMonth_means(k, l, yearRange, climVars)
  }
}

# new years, 10 year window ----
# convert the nc files to tifs
locOfNCFiles <- "/Volumes/PassportMac/ISIMIP/cmip6/"
startYearChoices_new <- c(2021, 2031, 2071)
startYearChoices_new <- c(2071)
yearRange <- 9
for (k in sspChoices) {
  for (l in startYearChoices_new) {
    f_convert_nc_to_tif(k, l, yearRange, climVars, locOfNCFiles)
  }
}
climVars_temp <- c("pr", "tas" , "tasmax", "tasmin", "hurs")
yearRange <- 9
for (k in sspChoices) {
  for (l in startYearChoices_new) {
    f_annualMonth_means(k, l, yearRange, climVars_temp)
  }
}

# convert smaller nc files
climVars <- c("pr", "tas" , "tasmax", "tasmin", "hurs")
yearRange <- 5
l = 2015
for (k in sspChoices) {
  f_convert_nc_to_tif(k, l, yearRange, climVars, locOfNCFiles)
}

#historical data -----
k = "historical"
l = 2011
yearRange <- 3
f_convert_nc_to_tif(k, l, yearRange, climVars, locOfNCFiles)

# calculate monthly averages -----

yearRange <- 5
l = 2015
for (k in sspChoices) {
  f_annualMonth_means(k, l, yearRange, climVars)
}

yearRange <- 3
l = 2011
k = "historical"
f_annualMonth_means(k, l, yearRange, climVars)














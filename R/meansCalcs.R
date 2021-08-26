# climate data means
{
  require("terra") # always used so load it here
  source("R/ISIMIPconstants.R")
  source("R/ISIMIPspatialConstants.R")
  locOfClimFiles <- "/Volumes/ExtremeSSD2/ISIMIP/cmip6/"
  
  climateVars <- c("pr") #, "tasmax", "tasmin", "hurs") #, "pr", "tas" , "tasmax", "tasmin" "hurs" , "sfcwind", "rsds"
  meanChoices <- c("daily", "monthly", "annual")
  meanChoices <- c("monthly")
  
  # test values
  climateVar <- "pr"
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
  # fileName_out_dailySD <- paste0(locOfClimFiles, "mean_daily/dailySD_", climateVar, "_", modelChoice_lower, "_", k,  "_", yearSpan, ".tif")
  # # print(system.time(r_dailySD <- tapp(r_in, indices_day, fun = "sd", filename = fileName_out_dailySD, overwrite = TRUE, wopt = list()))) # daily mean over the data set, output has 366 layers   
  # if (meanChoice == "monthly") {
  #   print(system.time(r_mean_monthly <- tapp(r_in, indices_month, fun = mean, filename = fileName_mean_monthly_out, overwrite = TRUE, wopt = list()))) # Output has 12 layers
  #   print(paste0("fileName out, monthly mean: ", fileName_mean_monthly_out))
  # }
  # if (meanChoice == "daily") {
  #   print(system.time(r_mean_daily <- tapp(r_in, indices_day, fun = mean, filename = fileName_mean_daily_out, overwrite = TRUE, wopt = list()))) # daily mean over the data set, output has 366 layers  
  #   print(paste0("fileName out, daily mean: ", fileName_mean_daily_out))
  #   
  # }
  
  #    print(paste0("writing annual mean: ", fileName_out_mean_annual))
  #    writeRaster(r_mean_daily, filename = paste0("data/cmip6/mean_annual/", fileName_out_mean_annual),  overwrite = TRUE, wopt= woptList)
  #     print(paste0("fileName out, daily mean: ", fileName_out_mean_daily))
  #     print(paste0("fileName out, daily SD: ", fileName_out_dailySD))
  # print(paste0("fileName out, monthly mean: ", fileName_out_mean_monthly))
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

#  ensemble calcs -----
# ensembles, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (climateVar in climateVars) {
      # meanName <- paste0("mean_", meanChoice, "/", "mean_", meanChoice, "_")
      # fileName_in <- paste0(locOfClimFiles, meanName, climateVar, "_", modelChoice_lower, "_", k,  "_", yearSpan, ".tif")
      for (meanChoice in meanChoices) {
        x <- lapply(modelChoices, f_readRast, k, l, climateVar, meanChoice)
        r <- rast(x)
        if (meanChoice == "daily") indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result XXX check here at some point
        if (meanChoice == "monthly") indices_month <- as.numeric(format(as.Date(names(r_in), format = "X%Y-%m-%d"), format = "%m"))
        for (hem in hemispheres) {
          if (hem == "NH") hemExt <- extent_NH
          if (hem == "SH") hemExt <- extent_SH
          r_hem <- crop(r, hemExt)
          if (meanChoice == "daily") fileName_out <- paste0("data/bigFiles/ensembleMn_dailyMn_20Yr_", k,  "_", hem, "_", climateVar, "_", yearSpan, ".tif") 
          if (meanChoice == "monthly") fileName_out <- paste0("data/bigFiles/ensembleMn_monthlyMn_20Yr_", k,  "_", hem, "_", climateVar, "_", yearSpan, ".tif")
          print(system.time(r_hem <- tapp(r_hem, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
          print(paste0("fileName out: ", fileName_out))
        }
      }
    }
  }
}

# ensembles, historical -----
k = "historical"
l = 1991
for (climateVar in climateVars) {
  x <- lapply(modelChoices, f_readRast, k, l, climateVar, meanChoice)
  r <- rast(x)
  indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
  for (hem in hemispheres) {
    if (hem == "NH") hemExt <- extent_NH
    if (hem == "SH") hemExt <- extent_SH
    r_hem <- crop(r, hemExt)
    if (meanChoice == "daily") fileName_out <- paste0("data/bigFiles/ensembleMn_dailyMn_20Yr_", k,  "_", hem, "_", climateVar, "_", yearSpan, ".tif") 
    if (meanChoice == "monthly") fileName_out <- paste0("data/bigFiles/ensembleMn_monthlyMn_20Yr_", k,  "_", hem, "_", climateVar, "_", yearSpan, ".tif")
    print(system.time(r_hem <- tapp(r_hem, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
    print(paste0("fileName out: ", fileName_out))
    
  }
}

# do calculations on historical data; note ensemble calculations for historical done in ensembleCalcsHistoricalData.R. Here we just need to calculate the monthly means (sums for precip)s
# climateVars <- c( "tasmax", "tasmin",  "pr", "rsds", "sfcwind", "hurs") #"hurs", "tas", 
# climateVars <- c("tasmax", "tasmin", "hurs") #"hurs",
# startYearChoices <-  c(1991, 2001) 
# startYearChoices <-  c(2001) 
# locOfClimFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/ensemble/"
# 
# #test values
# l = 2001
# var = "hurs"
# 
# for (l in startYearChoices) {
#   yearSpan <- paste0(l, "_", l + yearRange)
#   startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
#   indices <- seq(as.Date(startDate), as.Date(endDate), 1)
#   indices_month <- format(indices, format = "%m")
#   indices_day <- format(indices, format = "%var")
#   indices_month <- as.numeric(indices_month)
#   indices_day <- as.numeric(indices_day)
#   
#   for (climateVar in climateVars) {
#     gc()
#     
#     print(paste0("working on start year: ", l, ", variable: ", climateVar, ", pid: ", Sys.getpid(), " systime: ", Sys.time()))
#     fileName_in <- paste("ensemble_historical",  climateVar, yearSpan, sep = "_")
#     fileName_in <- paste0(fileName_in, ".tif")
#     temp <- paste(locOfClimFiles, fileName_in, sep = "")
#     r_in <- rast(temp)
#     print(r_in)
#     
#     print(system.time(r_mean <- mean(r_in))) # one layer with one value per cell; the means of daily values for the 10 year period
#     print(r_mean)
#     
#     if (climateVar %in% "pr") {
#       print(system.time(r_mean_daily <- tapp(r_in, indices_day, fun = mean)))
#       
#       indices_month <- format(indices, format = "%m")
#       indices_month <- as.numeric(indices_month)
#       print(system.time(r_mean_monthly <- tapp(r_in, indices_month, fun = sum))) # sum for pr. this sums the values for all days in a month across the whole data set. Need to divide by yearRange to get the per month value
#       print(system.time(r_mean_monthly <- r_mean_monthly/yearRange))
#     } else {
#       print(system.time(r_mean_monthly <- tapp(r_in, indices_month, fun = mean)))
#     }
#     names(r_mean_monthly) <- month.abb
#     print(r_mean_monthly)
#     XXXX this needs to be reviewedXXX
#     fileName_out_mean_annual <- paste0("ensemblemean_annual_", climateVar, "_historical_", yearSpan, ".tif")
#     fileName_out_mean_monthly <- paste0("ensemblemean_monthly_", climateVar, "_historical_", yearSpan, ".tif")
#     fileName_out_mean_daily <- paste0("ensemblemean_daily_", climateVar, "_", modelChoice_lower, "_", k,  "_", yearSpan, ".tif")
#     
#     print(paste0("writing annual mean: ", fileName_out_mean_annual))
#     writeRaster(r_mean, filename = paste0("data/cmip6/mean_annual/", fileName_out_mean_annual),  overwrite = TRUE, wopt= woptList)
#     
#     print(paste0("writing monthly mean: ", fileName_out_mean_monthly))
#     writeRaster(r_mean_monthly, filename = paste0("data/cmip6/mean_monthly/", fileName_out_mean_monthly),  overwrite = TRUE, wopt= woptList)
#   }
# }

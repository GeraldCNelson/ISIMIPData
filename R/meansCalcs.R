# climate data mean 
{#source("R/globallyUsed.R")
  library(terra)
  terraOptions(memfrac = 2, ncopies = 0,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE)
  woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))
  
  sspChoices <- c("ssp126", "ssp585") #"ssp126", "ssp585"
  modelChoices <- c(  "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR", "MRI-ESM2-0", "GFDL-ESM4")
  #modelChoices <- c( "MRI-ESM2-0")
  modelChoices.lower <- tolower(modelChoices)
  climateVars <- c( "tasmax", "tasmin", "pr", "hurs",  "rsds", "sfcwind") 
  climateVars <- c(  "tasmin") #, "hurs") # "tasmin", tasmax
  
  #startYearChoices <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
  startYearChoices <- c(2041, 2081)
  locOfFiles <- locOfCMIP6tifFiles <- "data/bigFiles/"
  yearRange <- 19
  northernHemExtent <- c( -180, 180, 0, 90)
  southernHemExtent <-c( -180, 180, -60, 0)
  hemispheres <- c("NH", "SH")
  
  # test values
  i <- "UKESM1-0-LL"
  k <- "ssp126"
  l <- 2041
  yearSpan <- paste0(l, "_", l + yearRange)
  climvar <- "tasmin"
  hem <- "NH"
  
  f_means <- function() {
    gc()
    modelName.lower <- tolower(i)
    yearSpan <- paste0(l, "_", l + yearRange)
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices <- paste0("X", as.character(indices))
    print(paste0("working on start year: ", l, ", variable: ", climvar, ", ssp choice: ", k, ", model: ", i))
    fileName_in <- paste(modelName.lower, k, climvar, "global_daily", yearSpan, sep = "_")
    fileName_in <- paste0(locOfFiles, fileName_in, ".tif")
    rastIn <- rast(fileName_in)
    names(rastIn) <- indices # may not be necessary now, but keep it around
    print(rastIn)
    indices_month <- as.numeric(format(as.Date(names(rastIn), format = "X%Y-%m-%d"), format = "%m"))
    indices_day <- as.numeric(format(as.Date(names(rastIn), format = "X%Y-%m-%d"), format = "%j"))
    
    if (climvar %in% "pr") { # XXXXX precip seems to be in kgs
      fileName_out_dailyMean <- paste0(locOfFiles, "dailyMean/dailyMean_", "pr", "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
      fileName_out_monthlyMean <- paste0(locOfFiles, "monthlyMean/monthlyMean_", "pr", "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
      fileName_out_annualSum <- paste0(locOfFiles, "annualMean/annualSum_", "pr", "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
      
      print(system.time(rast.dailyMean <- tapp(rastIn, indices_day, fun = mean, filename=fileName_out_dailyMean, overwrite = TRUE, wopt=list()))) # daily mean rainfall over the data set
      # sum for pr. this sums the values for all days in a month across the whole data set. Need to divide by yearRange to get the per month value
      print(system.time(rast.monthlyMean <- tapp(rastIn, indices_month, fun = sum))) 
      print(system.time(rast.monthlyMean <- rast.monthlyMean/yearRange))
      names(rast.monthlyMean) <- month.abb
      print(rast.monthlyMean)
      writeRaster(rast.monthlyMean, filename=fileName_out_monthlyMean, overwrite=FALSE, wopt=list())
      print(system.time(rast.annualSum <- app(rastIn, fun = sum, filename=fileName_out_annualSum, overwrite=FALSE, wopt=list()))) 
      print(rast.annualSum)
      
    } else {
      fileName_out_annualMean <- paste0(locOfFiles, "annualMean/annualMean_", climvar, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
      fileName_out_monthlyMean <- paste0(locOfFiles, "monthlyMean/monthlyMean_", climvar, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
      fileName_out_dailyMean <- paste0(locOfFiles, "dailyMean/dailyMean_", climvar, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
      print(system.time(rast.dailyMean <- tapp(rastIn, indices_day, fun = mean ,filename = fileName_out_dailyMean, overwrite = TRUE, wopt=list()))) # daily mean over the data set, output has 366 layers        
      print(system.time(rast.monthlyMean <- tapp(rastIn, indices_month, fun = mean, filename=fileName_out_monthlyMean, overwrite = TRUE, wopt=list()))) # Output has 12 layers
      names(rast.monthlyMean) <- month.abb
      print(rast.monthlyMean)
      print(system.time(rast.annualMean <- app(rastIn, fun = mean, filename=fileName_out_annualMean, overwrite = TRUE, wopt=list())))
      
      #    print(paste0("writing annual mean: ", fileName_out_annualMean))
      #       writeRaster(rast.dailyMean, filename = paste0("data/cmip6/annualMean/", fileName_out_annualMean),  overwrite = TRUE, wopt= woptList)
      
      print(paste0("fileName out, daily mean: ", fileName_out_dailyMean))
      print(paste0("fileName out, monthly mean: ", fileName_out_monthlyMean))
      print(paste0("fileName out, annual mean: ", fileName_out_annualMean))
    }
  }
}

# means, scenarios -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startYearChoices) {
      for (climvar in climateVars) {
        gc()
        f_means()
      }
    }
  }
}

# means, historical -----
k <- "historical"
l <- 1991
for (i in modelChoices) {
  for (climvar in climateVars) {
    gc()
    f_means()
  }
}


#  ensemble calcs -----
f_readRast <- function(modelChoice.lower) {
  model.name <- tolower(i)
  fileName_in <- paste0(locOfFiles, meanChoice, "Mean/", meanChoice, "Mean_", climvar, "_", modelChoice.lower, "_", k,  "_", yearSpan, ".tif")
  print(paste0("climvar: ", climvar, ", k: ", k, ", model: ", modelChoice.lower, ", fileName in: ", fileName_in))
  r <- rast(fileName_in)
  # indices <- seq(from = 1, to = nlyr(r), 1)
  # indices <- paste0("X", as.character(indices))
  # names(r) <- indices
  # r
}

meanChoices <- c("daily", "monthly", "annual")

# ensembles, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (climvar in climateVars) {
      fileName_in_annualMean <- paste0(locOfFiles, "annualMean/annualMean_", climvar, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
      fileName_in_monthlyMean <- paste0(locOfFiles, "monthlyMean/monthlyMean_", climvar, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
      fileName_in_dailyMean <- paste0(locOfFiles, "dailyMean/dailyMean_", climvar, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
      for (meanChoice in meanChoices) {
        x <- lapply(modelChoices.lower, f_readRast)
        r <- rast(x)
        if (meanChoice == "daily") indices <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
        if (meanChoice == "monthly") indices_month <- as.numeric(format(as.Date(names(rastIn), format = "X%Y-%m-%d"), format = "%m"))
        for (hem in hemispheres) {
          if (hem == "NH") hemExt <- northernHemExtent
          if (hem == "SH") hemExt <- southernHemExtent
          system.time(r_hem <- crop(r, hemExt))
          fileName_out <- paste0("data/bigFiles/ensembleMn_dailyMn_20Yr_", k,  "_", hem, "_", climvar, "_", yearSpan, ".tif") 
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
for (climvar in climateVars) {
  x <- lapply(modelChoices.lower, f_readRast)
  r <- rast(x)
  indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
  for (hem in hemispheres) {
    if (hem == "NH") hemExt <- northernHemExtent
    if (hem == "SH") hemExt <- southernHemExtent
    system.time(r_hem <- crop(r, hemExt))
    fileName_out <- paste0("data/bigFiles/ensembleMn_dailyMn_20Yr_", k,  "_", hem, "_", climvar, "_", yearSpan, ".tif") # note yearSpan here
    print(system.time(r_hem <- tapp(r_hem, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
    print(paste0("fileName out: ", fileName_out))
    
  }
}

# do calculations on historical data; note ensemble calculations for historical done in ensembleCalcsHistoricalData.R. Here we just need to calculate the monthly means (sums for precip)s
# climateVars <- c( "tasmax", "tasmin",  "pr", "rsds", "sfcwind", "hurs") #"hurs", "tas", 
# climateVars <- c("tasmax", "tasmin", "hurs") #"hurs",
# startYearChoices <-  c(1991, 2001) 
# startYearChoices <-  c(2001) 
# locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/ensemble/"
# 
# #test values
# l = 2001
# climvar = "hurs"
# 
# for (l in startYearChoices) {
#   yearSpan <- paste0(l, "_", l + yearRange)
#   startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
#   indices <- seq(as.Date(startDate), as.Date(endDate), 1)
#   indices_month <- format(indices, format = "%m")
#   indices_day <- format(indices, format = "%climvar")
#   indices_month <- as.numeric(indices_month)
#   indices_day <- as.numeric(indices_day)
#   
#   for (climvar in climateVars) {
#     gc()
#     
#     print(paste0("working on start year: ", l, ", variable: ", climvar, ", pid: ", Sys.getpid(), " systime: ", Sys.time()))
#     fileName_in <- paste("ensemble_historical",  climvar, yearSpan, sep = "_")
#     fileName_in <- paste0(fileName_in, ".tif")
#     temp <- paste(locOfFiles, fileName_in, sep = "")
#     rastIn <- rast(temp)
#     print(rastIn)
#     
#     print(system.time(rast.mean <- mean(rastIn))) # one layer with one value per cell; the means of daily values for the 10 year period
#     print(rast.mean)
#     
#     if (climvar %in% "pr") {
#       print(system.time(rast.dailyMean <- tapp(rastIn, indices_day, fun = mean)))
#       
#       indices_month <- format(indices, format = "%m")
#       indices_month <- as.numeric(indices_month)
#       print(system.time(rast.monthlyMean <- tapp(rastIn, indices_month, fun = sum))) # sum for pr. this sums the values for all days in a month across the whole data set. Need to divide by yearRange to get the per month value
#       print(system.time(rast.monthlyMean <- rast.monthlyMean/yearRange))
#     } else {
#       print(system.time(rast.monthlyMean <- tapp(rastIn, indices_month, fun = mean)))
#     }
#     names(rast.monthlyMean) <- month.abb
#     print(rast.monthlyMean)
#     XXXX this needs to be reviewedXXX
#     fileName_out_annualMean <- paste0("ensembleAnnualMean_", climvar, "_historical_", yearSpan, ".tif")
#     fileName_out_monthlyMean <- paste0("ensembleMonthlyMean_", climvar, "_historical_", yearSpan, ".tif")
#     fileName_out_dailyMean <- paste0("ensembledailyMean_", climvar, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
#     
#     print(paste0("writing annual mean: ", fileName_out_annualMean))
#     writeRaster(rast.mean, filename = paste0("data/cmip6/annualMean/", fileName_out_annualMean),  overwrite = TRUE, wopt= woptList)
#     
#     print(paste0("writing monthly mean: ", fileName_out_monthlyMean))
#     writeRaster(rast.monthlyMean, filename = paste0("data/cmip6/monthlyMean/", fileName_out_monthlyMean),  overwrite = TRUE, wopt= woptList)
#   }
# }

# do monthly means over 20 years instead of 10
sspChoices <- c("ssp585") #"ssp126", "ssp585"
modelChoices <- c(  "MPI-ESM1-2-HR", "UKESM1-0-LL",  "IPSL-CM6A-LR", "MRI-ESM2-0", "GFDL-ESM4") #, "MRI-ESM2-0", "GFDL-ESM4", "MRI-ESM2-0","MPI-ESM1-2-HR", "MRI-ESM2-0") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR"
#modelChoices <- c(  "MPI-ESM1-2-HR")
climateVars <- c( "tasmax", "tasmin")#, "pr", "hurs",  "rsds", "sfcwind") 

startYearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
# locOfFiles <- locOfCMIP6tifFiles
yearRange <- 19

# test values
i <- "MPI-ESM1-2-HR"
k <- "ssp585"
l <- 2081
climvar <- "tasmax"

for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startYearChoices) {
      for (climvar in climateVars) {
        gc()
        modelName.lower <- tolower(i)
        yearSpan <- paste0(l, "_", l + yearRange)
        startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
        indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        indices <- paste0("X", as.character(indices))
        
        print(paste0("working on start year: ", l, ", variable: ", climvar, ", ssp choice: ", k, ", model: ", i,  ", yearSpan: ", yearSpan))
        
        fileName_in <- paste(modelName.lower, k, climvar, "global_daily", yearSpan, sep = "_")
        fileName_in <- paste0(fileName_in, ".tif")
        temp <- paste(locOfFiles, k, "/", i, "/", fileName_in, sep = "")
        rastIn <- rast(temp)
        names(rastIn) <- indices
        print(rastIn)
        # 
        indices_month <- format(as.Date(names(rastIn), format = "X%Y-%m-%d"), format = "%m")
        indices_day <- format(as.Date(names(rastIn), format = "X%Y-%m-%d"), format = "%j")
        indices_month <- as.numeric(indices_month)
        indices_day <- as.numeric(indices_day)
        
        if (climvar %in% "pr") {
          print(system.time(rast.dailyMean <- tapp(rastIn, indices_day, fun = mean))) # daily mean for the 20 year period
          # indices_dayave <- as.numeric(format(as.Date(names(rast.dailyMean), format = "X%j"), format = "%m"))
          print(system.time(rast.monthlyMean <- tapp(rastIn, indices_month, fun = sum))) # sum for pr. this sums the values for all days in a month across the whole data set. Need to divide by yearRange to get the per month value
          print(system.time(rast.monthlyMean <- rast.monthlyMean/yearRange))
          
        } else {
          print(system.time(rast.monthlyMean <- tapp(rastIn, indices_month, fun = mean)))
        }
        names(rast.monthlyMean) <- month.abb
        print(rast.monthlyMean)
        fileName_out_annualMean <- paste0("annualMean_", climvar, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        fileName_out_monthlyMean <- paste0("monthlyMean_", climvar, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        fileName_out_dailyMean <- paste0("dailyMean_", climvar, "_", modelName.lower, "_", k,  "_", yearSpan, ".tif")
        
        print(paste0("writing annual mean: ", fileName_out_annualMean))
        writeRaster(rastIn, filename = paste0("data/cmip6/annualMean/", fileName_out_annualMean),  overwrite = TRUE, wopt= woptList)
        
        print(paste0("writing monthly mean: ", fileName_out_monthlyMean))
        writeRaster(rast.monthlyMean, filename = paste0("data/cmip6/monthlyMean/", fileName_out_monthlyMean),  overwrite = TRUE, wopt= woptList)
        rast.monthlyMean <- NULL
        writeRaster(rast.dailyMean, filename = paste0("data/cmip6/dailyMean/", fileName_out_dailyMean),  overwrite = TRUE, wopt= woptList)
        
        gc()
      }
    }
  }
}

# do 20 year historical means
# do calculations on historical data; note ensemble calculations for historical done in ensembleCalcsHistoricalData.R. Here we just need to calculate the monthly means (sums for precip)s
climateVars <- c( "tasmax", "tasmin",  "pr", "rsds", "sfcwind", "hurs") #"hurs", "tas", 
#climateVars <- c( "hurs") #"hurs",
startYearChoices <-  c(1991) 
locOfFiles <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/historical/ensemble/"
yearRange = 19
#test values
l = 1991
climvar = "hurs"

for (l in startYearChoices) {
  
  yearSpan <- paste0(l, "_", l + yearRange)
  
  startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
  indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  indices_month <- format(indices, format = "%m")
  indices_day <- format(indices, format = "%j")
  indices_month <- as.numeric(indices_month)
  indices_day <- as.numeric(indices_day)
  
  for (climvar in climateVars) {
    gc()
    
    print(paste0("working on start year: ", l, ", variable: ", climvar))
    fileName_in <- paste("ensemble_historical",  climvar, yearSpan, sep = "_")
    fileName_in <- paste0(fileName_in, ".tif")
    temp <- paste(locOfFiles, fileName_in, sep = "")
    rastIn <- rast(temp)
    print(rastIn)
    
    print(system.time(rast.mean <- mean(rastIn))) # one layer with one value per cell; the means of daily values for the 10 year period
    print(rast.mean)
    
    if (climvar %in% "pr") {
      print(system.time(rast.dailyMean <- tapp(rastIn, indices_day, fun = mean)))
      
      indices_month <- format(indices, format = "%m")
      indices_month <- as.numeric(indices_month)
      print(system.time(rast.monthlyMean <- tapp(rastIn, indices_month, fun = sum))) # sum for pr. this sums the values for all days in a month across the whole data set. Need to divide by yearRange to get the per month value
      print(system.time(rast.monthlyMean <- rast.monthlyMean/yearRange))
    } else {
      print(system.time(rast.monthlyMean <- tapp(rastIn, indices_month, fun = mean)))
    }
    names(rast.monthlyMean) <- month.abb
    print(rast.monthlyMean)
    
    fileName_out_annualMean <- paste0("ensembleAnnualMean_", climvar, "_historical_", yearSpan, ".tif")
    fileName_out_monthlyMean <- paste0("ensembleMonthlyMean_", climvar, "_historical_", yearSpan, ".tif")
    
    print(paste0("writing annual mean: ", fileName_out_annualMean))
    writeRaster(rast.mean, filename = paste0("data/cmip6/annualMean/", fileName_out_annualMean),  overwrite = TRUE, wopt= woptList)
    
    print(paste0("writing monthly mean: ", fileName_out_monthlyMean))
    writeRaster(rast.monthlyMean, filename = paste0("data/cmip6/monthlyMean/", fileName_out_monthlyMean),  overwrite = TRUE, wopt= woptList)
  }
}



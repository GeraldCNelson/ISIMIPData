# calculate animal and human climate stress
#source("R/globallyUsed.R")
{library("terra")
  library("crayon")
  
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
    terraOptions(memfrac = 4, progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path, memfrac = .9,
  }else{
    terraOptions(memfrac = .6,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
  }
  
  terraOptions(memfrac = 4, progress = 0, tempdir =  "data/ISIMIP", verbose = FALSE)
  
  #locOfFiles <- locOfCMIP6tifFiles
  locOfFiles <- "data/bigFiles/"
  speciesChoice <- c("humans", "cattle", "goat", "swine", "chicken", "sheep") 
  speciesChoice <- c("humans")
  sspChoices <- c("ssp126", "ssp585") 
  #sspChoices <- c("ssp585") 
  modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
  startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
  startyearChoices_historical <- c(1991)
  scenarioChoicesEnsemble <- c("historical", sspChoices)
  
  resultsStorage <- data.table::data.table(species = character(), scenario = character(), startYear = numeric(), model = character(), ct_small= numeric(), ct_large = numeric())
  
  bpList <- data.table::as.data.table(readxl::read_excel("data-raw/animals/AnimalbreakpointslistRaw.xlsx"))
  extremeStress_humans <- 60
  
  ext_noAntarctica <- ext(-180, 180, -60, 90)
  
  yearRange <- 19
  woptList <- list(gdal=c("COMPRESS=LZW"))
  colorList <- (RColorBrewer::brewer.pal(5, "RdYlGn"))
  
  #test values
  i <- "UKESM1-0-LL"
  k <- "ssp585"
  l <- 2081
  yearNumber <- 2043
  m <- "cattle"
  
  readRast_thi_cattle <- function(yearNumber) {
    fileNameIn <- paste0("data/cmip6/THI/thi.cattle_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
    r <- rast(fileNameIn)
  }
  
  readRast_thi_ensemble <- function(i) {
    fileNameIn <- paste0("data/cmip6/THI/thi.", m, "_", i, "_", k,  "_", yearSpan, ".tif") # note yearSpan here
    print(fileNameIn)
    r <- rast(fileNameIn)
    indices <- seq(from = 1, to = nlyr(r), 1)
    indices <- paste0("X", as.character(indices))
    names(r) <- indices
    r
  }
  
  readRast_thi_sheep <- function(yearNumber) {
    fileNameIn <- paste0("data/cmip6/THI/thi.sheep_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
    print(fileNameIn)
    r <- rast(fileNameIn)
  }
  readRast_thi_goat <- function(yearNumber) {
    fileNameIn <- paste0("data/cmip6/THI/thi.goat_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
    print(fileNameIn)
    r <- rast(fileNameIn)
  }
  readRast_thi_chicken <- function(yearNumber) {
    fileNameIn <- paste0("data/cmip6/THI/thi.chicken_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
    print(fileNameIn)
    r <- rast(fileNameIn)
  }
  
  readRast_thi_swine <- function(yearNumber) {
    fileNameIn <- paste0("data/cmip6/THI/thi.swine_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
    print(fileNameIn)
    r <- rast(fileNameIn)
  }
  
  THIfun_cattle <- function(rh, tmax) {
    thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))
    thi[tmax < 20] <- 0
    thi <- round(thi, 2)
  }
  
  THIfun_sheep <- function(rh, tmax) {
    thi <- tmax - ((0.31 - (0.31 * (rh / 100))) * (tmax - 14.4)) 
    thi[tmax < 20] <- 0
    thi <- round(thi, 2)
  }
  
  THIfun_goat <- function(rh, tmax) {
    thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) # should this be the same as cattle?
    thi[tmax < 20] <- 0
    thi <- round(thi, 2)
  }
  
  THIfun_swine <- function(rh, tmax) {
    thi <- tmax - (0.55 - (0.0055 * rh) * (tmax - 14.5))
    thi[tmax < 20] <- 0
    thi <- round(thi, 2)
  }
  
  THIfun_chicken <- function(tmax, wbulb) {
    thi <- (0.85 * tmax + 0.15 * wbulb) # using broiler formula. Note. no rh needed
    thi[tmax < 20] <- 0
    #  thi[thi > 100] <- 100
    thi <- round(thi, 2)
  }
  
  # formula from Stull, R. (2011). Wet-Bulb Temperature from Relative Humidity and Air Temperature. J. Appl. Meteorol. Climatol. 50, 2267–2269. doi:10.1175/JAMC-D-11-0143.1.
  wetbulb <- function(rh, tas) {
    wb <- tas * atan(0.151977* (rh + 8.313659)^(1/2)) +
      atan(tas + rh) - atan(rh - 1.676331) +
      0.00391838 * (rh)^ (3/2) * atan(0.023101 * rh) - 4.686035
    return(wb)
  }
  
  THIfun_humans <-  function(rh, tas) {
    pwc <- 100/(1+(((-12.28*log(rh)+87.99)/tas))^(-2.21*log(rh)+2.63))
    #  pwc[pwc > 100] <- 100 #Not needed because pwc is never above 100
    # pwc[tas < 15] <- 100
    # pwc
  }
}
#THI scenarios -----
# speciesChoice <- c("chicken")
# sspChoices <- "ssp126"
# startyearChoices <- 2081
# modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

THIfun <- function() {
  startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
  indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  indices <- paste0("X", as.character(indices))
  indices_day <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
  indices_day <- as.numeric(indices_day)
  
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    fileName_rh_mean <- paste0(locOfFiles, "dyMean20yr_", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
    fileName_tmax_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
    fileName_tmin_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
    fileName_tas_mean <- paste0(locOfFiles,  "dyMean20yr_", modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
    
    rh.mean <- rast(fileName_rh_mean)
    cat(red(paste0("rh.mean min: ", round(min(minmax(rh.mean)), 2), ", rh.mean max: ", round(max(minmax(rh.mean)), 2), ", rh file: ", fileName_rh_mean, "\n")))
    tmax.mean <- rast(fileName_tmax_mean)
    cat(red(paste0("tmax.mean min: ", round(min(minmax(tmax.mean)), 2), ", tmax.mean max: ", round(max(minmax(tmax.mean)), 2), ", tmax file: ", fileName_tmax_mean, "\n")))
    tmin.mean <- rast(fileName_tmin_mean)
    cat(red(paste0("tmin.mean min: ", round(min(minmax(tmin.mean)), 2), ", tmin.mean max: ", round(max(minmax(tmin.mean)), 2), ", tmin file: ", fileName_tmin_mean, "\n")))
    tas.mean <- rast(fileName_tas_mean)
    cat(red(paste0("tas.mean min: ", round(min(minmax(tas.mean)), 2), ", tas.mean max: ", round(max(minmax(tas.mean)), 2), ", tmax file: ", fileName_tmax_mean, "\n")))
    
    if ((max(minmax(tas.mean)) | max(minmax(tmin.mean))) > max(minmax(tmax.mean))) stop("something is wrong!!")
    
    comb <- sds(rh.mean, tmax.mean)
    
    for (m in speciesChoice) {
      if (m %in% c("humans")) {extremeStress <- extremeStress_humans
      } else {
        extremeStress <- bpList[species %in% m, extremeStress]
      }
      fileName_out <- paste0("data/cmip6/THI/thi.", m, "_",  i, "_", k, "_", yearSpan, ".tif")
      print(paste0("fileName out: ", fileName_out))
      funName <- paste0("THIfun_", m)
      
      if (m %in% "chicken") {
        #         tmin.mean <- rast(fileName_tmin_mean)
        comb_wb <- sds(rh.mean, tas.mean)
        system.time(wbulb <- lapp(comb, fun = wetbulb))
        comb_chicken <- sds(tas.mean, wbulb)
        print(system.time(r_out <- lapp(comb_chicken, THIfun_chicken, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
        cat(red(paste0("thi chicken min: ", min(minmax(r_out)), ", thi chicken max: ", max(minmax(r_out)), ", thi chicken out file: ", fileName_out, "\n\n" )))
        ct_small <- sum(r_out < extremeStress, na.rm = FALSE)
        ct_large <- sum(r_out >= extremeStress, na.rm = FALSE)
        ct_small_sum <- global(ct_small, sum, na.rm = TRUE)
        ct_large_sum <- global(ct_large, sum, na.rm = TRUE)
        ratio <- ct_small_sum/ct_large_sum
        results <- c(m, k, l, i, ct_small_sum, ct_large_sum)
        resultsStorage <- data.table::rbindlist(list(resultsStorage, results), use.names = FALSE)
        cat(red(paste0("species: ", m, ", ssp: ", k, ", start year: ", l, ", model: ", i, ", ratio of small to large: ", round(ratio, 2)), "\n\n"))
        # plot(ct_small)
      }
      if (m %in% c("humans", "sheep", "swine")) {
        tas.mean <- rast(fileName_tas_mean)
        print(paste0("tas.mean min: ", round(min(minmax(tas.mean)), 2), ", tas.mean max: ", round(max(minmax(tas.mean)), 2)))
        
        comb_humans <- sds(rh.mean, tas.mean)
        comb_humans <- comb_humans
        funName <- paste0("THIfun_", m)
        # generate a SpatRaster of productive capacity values and write out the file
        print(system.time(r_out <- lapp(comb_humans, fun = funName, filename = fileName_out, overwrite =TRUE, wopt = woptList))) #parse(eval(text = funName)
        col = (colorList)
        if (m %in% c( "sheep", "swine"))  col = rev(colorList)
        plot(r_out, 1, main = paste0(m, " ", i, " ", k, " ", l, ", Jan 1"), ylim = c(-60, 90), range = c(0, 100), col = col, breaks = c(0, 40, 60, 80, 100), axes = FALSE)
        ct_small <- sum(r_out < extremeStress, na.rm = FALSE)
        ct_large <- sum(r_out >= extremeStress, na.rm = FALSE)
        ct_small_sum <- global(ct_small, sum, na.rm = TRUE)
        ct_large_sum <- global(ct_large, sum, na.rm = TRUE)
        ratio <- ct_small_sum/ct_large_sum
        results <- c(m, k, l, i, ct_small_sum, ct_large_sum)
        resultsStorage <- data.table::rbindlist(list(resultsStorage, results), use.names = FALSE)
        
        cat(paste0(red("humProd min: ", round(min(minmax(r_out)), 2), ", humProd max: ", round(max(minmax(r_out)), 2), ", hum prod out file: ", fileName_out), "\n\n" ))
        print(r_out)
        breaks = c(bpList[species %in% m, zeroLevel], bpList[species %in% m, noStress], bpList[species %in% m, moderateStress],bpList[species %in% m, extremeStress], max(minmax(r_out)))
        breaks <- round(breaks, 0)
        plot(r_out, 1, main = paste0(m, " ", i, " ", k, " ", l, ", Jan 1"), ylim = c(-60, 90), range = c(0, 100), col = col, breaks = breaks, axes = FALSE)
      } 
      if (m %in% c("cattle", "goat") ) { #, "swine", "sheep"
        print(system.time(r_out <- lapp(comb, fun = funName, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
        col <- rev(colorList)
        plot(r_out, 1, main = paste0(m, " ", i, " ", k, " ", l, ", Jan 1"), ylim = c(-60, 90), range = c(0, 100), col = col, breaks = c(0, 40, 60, 80, 100), axes = FALSE)
        ct_small <- sum(r_out < extremeStress, na.rm = FALSE)
        ct_large <- sum(r_out >= extremeStress, na.rm = FALSE)
        ct_small_sum <- global(ct_small, sum, na.rm = TRUE)
        ct_large_sum <- global(ct_large, sum, na.rm = TRUE)
        ratio <- ct_small_sum/ct_large_sum
        results <- c(m, k, l, i, ct_small_sum, ct_large_sum)
        resultsStorage <- data.table::rbindlist(list(resultsStorage, results), use.names = FALSE) 
        cat(red(paste0("species: ", m, ", ssp: ", k, ", start year: ", l, ", model: ", i, red(", ratio of small to large: ", round(ratio, 2)), "\n\n")))
        print(paste0("app done"))
        gc()
      }
      cat(paste("Done", "\n"))
    }
  }
}

for (k in sspChoices) {
  #    k = "ssp126"
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    THIfun()
  }
}

#THI historical -----
k <- "historical"
l = 1991
yearSpan <- paste0(l, "_", l + yearRange)
THIfun()

# ensemble means -----
# combine all the spatrasters by model for the time period and then take the mean across that combo
#speciesChoice <- c("chicken")

ensembleTHIfun <- function() {
  
  # cattle and goats have the same 
  for (m in speciesChoice) {
    if (m %in% "humans") {extremeStress <- extremeStress_humans
    } else {
      extremeStress <- bpList[species %in% m, extremeStress]
    }
    print(paste0("species Choice m: ", m))
    x <- lapply(modelChoices, readRast_thi_ensemble)
    r <- rast(x)
    indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
    maxVal <- round(max(minmax(r)), 2)
    minVal <- round(min(minmax(r)), 2)
    #     print(r)
    fileName_out <- paste0("data/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
    cat(paste0(red("species: ", m, ", ensemble ssp: ", k, ", start year: ", l, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName out: ", fileName_out), "\n\n"))
    print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
    names(r.mean) <- gsub("X", "Day ", names(r.mean))
    
    cat(paste0(red("species: ",m, "thi min: ", round(min(minmax(r.mean)), 2), "thi max: ", round(max(minmax(r.mean)), 2), "\n\n")))
    ct_small <- sum(r.mean < extremeStress, na.rm = FALSE)
    ct_large <- sum(r.mean >= extremeStress, na.rm = FALSE)
    ct_small_sum <- global(ct_small, sum, na.rm = TRUE)
    ct_large_sum <- global(ct_large, sum, na.rm = TRUE)
    ratio <- ct_small_sum/ct_large_sum
    results <- c(m, k, l, "ensemble", ct_small_sum, ct_large_sum)
    resultsStorage <- data.table::rbindlist(list(resultsStorage, results), use.names = FALSE)
    cat(paste0(red("species: ", m, ", ensemble ssp: ", k, ", start year: ", l, ", ratio of small to large: ", round(ratio, 2)), "\n"))
    
    cat((paste0(red("ensemble mean min: ", round(min(minmax(r.mean)), 2), ", ensemble mean max: ", round(max(minmax(r.mean)), 2), ", ensemble mean out file: ", fileName_out), "\n\n")))
    #    r.mean <- NULL
    gc()
  }
}

for (k in sspChoices) {
  #  k <- "ssp585"
  for (startYear in startyearChoices) {
    l <- startYear
    yearSpan <- paste0(l, "_", l + yearRange)
    ensembleTHIfun()
  }
}

# ensemble means, historical -----
#speciesChoice <- c("chicken")
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
ensembleTHIfun()

fileName_resultsStorage <- "data/CMIP6/THI/results/resultsStorage_all.csv"

write.csv(resultsStorage, fileName_resultsStorage, row.names = FALSE)

# mpegs of ensemble means -----

#speciesChoice <- c("humans")
#scenarioChoicesEnsemble <- "ssp585"
for (k in scenarioChoicesEnsemble) {
  #  k <- "ssp126"
  for (startYear in startyearChoices) {
    l <- startYear
    yearSpan <- paste0(l, "_", l + yearRange)
    yearnumberRange <- seq(l, (l + yearRange), 1)
    if (k %in% "historical") {yearSpan <- "1991_2010"; l <- 1991}
    
    # cattle and goats have the same 
    for (m in speciesChoice) {
      gc()
      # for (m in "humans") {
      print(paste0("species choice m: ", m))
      fileName_in <- paste0("data/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
      r <- rast(fileName_in)
      maxVal <- round(max(minmax(r)), 2)
      minVal <- round(min(minmax(r)), 2)
      print(paste0("var: ", m, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName in: ", fileName_in))
      
      # set up names with Jan-1. Need to set up the endDate_year with a leap year such as 2040
      startDate_year <- paste0(2040, "-01-01")
      endDate_year <- paste0(2040, "-12-31")
      indices_date <- seq(as.Date(startDate_year), as.Date(endDate_year), 1)
      indices_date <- format(indices_date, "%b %d")
      #      names(r) <- gsub("X", "Day.", names(r))
      videoName_out <- paste0("graphics/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".mp4")
      print(paste0("video fileName out: ", videoName_out))
      #     x.cv <- tapp(x, indices_day, fun = cv, na.rm = TRUE)
      # system.time(r.mean <- tapp(r, indices_day, fun = mean, na.rm = TRUE, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      # names(r.mean) <- gsub("X", "Day ", names(r.mean))
      # # r.mean_stack <- raster::stack(r.mean)
      # # names(r.mean_stack) <- gsub("X", "Day ", names(r.mean_stack))
      # par(usr = c(-150, 150, -90, 90))
      if (m %in% "humans") {
        title_animate <- paste0("Physical work capacity (%), ", k, ", ", gsub("_", "-", yearSpan))
        names(r) <- paste0(title_animate, ", ", indices_date)
        
        animation::saveVideo(animate( r, n=1, ylim = c(-60, 90), range = c(0, 100), pause = .001, sub = title_animate, col = (colorList), breaks = c(0, 40, 60, 80, 100), axes = FALSE), 
                             ani.height = 800, ani.width = 1200, video.name = videoName_out)
      } else {
        title_animate <- paste0("Temperature Humidity Index (THI) value, ", m, " ", k,  ", ", gsub("_", "-", yearSpan))
        names(r) <- paste0(title_animate, ", ", indices_date)
        breaks = c(bpList[species %in% m, zeroLevel], bpList[species %in% m, noStress], bpList[species %in% m, moderateStress],bpList[species %in% m, extremeStress], max(minmax(r)))
        breaks <- round(breaks, 0)
        rangeMaximum <- max(minmax(r))
        animation::saveVideo(animate( r, n=1, ylim = c(-60, 90), range = c(0, rangeMaximum), pause = .001, sub = title_animate, col = rev(colorList), breaks = breaks, axes = FALSE), 
                             ani.height = 800, ani.width = 1200, video.name = videoName_out)
      }
    }
  }
}

# extreme stress scenarios -------
library(data.table)
funextTHI <- function(cellVector) {
  if (m %in% "humans") {
    extremeCt <- sum(cellVector < extremeStress, na.rm = FALSE)
  } else {extremeCt <- sum(cellVector > extremeStress, na.rm = FALSE)
  }
  return(extremeCt) 
}

for (m in speciesChoice) {
  if (m %in% "humans") {extremeStress <- extremeStress_humans
  } else {
    extremeStress <- bpList[species %in% m, extremeStress]
  }
  for (k in sspChoices) {
    # for (k in c("ssp585")) {
    for (startYear in startyearChoices) {
      #    for (startYear in 2041) {
      l <- startYear
      yearSpan <- paste0(l, "_", l + yearRange)
      #      yearnumberRange <- seq(l, (l + yearRange), 1)
      if (k %in% "historical") {yearSpan <- "1991_2010"; l <- 1991}
      
      fileName_in <- paste0("data/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
      r <- rast(fileName_in)
      maxVal <- round(max(minmax(r)), 2)
      minVal <- round(min(minmax(r)), 2)
      print(paste0("var: ", m, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName in: ", fileName_in))
      
      print(system.time(extremeCt <- app(r, funextTHI)))
      fileName_out <- paste0("data/cmip6/THI/extremeCt.", m, "_", k, "_", yearSpan, ".tif")
      print(paste0("fileName out: ", fileName_out))
      writeRaster(extremeCt, fileName_out, overwrite = TRUE, woptList = woptList )
      title <- paste0(m, ", days above extreme stress value of ", extremeStress, ", ensemble means, ", k, ", ", yearSpan)
      title <- paste0(m, ", days above extreme stress value of ", extremeStress, ", \nensemble means, ", k, ", ", yearSpan)
      col <- rev(colorList)
      breaks = seq(from = 0, to = max(minmax(extremeCt)), length.out = 5)
      breaks <- round(breaks, 0)
      
      plot(extremeCt, 1, main = title, ylim = c(-60, 90), range = c(0, 100), col = col, breaks = breaks, axes = FALSE)
      
    }
  }
}

# extreme stress, historical -----
k <- "historical"
l <- 1991
for (m in speciesChoice) {
  if (m %in% "humans") {extremeStress <- extremeStress_humans
  } else {
    extremeStress <- bpList[species %in% m, extremeStress]
  }
  yearSpan <- paste0(l, "_", l + yearRange)
  #  yearnumberRange <- seq(l, (l + yearRange), 1)
  if (k %in% "historical") {yearSpan <- "1991_2010"; l <- 1991}
  
  fileName_in <- paste0("data/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
  r <- rast(fileName_in)
  maxVal <- round(max(minmax(r)), 2)
  minVal <- round(min(minmax(r)), 2)
  print(paste0("var: ", m, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName in: ", fileName_in))
  
  print(system.time(extremeCt <- app(r, funextTHI)))
  fileName_out <- paste0("data/cmip6/THI/extremeCt.", m, "_", k, "_", yearSpan, ".tif")
  print(paste0("fileName out: ", fileName_out))
  writeRaster(extremeCt, fileName_out, overwrite = TRUE, woptList = woptList )
  title <- paste0(m, ", days above extreme stress value of ", extremeStress, ", \nensemble means, ", k, ", ", yearSpan)
  col <- rev(colorList)
  breaks = seq(from = 0, to = max(minmax(extremeCt)), length.out = 5)
  breaks <- round(breaks, 0)
  
  plot(extremeCt, 1, main = title, ylim = c(-60, 90), range = c(0, 100), col = col, breaks = breaks, axes = FALSE)
}

#  do graphics -----
library(ggplot2)
library(RColorBrewer)
library(rworldmap)
library(maps)
#remotes::install_github("ropensci/rnaturalearthhires") need to do once to get the library from github
library(rnaturalearthhires)
library(ggspatial)
library(data.table)
library(readxl)
library(sf)
library(dplyr)
# colorList defined at the top of the script and below

coastline <- st_read("data-raw/regionInformation/ne_50m_coastline/ne_50m_coastline.shp")

#function to get rid of Antarctica
crop_custom <- function(poly.sf) {
  poly.sp <- as(poly.sf, "Spatial")
  extR <- raster::extent(c(-180, 180, -60, 90))
  poly.sp.crop <- crop(poly.sp, extR)
  st_as_sf(poly.sp.crop)
}
coastline <- crop_custom(coastline)
#st_crop(coastline, c(xmin=-180, xmax= 180, ymin = -60, ymax = 90))
RobinsonProj <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
crsRob <- RobinsonProj
coastline <- st_transform(coastline, crsRob)

yearSpan_begin <- "1991_2010"
yearSpan_end <- "2081_2100"
legendTitle <- "Days"
# test values
m = "chicken"

for (m in speciesChoice) {
  if (m %in% "humans") {extremeStress <- extremeStress_humans
  } else {
    extremeStress <- bpList[species %in% m, extremeStress]
  }
  fileName_century_begin <- paste0("data/cmip6/THI/extremeCt.", m, "_", "historical", "_", yearSpan_begin, ".tif")
  r_begin <- rast(fileName_century_begin)
  names(r_begin) <- "value"
  fileName_r_mask <- paste0("data/animals/raster_ct_", m, ".tif")
  r_mask <- rast(fileName_r_mask)
  if (m %in% "cattle") maskMin <- 5000
  #  if (m %in% "humans") maskMin <- 10 commented out to see if that affects the middle east values
  if (m %in% "humans") maskMin <- 0 
  if (m %in% "swine") maskMin <- 1000
  if (m %in% "chicken") maskMin <- 20000
  if (m %in% "goat") maskMin <- 5000
  if (m %in% "sheep") maskMin <- 3000
  r_mask[r_mask < maskMin] <- NA
  r_mask[r_mask > 0] <- 1
  r_begin <- mask(r_begin, r_mask)
  r_begin <- crop(r_begin, ext_noAntarctica)
  r_begin <- project(r_begin, crsRob)
  r_begin_df <- as.data.frame(r_begin, xy = TRUE)
  
  #sspChoices <- c("ssp126", "ssp585")
  #  for (k in sspChoices) {
  fileName_century_end_ssp585 <- paste0("data/cmip6/THI/extremeCt.", m, "_", "ssp585", "_", yearSpan_end, ".tif")
  r_end_ssp585 <- rast(fileName_century_end_ssp585)
  
  r_end_ssp585 <- mask(r_end_ssp585, r_mask)
  r_end_ssp585 <- crop(r_end_ssp585, ext_noAntarctica)
  r_end_ssp585 <- project(r_end_ssp585, crsRob)
  
  fileName_century_end_ssp126 <- paste0("data/cmip6/THI/extremeCt.", m, "_", "ssp126", "_", yearSpan_end, ".tif")
  r_end_ssp126 <- rast(fileName_century_end_ssp126)
  r_end_ssp126 <- mask(r_end_ssp126, r_mask)
  r_end_ssp126 <- crop(r_end_ssp126, ext_noAntarctica)
  r_end_ssp126 <- project(r_end_ssp126, crsRob)
  
  r_delta_ssp585 <- r_end_ssp585 - r_begin
  names(r_end_ssp585) <-  names(r_delta_ssp585) <- "value"
  
  r_delta_ssp126 <- r_end_ssp126 - r_begin
  names(r_end_ssp126) <-  names(r_delta_ssp126) <- "value"
  
  
  # convert to data frame to use with ggplot
  r_end_ssp126_df <- as.data.frame(r_end_ssp126, xy = TRUE)
  r_delta_ssp126_df <- as.data.frame(r_delta_ssp126, xy = TRUE)
  
  r_end_ssp585_df <- as.data.frame(r_end_ssp585, xy = TRUE)
  r_delta_ssp585_df <- as.data.frame(r_delta_ssp585, xy = TRUE)
  
  for (i in c("r_begin_df", "r_end_ssp585_df", "r_delta_ssp585_df", "r_end_ssp126_df", "r_delta_ssp126_df")) {
    colorList <- (RColorBrewer::brewer.pal(5, "YlOrRd")) # yellow to red
    
    if (i %in% "r_begin_df") {
      titleText <- paste0(m, ", days above extreme stress value, ", gsub("_", "-", yearSpan_begin))
      if (m %in% "humans") {
        titleText <- paste0(m, ", days with PWC below ", extremeStress, " percent, ", gsub("_", "-", yearSpan_begin))
        colorList <- rev(colorList) # red to yellow
      }
      
      outFilename <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", "historical", "_", yearSpan_begin, ".png")
      r <- r_begin_df}
    if (i %in% "r_end_ssp585_df") {
      titleText <- paste0(m, ", days above extreme stress value, ", " ssp585, ", " ", gsub("_", "-", yearSpan_end))
      if (m %in% "humans") {
        titleText <- paste0(m, ", days with PWC below ", extremeStress, " percent" , ", ssp585, ", gsub("_", "-", yearSpan_begin))
        outFilename <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", "ssp585", "_", yearSpan_end, ".png")
        colorList <- rev(colorList) # red to yellow
      }
      r <- r_end_ssp585_df}
    if (i %in% "r_delta_ssp585_df") {
      titleText <- paste0(m, ", change in days above extreme stress value, \n early to end century ", "ssp585")
      if (m %in% "humans") titleText <- paste0(m, ", change in days with PWC below ", extremeStress, " percent, \n early to end century ", "ssp585")
      outFilename <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", "ssp585", "_", yearSpan_end, ".png")
      r <- r_delta_ssp585_df}
    if (i %in% "r_end_ssp126_df") {
      titleText <- paste0(m, ", days above extreme stress value, ", " ssp126", " ", gsub("_", "-", yearSpan_end))
      if (m %in% "humans") {
        titleText <- paste0(m, ", days with PWC below ", extremeStress, " percent" , ", ssp126, ", gsub("_", "-", yearSpan_end))
        colorList <- rev(colorList) # red to yellow
      }
      outFilename <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", "ssp126", "_", yearSpan_end, ".png")
      r <- r_end_ssp126_df}
    if (i %in% "r_delta_ssp126_df") {
      titleText <- paste0(m, ", change in days above extreme stress value, \n early to end century ", "ssp585")
      if (m %in% "humans") titleText <- paste0(m, ", change in days with PWC below ", extremeStress, " percent, \n early to end century ", "ssp126")
      outFilename <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", "ssp126", "_", yearSpan_end, ".png")
      r <- r_delta_ssp126_df
    }
    gc()
    
    maxVal <- max(minmax(r_end_ssp585)) 
    #   if (maxVal > 150) maxVal <- 5
    custom_bins <- round(seq.int(from = 0, to = maxVal, length = 4))
    r$value[r$value > maxVal] <- maxVal #set values > maxVal to maxVal
    g <- ggplot(data = coastline) +
      labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
      labs(x = "", y = "") +
      geom_raster(data = r, aes(x, y, fill = value)) +
      scale_fill_gradientn(colours = colorList, na.value = "white",
                           breaks = custom_bins,labels = custom_bins,
                           limits = c(0, maxVal)) + 
      geom_sf(fill = NA, color = "gray", lwd = 0.2) +
      theme_bw() +
      theme(legend.text.align = 1)
    print(g)
    ggsave(filename = outFilename, plot = g, width = 6, height = 6, units = "in", dpi = 300)
    print(paste0("out file name: ", outFilename))
    g <- NULL
    #     print(g)
    dev.off()
  }
}

# graphics with polygon shading -----
library(ggpattern)
for (m in speciesChoice) {
  if (m %in% "humans") {extremeStress <- extremeStress_humans
  } else {
    extremeStress <- bpList[species %in% m, extremeStress]
  }
  fileName_century_begin <- paste0("data/cmip6/THI/extremeCt.", m, "_", "historical", "_", yearSpan_begin, ".tif")
  r_begin <- rast(fileName_century_begin)
  names(r_begin) <- "value"
  fileName_r_mask <- paste0("data/animals/raster_ct_", m, ".tif")
  r_mask <- rast(fileName_r_mask)
  if (m %in% "cattle") maskMin <- 5000
  #  if (m %in% "humans") maskMin <- 10 commented out to see if that affects the middle east values
  if (m %in% "humans") maskMin <- 0 
  if (m %in% "swine") maskMin <- 1000
  if (m %in% "chicken") maskMin <- 10000
  if (m %in% "goat") maskMin <- 5000
  if (m %in% "sheep") maskMin <- 3000
  r_mask[r_mask < maskMin] <- NA
  r_mask[r_mask > 0] <- 1
  r_mask_pg <- as.polygons(r_mask)
  test <- as(r_mask_pg, "Spatial") # convert the SpatVector to a spatial SpatialPolygonsDataFrame
  
  r_begin <- mask(r_begin, r_mask)
  r_begin <- crop(r_begin, ext_noAntarctica)
  r_begin <- project(r_begin, crsRob)
  r_begin_df <- as.data.frame(r_begin, xy = TRUE)
  
  #sspChoices <- c("ssp126", "ssp585")
  #  for (k in sspChoices) {
  fileName_century_end_ssp585 <- paste0("data/cmip6/THI/extremeCt.", m, "_", "ssp585", "_", yearSpan_end, ".tif")
  r_end_ssp585 <- rast(fileName_century_end_ssp585)
  
  r_end_ssp585 <- mask(r_end_ssp585, r_mask)
  r_end_ssp585 <- crop(r_end_ssp585, ext_noAntarctica)
  r_end_ssp585 <- project(r_end_ssp585, crsRob)
  
  fileName_century_end_ssp126 <- paste0("data/cmip6/THI/extremeCt.", m, "_", "ssp126", "_", yearSpan_end, ".tif")
  r_end_ssp126 <- rast(fileName_century_end_ssp126)
  r_end_ssp126 <- mask(r_end_ssp126, r_mask)
  r_end_ssp126 <- crop(r_end_ssp126, ext_noAntarctica)
  r_end_ssp126 <- project(r_end_ssp126, crsRob)
  
  r_delta_ssp585 <- r_end_ssp585 - r_begin
  names(r_end_ssp585) <-  names(r_delta_ssp585) <- "value"
  
  r_delta_ssp126 <- r_end_ssp126 - r_begin
  names(r_end_ssp126) <-  names(r_delta_ssp126) <- "value"
  
  
  # convert to data frame to use with ggplot
  r_end_ssp126_df <- as.data.frame(r_end_ssp126, xy = TRUE)
  r_delta_ssp126_df <- as.data.frame(r_delta_ssp126, xy = TRUE)
  
  r_end_ssp585_df <- as.data.frame(r_end_ssp585, xy = TRUE)
  r_delta_ssp585_df <- as.data.frame(r_delta_ssp585, xy = TRUE)
  
  for (i in c("r_begin_df", "r_end_ssp585_df", "r_delta_ssp585_df", "r_end_ssp126_df", "r_delta_ssp126_df")) {
    if (i %in% "r_begin_df") {
      titleText <- paste0(m, ", days above extreme stress value, ", gsub("_", "-", yearSpan_begin))
      if (m %in% "humans") titleText <- paste0(m, ", days with PWC below ", extremeStress, " percent, ", gsub("_", "-", yearSpan_begin))
      outFilename <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", "historical", "_", yearSpan_begin, ".png")
      r <- r_begin_df}
    if (i %in% "r_end_ssp585_df") {
      titleText <- paste0(m, ", days above extreme stress value, ", " ssp585, ", " ", gsub("_", "-", yearSpan_end))
      if (m %in% "humans") titleText <- paste0(m, ", days with PWC below ", extremeStress, " percent" , ", ssp585, ", gsub("_", "-", yearSpan_begin))
      outFilename <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", "ssp585", "_", yearSpan_end, ".png")
      r <- r_end_ssp585_df}
    if (i %in% "r_delta_ssp585_df") {
      titleText <- paste0(m, ", change in days above extreme stress value, \n early to end century ", "ssp585")
      if (m %in% "humans") titleText <- paste0(m, ", change in days with PWC below ", extremeStress, " percent, \n early to end century ", "ssp585")
      outFilename <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", "ssp585", "_", yearSpan_end, ".png")
      r <- r_delta_ssp585_df}
    if (i %in% "r_end_ssp126_df") {
      titleText <- paste0(m, ", days above extreme stress value, ", " ssp126", " ", gsub("_", "-", yearSpan_end))
      if (m %in% "humans") titleText <- paste0(m, ", days with PWC below ", extremeStress, " percent" , ", ssp126, ", gsub("_", "-", yearSpan_end))
      outFilename <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", "ssp126", "_", yearSpan_end, ".png")
      r <- r_end_ssp126_df}
    if (i %in% "r_delta_ssp126_df") {
      titleText <- paste0(m, ", change in days above extreme stress value, \n early to end century ", "ssp585")
      if (m %in% "humans") titleText <- paste0(m, ", change in days with PWC below ", extremeStress, " percent, \n early to end century ", "ssp126")
      outFilename <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", "ssp126", "_", yearSpan_end, ".png")
      r <- r_delta_ssp126_df
    }
    gc()
    
    maxVal <- max(minmax(r_end_ssp585)) 
    #   if (maxVal > 150) maxVal <- 5
    custom_bins <- round(seq.int(from = 0, to = maxVal, length = 4))
    r$value[r$value > maxVal] <- maxVal #set values > maxVal to maxVal
    g <- ggplot(data = coastline) +
      labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
      labs(x = "", y = "") +
      geom_raster(data = r, aes(x, y, fill = value)) +
      scale_fill_gradientn(colours = colorList, na.value = "white",
                           breaks = custom_bins,labels = custom_bins,
                           limits = c(0, maxVal)) + 
      geom_sf(fill = NA, color = "gray", lwd = 0.2) +
      theme_bw() +
      theme(legend.text.align = 1) +
      ggpattern::geom_polygon_pattern(data=test, aes(x=long, y=lat), 
                                      pattern = 'stripe', fill   = 'white', colour  = 'black')
    
    print(g)
    ggsave(filename = outFilename, plot = g, width = 6, height = 6, units = "in", dpi = 300)
    print(paste0("out file name: ", outFilename))
    g <- NULL
    #     print(g)
    dev.off()
  }
}

library(rgl)
plot3d(x = r_end_ssp126_df$x, y = r_end_ssp126_df$y, z = r_end_ssp126_df$value, col = colorList,
       xlab="longitude", ylab="latitude", zlab="Count of extreme stress")

library(plotly)
plot_ly(x = r_end_ssp126_df$x, y = r_end_ssp126_df$y, z = r_end_ssp126_df$value, colors = colorList, type = "scatter3d", mode = "markers",
        xlab="longitude", ylab="latitude", zlab="Count of extreme stress")
# # get animal raster masks
# animalsOnly <- speciesChoice[!speciesChoice %in% "humans"]
# for (m in animalsOnly) {
#   if (m %in% "cattle") cutoff = 2000
#   fileName_r_mask <- paste0("data/animals/raster_ct_", m, ".tif")
#   r_mask <- rast(fileName_r_mask)
#   values(r_mask)[values(r_mask) < 2000] <- NA
#   r_delta_mask <- mask(r_delta, r_mask)
# }
# r_delta_mask <- mask(r_delta, pop_mask)
# pop_mask
# #plot data with masking
# 
# for (m in speciesChoice) {
#   fileName_century_end <- paste0("data/cmip6/THI/extremeCt.", m, "_", k, "_", yearSpan_end, ".tif")
#   fileName_century_begin <- paste0("data/cmip6/THI/extremeCt.", m, "_", "historical", "_", yearSpan_begin, ".tif")
#   r_raster <- 
#     r_end <- rast(fileName_century_end)
#   r_begin <- rast(fileName_century_begin)
#   r_delta <- r_end - r_begin
#   
#   r_delta_df <- as.data.frame(r_delta_mask, xy = TRUE)
#   names(r_delta_df)[names(r_delta_df) == "lyr.1"] <- "value"
#   
#   titleText <- paste0(m, ", change in days above extreme stress value, \n\n early to end century ", "SSP585 ")
#   
#   legendTitle <- "Change in \n\nHigh Stress Days"
#   print(titleText)
#   gc()
#   colorList <- (RColorBrewer::brewer.pal(5, "YlOrRd"))
#   maxVal <- 250
#   custom_bins <- round(seq.int(from = 0, to = maxVal, length = 6))
#   r <- r_delta_df
#   r$value[r$value > maxVal] <- maxVal
#   g <- ggplot(data = coastline) +
#     labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
#     labs(x = "", y = "") +
#     
#     geom_raster(data = r, aes(x, y, fill = value)) +
#     scale_fill_gradientn(colours = colorList, na.value = "white",
#                          breaks = custom_bins,labels = custom_bins,
#                          limits = c(0, maxVal)) + 
#     geom_sf(fill = NA, color = "gray")
#   g
#   outFilename <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", k, ".png")
#   png(filename = outFilename, width = 6, height = 6, units = "in", res = 300)
#   print(g)
#   dev.off()
# }
# 
# # convert population density raster
# pop_mask <- rast("data-raw/gpw_v4_population_density_rev11_2020_30_min.tif")
# setMinMax(pop_mask)
# fileName_r_mask <- paste0("data/animals/raster_ct_", "humans", ".tif")
# writeRaster(pop_mask, fileName_r_mask, overwrite = TRUE, woptList = woptList)
# 
# maskValUpper <- 3000
# maskValLower <- 10
# pop_mask > maskValUpper
# pop_mask[pop_mask > maskValUpper] <- NA
# pop_mask[pop_mask < maskValLower] <- NA
# pop_mask[pop_mask > 0] <- 1
# 
# fileName_in <- "data/cmip6/THI/ensemble_thi.humans_ssp585_2081_2100.tif"
# test <- rast(fileName_in)
# test
# test_brick <- raster::brick(fileName_in)
# test_brick
# test_anim <- raster::animate(test_brick, 1)
# 
# # calculate number of sequential days with stress; minimum of 5
# 
# fun <- function(cellVector) {
#   startend <- c(NA, NA) 
#   # if (is.nan(cellVector[1])) {
#   #   return(startend)
#   # }
#   g <- gregexpr("1{5,}", paste(+(cellVector > 89), collapse = ""))[[1]]
#   print(g)
#   if (!g[[1]] == -1) { # no need to write to growing season if g returns 1
#     startend[1] <- g[[1]]
#     matchLength <- attributes(g)$match.length
#     startend[2] <- startend[1] + matchLength - 1
#   }
#   return(startend) 
# }
# 
# fileName_in <- paste0("data/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
# r <- rast(fileName_in)
# 
# print(system.time(extremeStress <- app(r, fun)))

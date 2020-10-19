# calculate animal climate stress
#source("R/globallyUsed.R")
library("terra")
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
  terraOptions(memfrac = 1,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
}else{
  terraOptions(memfrac = .6,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
}


locOfFiles <- locOfCMIP6tifFiles
locOfFiles <- "data/bigFiles/"

sspChoices <- c("ssp126", "ssp585") 
sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
# modelChoices <- c( "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
#startyearChoices <-  c(2041) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_historical <- c(1991)
scenarioChoicesEnsemble <- c("historical", sspChoices)
northernHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-c( -180, 180, -90, 0)
hemisphere <- c("NH", "SH")

yearRange <- 19
yearRangeSH <- 18 # one less year because of 6 month offset

woptList <- list(gdal=c("COMPRESS=LZW"))

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2041
yearNumber <- 2043

readRast_thi_cattle <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/THI/thi.cattle_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  r <- rast(fileNameIn)
  r <- round(r, 2)
}
readRast_thi_cattleEnsemble <- function(i) {
  fileNameIn <- paste0("data/cmip6/THI/thi.cattle_", i, "_", k,  "_", yearSpan, ".tif") # note yearSpan here
  print(fileNameIn)
  r <- rast(fileNameIn)
  #  r <- round(rast(fileNameIn), 2)
}

readRast_thi_sheep <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/THI/thi.sheep_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  print(fileNameIn)
  r <- round(rast(fileNameIn), 2)
}
readRast_thi_goat <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/THI/thi.goat_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  print(fileNameIn)
  r <- round(rast(fileNameIn), 2)
}
readRast_thi_chicken <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/THI/thi.chicken_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  print(fileNameIn)
  r <- round(rast(fileNameIn), 2)
}

readRast_thi_swine <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/THI/thi.swine_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  print(fileNameIn)
  r <- round(rast(fileNameIn), 2)
}

THIfun_cattle <- function(cellVector) {
  rh <- cellVector[1:vlend]
  tmax <- cellVector[v2start:nl]
  if (is.nan(cellVector[1])) { return(rep(NA, vlend)) }
  #  print(paste("length tmax: ", (length(tmax))))
  thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))
  thi[tmax < 20] <- 0
  thi <- round(thi, 2)
}


THIfun_sheep <- function(cellVector) {
  rh <- cellVector[1:vlend]
  tmax <- cellVector[v2start:nl]
  if (is.nan(cellVector[1])) { return(rep(NA, vlend)) }
  #  print(paste("length tmax: ", (length(tmax))))
  thi <- tmax - ((0.31 - (0.31 * (rh / 100))) * (tmax - 14.4)) 
  thi[tmax < 20] <- 0
  thi <- round(thi, 2)
}

THIfun_goat <- function(cellVector) {
  rh <- cellVector[1:vlend]
  tmax <- cellVector[v2start:nl]
  if (is.nan(cellVector[1])) { return(rep(NA, vlend)) }
  #  print(paste("length tmax: ", (length(tmax))))
  thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) 
  thi[tmax < 20] <- 0
  thi <- round(thi, 2)
}

THIfun_swine <- function(cellVector) {
  rh <- cellVector[1:vlend]
  tmax <- cellVector[v2start:nl]
  if (is.nan(cellVector[1])) { return(rep(NA, vlend)) }
  #  print(paste("length tmax: ", (length(tmax))))
  thi <- tmax - (0.55 - (0.0055 * rh) * (tmax - 14.5))
  thi[tmax < 20] <- 0
  thi <- round(thi, 2)
}

THIfun_chicken <- function(tmax) {
  thi <- 0.60 * tmax + 0.40 * tmin # using broiler formula. Note. no rh needed
  thi[thi < 0 ] <- 0
}

#THI scenarios -----
speciesChoice <- c("goat", "swine", "chicken", "sheep") # cattle
speciesChoice <- "cattle"
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange + 1) # the +1 here is to get at the original file names
      modelName.lower <- tolower(i)
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      fileName_tasmax <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      tmax <- rast(fileName_tasmax)
      fileName_rh <- paste0(locOfFiles, modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      rh <- rast(fileName_rh)
      
      combined <- c(rh, tmax)
      nl <- nlyr(combined)
      vlend <- nl/2
      v2start <- 1 + vlend
      
      for (yearNumber in l:(l + yearRange)) {
        gc()
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        
        print(system.time(tmax_yr <- subset(tmax, indicesCharYear)))
        print(system.time(rh_yr <- subset(rh, indicesCharYear)))
        for (m in speciesChoice) {
          
          fileName_out <- paste0("data/cmip6/THI/thi.", speciesChoice, "_",  i, "_", k, "_", yearNumber, ".tif")
          funName <- paste0("THIfun_", speciesChoice)
          if (speciesChoice %in% "chicken") {
            comb_chicken <- tmax_yr
            print(system.time(r_out <- lapp(comb_chicken, THIfun_chicken, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
          } else {
            print(system.time(r_out <- app(combined, funName)))
            print(system.time(writeRaster(r_out, filename = fileName_out, overwrite=TRUE, wopt = woptList))); flush.console()
          }
        }
      }
    }
  }
  
  #THI historical -----
  k <- "historical"
  for (i in modelChoices) {
    for (l in startyearChoices_historical) {
      yearSpan <- paste0(l, "_", l + yearRange + 1) # the +1 here is to get at the original file names
      modelName.lower <- tolower(i)
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      fileName_tasmax <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      tmax <- rast(fileName_tasmax)
      fileName_rh <- paste0(locOfFiles, modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      rh <- rast(fileName_rh)
      
      for (yearNumber in l:(l + yearRange)) {
        gc()
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        
        print(system.time(tmax_yr <- subset(tmax, indicesCharYear)))
        print(system.time(rh_yr <- subset(rh, indicesCharYear)))
        comb <- sds(rh_yr, tmax_yr)
        fileName_out <- paste0("data/cmip6/THI/thi.cattle_",  i, "_", k, "_", yearNumber, ".tif")
        
        print(system.time(r_out <- lapp(comb, THIfun_cattle, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
      }
    }
  }
  
  # 20 year THI results  ------
  for (k in sspChoices) {
    for (i in modelChoices) {
      modelName.lower <- tolower(i)
      for (l in startyearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        yearnumberRange <- seq(l, (l + yearRange), 1)
        gc()
        x <- lapply(yearnumberRange, readRast_thi_cattle)
        x_start <- x
        r <- rast(x)
        fileNameOut <- paste0("data/cmip6/THI/thi.cattle_", i, "_", k,  "_", yearSpan, ".tif")
        print(paste0("fileNameOut: ", fileNameOut))
        print(system.time(writeRaster(r, fileNameOut, overwrite=TRUE, wopt = woptList))); flush.console()
        r_start <- r_end <- NULL
        gc()
      }
    }
  }
  
  # 20 year THI results, historical  ------
  k <- "historical"
  for (i in modelChoices) {
    for (l in startyearChoices_historical) {
      yearSpan <- paste0(l, "_", l + yearRange)
      yearnumberRange <- seq(l, (l + yearRange), 1)
      gc()
      x <- lapply(yearnumberRange, readRast_thi_cattle)
      r <- rast(x)
      fileNameOut <- paste0("data/cmip6/THI/thi.cattle_", i, "_", k,  "_", yearSpan, ".tif")
      print(paste0("fileNameOut: ", fileNameOut))
      print(system.time(writeRaster(r, fileNameOut, overwrite=TRUE, wopt = woptList))); flush.console()
      r_start <- r_end <- NULL
      gc()
    }
  }
}

# ensemble means -----
# combine all the spatrasters by model for the hemisphere, time period, start or end of season and scenario and then take the mean across that combo

for (k in scenarioChoicesEnsemble) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    yearnumberRange <- seq(l, (l + yearRange), 1)
    if (k %in% "historical") yearSpan <- "1991_2010"
    gc()
    x <- lapply(modelChoices, readRast_thi_cattleEnsemble)
    r <- rast(x)
    fileName_out <- paste0("data/cmip6/THI/ensemble_thi.cattle_", k, "_", yearSpan, ".tif")
    system.time(r_mean <- app(r, fun = "mean", filename = fileName_out, overwrite = TRUE, woptList = woptList))
    plot(r_mean, 1, main = paste0("Ensemble mean, THI cattle, ", k, ", period ", yearSpan))
  }
}

# ensemble deltas


# # fix thi.cattle filename
# flist <- list.files("data/cmip6/THI/")
# flist_thi.cattle <- flist[grepl("thi.cattle", flist, fixed = TRUE)]
# flist_thi.cattleToFix <- flist_thi.cattle[grepl("thi.cattleG", flist, fixed = TRUE)]
# flist_thi.cattleToFix <- unique(flist_thi.cattleToFix)
# flist_thi.cattleFixed <- gsub("thi.cattle", "thi.cattle_G", flist_thi.cattleToFix)
# flist_thi.cattleToFix <- paste0("data/cmip6/THI/", flist_thi.cattleToFix)
# flist_thi.cattleFixed <- paste0("data/cmip6/THI/", flist_thi.cattleFixed)
# for (i in 1:length(flist_thi.cattleToFix)){
#   file.rename(from = flist_thi.cattleToFix[i], to = flist_thi.cattleFixed[i])
# }

# the best function to use, in combination with lapp
THIfun_cattle <- function(rh, tmax) {
  thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))
  thi[thi < 0 ] <- 0
}

comb <- sds(rh, tmax)

fun <- function(comb) {
  thiout <- c(NA) 
  if (is.nan(cellVector[1])) {
    return(thiout)
  }
  g <- gregexpr("1{60,}", paste(+(cellVector > 0), collapse = ""))[[1]]
  if (!g[[1]] == -1) { # no need to write to growing season if g returns 1
    startend[1] <- g[[1]]
    matchLength <- attributes(g)$match.length
    startend[2] <- startend[1] + matchLength - 1
  }
  return(thiout) 
}

comb <- sds(rh_yr_NH, tmax_yr_NH)
system.time(rrr <- lapp(comb, funTHI))
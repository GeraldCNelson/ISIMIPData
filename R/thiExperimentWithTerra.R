# Test new function to see if I can figure out how to do a thi calc, combining tmax and rh
# run the setup code in tmaxFruitCalcs to get everything set up

source("R/globallyUsed.R")

locOfFiles <- locOfCMIP6tifFiles

sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
modelChoices <- c(  "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startYearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices_historical <- c(1991)
northernHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-c( -180, 180, -90, 0)

yearRange <- 19
woptList <- list(gdal=c("COMPRESS=LZW"))

i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2041
yearNumber <- 2043

# the best function to use, in combination with lapp

THIfun4 <- function(rh, tmax) {
  (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))
}

for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange + 1) # the +1 here is to get at the original file names
      modelName.lower <- tolower(i)
      modelName.lower <- tolower(i)
      yearSpan <- paste0(l, "_", l + yearRange)
      fileName_tasmax <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      tmax <- rast(fileName_tasmax)
      fileName_rh <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      rh <- rast(fileName_rh)
      
      for (yearNumber in l:(l + yearRange)) {
        gc()
        
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        
        print(system.time(tmax_yr <- subset(tmax, indicesCharYear)))
#        names(tmax_yr) <- indicesCharYear
        # system.time(tmax_NH <- crop(tmax, ext(northernHemExtent)))
        # system.time(tmax_yr_NH <- subset(tmax_NH, indicesCharYear))
        
        print(system.time(rh_yr <- subset(rh, indicesCharYear)))
  #      names(rh_yr) <- indicesCharYear
        # system.time(rh_NH <- crop(rh, ext(northernHemExtent)))
        # system.time(rh_yr_NH <- subset(rh_NH, indicesCharYear))
        
        comb <- sds(rh_yr, tmax_yr)
        fileName_out <- paste0("data/cmip6/THI/thi.cattle_",  i, "_", k, "_", yearNumber, ".tif")
        
        print(system.time(rrr <- lapp(comb, THIfun4, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
      }
    }
  }
}


funExtremeCattleTHIct <- function(cellVector) {
  heatCt <- c(NA) 
  if (is.nan(cellVector[1])) {
    return(heatCt)
  }
  heatCt <- sum(cellVector > 86)
  return(heatCt) 
}

yearNumber <- 2041
fileName_in <- paste0("data/cmip6/THI/thi.cattle_",  i, "_", k, "_", yearNumber, ".tif")
thi.cattle <- rast(fileName_in)

print(system.time(extremeCt_cattle <- app(thi.cattle, funExtremeCattleTHIct))) 


















# hold for potential future use
# simplest function
combined <- c(rh_yr_NH, tmax_yr_NH)
nl <- nlyr(combined)
vlend <- nl/2
v2start <- 1 + vlend

# either use algebra
system.time(rr <- THIfun4(rh = rh_yr_NH, tmax = tmax_yr_NH))
rr
rr <- NULL

# or lapp (so that you can provide a filename and such)
comb <- sds(rh_yr_NH, tmax_yr_NH)
system.time(rrr <- lapp(comb, THIfun4))

funTHI <- function(cellVector) {
  # nl <- length(cellVector)
  # vlend <- nl/2
  if (is.nan(cellVector[1])) { return(rep(NA, vlend)) }
  # v2start <- 1 + vlend
  rh <- cellVector[1:vlend]
  #  print(paste("length rh: ", (length(rh))))
  tmax <- cellVector[v2start:nl]
  #  print(paste("length tmax: ", (length(tmax))))
  (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) 
}

system.time(test <- app(combined, funTHI))
test
test <- NULL

# and more to my taste (the mini version). Assuming NAs will take care of themselves.
funTHI3 <- function(x) {
  tmax <- x[v2start:nl]
  (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * x[1:v1end]) * (1.8 * tmax - 26.8))
}


system.time(test3 <- app(combined, funTHI3))
print(test3)
test3 <- NULL

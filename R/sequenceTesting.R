# identify runs of common values such as where temp is greater than some value
library(terra)
# starting point is approaches in https://stackoverflow.com/questions/64091829/r-how-to-find-a-series-of-common-values-in-a-vector-identifying-growing-season
sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
startyearChoices <-  c(2041, 2081) 
terraOptions(memfrac = 2, progress = 0, tempdir =  "data/ISIMIP", verbose = FALSE)
locOfFiles <- "data/bigFiles/"

sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") 
startyearChoices <-  c(2041, 2081) 
#startyearChoices <-  c(2081) 
startyearChoices_historical <- c(1991)
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))
options(warn=0) # convert warnings to errors

yearRange <- 19
# this function writes the number of runs that meet both the logic and length test
# runTarget <- "x > 0 & x < 9"
test_logic <- "x < 0 "
test_length <- 5

runsfun <- function(x) {
  runResult <- c(NA, NA) 
  if (is.nan(x[1])) {
    return(runResult)
  }
  seqLengthCode <- paste0("1{", test_length, ",}") #A regular expression  to get the first item of gregexpr. It says look for  test_length times See http://xenon.stanford.edu/~xusch/regexp/
  g <- gregexpr(seqLengthCode, paste(+eval(parse(text = test_logic)), collapse = ""))[[1]] # The + converts TRUE and FALSE to 1 and 0
  if (!(g[1] == -1)) { # no need to write to growing season if g returns -1, return 0,0
    startDays <- unlist(g)
    runLengths <- sum(as.numeric(attributes(g)$match.length))
    runResult <- c(length(startDays), runLengths)
  } else {
    runResult <- c(0, 0) 
  }
  return(runResult)
}

# test cattle thi for runs
test_logic <- "x > 88"
test_length <- 10

for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_in <- paste0("data/cmip6/THI/ensemble_thi.cattle_", k, "_", yearSpan, ".tif")
    r <- rast(fileName_in)
    print(system.time(r_runs <- app(r, runsfun)))
    fileName_out <- paste0("data/cmip6/THI/run_10_lim_gt88_ensemble_thi.cattle_", k, "_", yearSpan, ".tif")
    print(system.time(writeRaster(r_runs, filename = fileName_out,  overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
    
  }
}
    
plot(temp$lyr.1, main = "cattle THI runs, minimum length of 10 days, thi > 88, SSP585, end century")
plot(temp$lyr.2, main = "cattle THI, days in runs, minimum length of 10 days, thi > 88, SSP585, end century")

# number of days where tmax > 35C

test_logic <- "x > 35"
test_length <- 10

for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_in <- paste0("data/bigFiles/ensemble_dyMean20yr_", k, "_tasmax_global_daily_", yearSpan, ".tif")
    r <- rast(fileName_in)
    print(system.time(r_runs <- app(r, runsfun)))
    fileName_out <- paste0("data/cmip6/tmaxRuns/run_10_lim_gt35_ensemble_tmax_", k, "_", yearSpan, ".tif")
    print(system.time(writeRaster(r_runs, filename = fileName_out,  overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
    
  }
}
yearSpan_end_century <- "2081_2100"
yearSpan_mid_century <- "2041_2060"
temp_ssp585_end <- rast(paste0("data/cmip6/tmaxRuns/run_10_lim_gt35_ensemble_tmax_", k, "_", yearSpan_end_century, ".tif"))
temp_ssp585_mid <- rast(paste0("data/cmip6/tmaxRuns/run_10_lim_gt35_ensemble_tmax_", k, "_", yearSpan_mid_century, ".tif"))
plot(temp_ssp585_end$lyr.1, main = "Number of tmax runs, minimum length of 10 days, tmax > 35\nSSP585, end century")
plot(temp_ssp585_end$lyr.2, main = "Tmax number of days in runs, minimum length of 10 days, tmax > 35\nSSP585, end century")
plot(temp_ssp585_mid$lyr.1, main = "Number of tmax runs, minimum length of 10 days, tmax > 35, SSP585\nmid century")
plot(temp_ssp585_mid$lyr.2, main = "Tmax number of days in runs, minimum length of 10 days, tmax > 35\nSSP585, mid century")

    
    # this process uses two items -  a run criterion using a logical expression, e.g., values greater than x, and a minimum length of the run, say 5. It applies these to a vector
    cellVector <-     c(1,3,4,2,1,-5,6,0,1, 3, 4,-2, 6, 7, 5, 8, 1)
    cellitemnumber <- c(1,2,3,4,5, 6,7,8,9,10,11,12,13,14,15,16,17)
    # this function captures the first run that meets the criterion and limit. It returns a vector
    fun <- function(cellVector) {
      startend <- c(NA, NA) 
      if (is.nan(cellVector[1])) {
        return(startend)
      }
      # gregexpr returns a list of the same length as text each element of which is of the same form as the return value for regexpr, except that the starting positions of every (disjoint) match are given.
      #gregexpr wants a regular expression as its first element
      g <- gregexpr(seqLengthCode, paste(+(cellVector > 0), collapse = ""))[1] # The + converts TRUE and FALSE to 1 and 0
      if (!g[[1]] == -1) { # no need to write to growing season if g returns 1
        startend[1] <- g[[1]] # the starting day of the sequence
        matchLength <- as.numeric(attributes(g)$match.length) # the number of days in the sequence of days that match at least the minimum length required
        startend[2] <- startend[1] + as.numeric(matchLength) - 1
      }
      return(startend) 
    }
    
    # now try it out with northern hemisphere spring
    runTest_fileName <- paste0("data/bigFiles/ensemble_dyMean20yr_ssp585_tasmin_global_daily_2041_2060.tif")
    runTest <- rast(runTest_fileName)
    #thi_run <- fun2(thiTest[75778])
    #thi_run <- fun2(thiTest[603])
    
    extent_NH <- c( -180, 180, 0, 90)
    springStart_NH <- 60 #March 1 in 2019
    springEnd_NH <- 120 #April 30 in 2019
    
    runTest <- subset(runTest, springStart_NH:springEnd_NH)
    runTest <- crop(runTest, extent_NH)
    system.time(runTestResults <- app(runTest, fun2))
    plot(runTestResults, 1, main = "number of spring 3 day runs of temps below 0\nssp585, mid century")
    plot(runTestResults, 2, main = " tot. number of spring days below 0 in 3 day runs of temps below 0\nssp585, mid century")
    
    minimumGrwSeasonLength = 4
    seqLengthCode <- paste0("1{", minimumGrwSeasonLength, ",}") # some regular expression language to get the first item of gregexpr. It says look for 1 minimumGrwSeasonLength times See http://xenon.stanford.edu/~xusch/regexp/
    
    getSeq2 <- function(x) {
      g <- gregexpr("1{5,}", paste(+(x > 0), collapse = ""))[[1]]
      vals <- function(i) {
        ix <- seq(g[i], length = attr(g, "match.length")[i])
        setNames(x[ix], ix)
      }
      if (length(g) == 1 && g == -1) list() else lapply(seq_along(g), vals)
    }
    
    library(data.table)
    
    getSeq <- function(x) {
      names(x) <- seq_along(x) # seq_along generates a sequence of integers that starts with 1 and increments by 1
      ok <- x > 0 # Ok is a logical vector of whether x meets the criterion (>0) or not
      # x[ok] returns a numeric vector of all the values that meet the criterion
      # rleid(ok)[ok] returns the start location of a sequence that meets the criterion
      s <- split(x = x[ok], f = rleid(ok)[ok]) #split divides the data in the vector x into the groups defined by f. The replacement forms replace values corresponding to such a division. unsplit reverses the effect of split.
      unname(s)[lengths(s) >= minimumGrwSeasonLength]
    }
    
    temp <- getSeq(cellVector)
    
    cutoff <- 40
    seqLength <- 10
    fun <- function(x) {
      if (is.nan(x[1])) {
        return(x)
      } else {
        names(x) <- seq_along(x) # seq_along generates a sequence of integers that starts with 1 and increments by 1
        ok <- x > cutoff # Ok is a logical vector of whether x meets the criterion (> cutoff) or not; ie, is ok
        # x[ok] returns a numeric vector of all the values that meet the criterion
        # rleid(ok)[ok] returns the start location of a sequence that meets the criterion
        s <- split(x = x[ok], f = rleid(ok)[ok]) #split divides the data in the vector x into the groups defined by f. The replacement forms replace values corresponding to such a division.
        temp <- unname(s)[lengths(s) >= seqLength]
        zeroVec <- rep(0, length(x))
        if (length(temp) == 0) {
          return(zeroVec) }
        if (length(temp) == 1) {
          position <- as.numeric(attributes(temp[[1]][1]))
          zeroVec[position] <- length(temp[[1]]) - 1
          # print(paste0("number of runs: ", length(temp)))
          # print(paste0("position: ", position))
          # print(paste0("length: ", length(temp[[1]]) - 1)) # the minus one means the length covers only the positions where the condition is true
          return(zeroVec) 
        } else {
          for (i in 1:length(temp)) { # this loop deals with multiple runs
            position <- as.numeric(attributes(temp[[i]])[[1]][1])
            zeroVec[position] <- length(temp[[i]][1] - 1)
            # print(paste0("number of runs: ", length(temp)))
            # print(paste0("position: ", position))
            # print(paste0("length: ", length(temp[[i]][1])))
          }
          return(zeroVec)
        }
      }
    }
    testtmin <- fun(tmin[75778])
    
    system.time(tmin_ctoff <- app(tmin, fun = fun))
    system.time(tmin_ctoff_sum <- app(tmin_ctoff, fun = sum))
    
    #test values
    i <- "IPSL-CM6A-LR"
    k <- "ssp585"
    l <- 2081
    modelName.lower <- tolower(i)
    yearRange <- 19
    yearSpan <- paste0(l, "_", l + yearRange)
    
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices_day <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices_year <- format(indices_day, format = "%Y") # %j is day of the year
    indices_day <- paste0("X", as.character(indices_day))
    indices_year <- as.numeric(indices_year)
    
    fileName_tasmax <- paste0("/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/ssp585/IPSL-CM6A-LR/", "ipsl-cm6a-lr_ssp585_tasmax_global_daily_2081_2100.tif")
    
    tmax <- rast(fileName_tasmax)
    
    system.time(tmax_ctoff <- app(tmax, fun = fun))
    names(tmax_ctoff) <- indices_day
    
    system.time(tmax_ctoff_sum <- app(tmax_ctoff, fun = sum))
    system.time(tmax_ctoff_sum_yr <- tapp(tmax_ctoff, indices_year, fun = sum))
    
    
    for (i in 1:length(thiTest)) {
      print(paste("i: ", head(thiTest[i])))
    }
    
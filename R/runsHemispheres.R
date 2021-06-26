{
  require(terra)
  source("R/ISIMIPconstants.R")
  source("R/ISIMIPspatialConstants.R")
  
  seasons <- c("winter", "summer")
  startYearChoices <-  c(2041, 2081) 
  
  #test files
  i <- "IPSL-CM6A-LR"
  modelChoice_lower <- tolower(i)
  k <- "ssp585"
  l <- 2041
  yearSpan <- paste0(l, "_", l + yearRange)
  fileName_tasmin <- paste0(locOfClimFiles,  modelChoice_lower, "_tasmin", "_", k,  "_", yearSpan, ".tif")
  rastIn <- rast(fileName_tasmin)
  
  print(system.time(tmin_SH <- crop(rastIn, extent_SH)))
  
  # indices_mean_day <- format(as.Date(names(rastIn), format = "X%Y-%m-%d"), format = "%j")
  # indices_mean_month <- as.numeric(indices_mean_month)
  # indices_mean_day <- as.numeric(indices_mean_day)
  
  # startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
  # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  # indices_day <- format(indices, format = "%j")
  
  layerNums <- 1:length(names(tmin_SH))
  
  startDate <- paste0(l, "-07-01"); endDate <- paste0(l + yearRange, "-06-30")
  indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  indices_day_l <-lubridate::yday(indices - lubridate::yday(endDate) + 1)
  
  # perennials hemisphere specific runs calculations -----
  {
    climVal <- 0
    test_logic <- paste0("x > ", climVal)
    k <- "ssp585"
    l = "2041"
    logicDirection <- ">"
    if (logicDirection == ">") ldtext <-"gt"
    if (logicDirection == "<") ldtext <-"lt"
  }
  f_runs <- function(x) {
    #    print(paste0(" x: ", x[1]))
    runResult <- c(NA, NA) 
    if (is.nan(as.numeric(x[1]))) {
      return(runResult)
    }
    seqLengthCode <- paste0("1{", runlength, ",}") #A regular expression  to get the first item of gregexpr. It says look for  run_length times See http://xenon.stanford.edu/~xusch/regexp/
    g <- gregexpr(seqLengthCode, paste(+eval(parse(text = logicString)), collapse = ""))[[1]] # The + converts TRUE and FALSE to 1 and 0
    #  print(paste0("g1: ", g[1]))
    if ((g[1] == -1)) { # no need to write to growing season if g returns -1, return 0,0
      runResult <- c(0, 0) 
      #    print("no runs")
    } else {
      startDays <- unlist(g)
      runLengths <- sum(as.numeric(attributes(g)$match.length))
      runResult <- c(length(startDays), runLengths)
      #     print(paste0("runResult: ", runResult))
    }
    return(runResult)
  }
  
  f_noFreeze <- function(cellVector, seqLengthCode) {
    startend <- c(NA, NA) 
    # if (is.nan(cellVector[1])) {
    #   return(startend)
    # }
    g <- gregexpr(seqLengthCode, paste(+(cellVector > 0), collapse = ""))[[1]]
    if (!g[[1]] == -1) { # no need to write to growing season if g returns 1
      startend[1] <- g[[1]]
      matchLength <- attributes(g)$match.length
      startend[2] <- startend[1] + matchLength - 1
      print(g)
      #    print(paste0("startend[1]: ", startend[1], " matchLength: ", matchLength ))
    }
    return(startend) 
  }
}

runlengthChoices <- c(100) # at least 100 days of tmin > 0
varName <- "tasmin"
for (runlength in runlengthChoices) {
  for (k in sspChoices) {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      for (modelChoice in modelChoices) {
        modelChoice_lower <- tolower(modelChoice)
        logicString <- paste0("x ", logicDirection, " ", climVal)
        fileName_in <- paste0(locOfClimFiles, modelChoice_lower, "_", varName, "_", k, "_", yearSpan, ".tif")
        r <- rast(fileName_in)
        for (hem in hemispheres) {
          yrs <- l + yearRange # for NH
          if (hem == "SH") yrs <- l + yearRange - 1
        for (yearNumber in l:(l + yrs)) {
          if (hem == "SH")  {startDate <-  paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber + 1, "-06-30")} # in southern hemisphere search July 1 to June 30 of the next year.
          if (hem == "NH")  {startDate <-  paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber + 1, "-12-31")}
          
          indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
          indicesChar <- paste0("X", indices)
          r_yr <- subset(r, indicesChar)
          r_yr <- crop(r_yr, get(paste0("extent_", hem)))
          print(system.time(r_runs <- app(r_yr, f_runs)))
          if (yearNumber == l ) {
            runs_length <- subset(r_yr, 2)
            runs_ct <- subset(r_yr, 1)
          } else {
            runs_length <- c(runs_length, subset(r_yr, 2))
            runs_ct <- c(runs_ct, subset(r_yr, 1))
          }
        }
          fileName_ct_out <- paste0("data/cmip6/runs/", "runs_ct_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          fileName_length_out <- paste0("data/cmip6/runs/", "runs_length_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          print(system.time(writeRaster(runs_length, filename = fileName_length_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(system.time(writeRaster(runs_ct, filename = fileName_ct_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(paste0("fileName_ct_out: ", fileName_ct_out))
          print(paste0("fileName_length_out: ", fileName_length_out))
        }
          
        plot(r_runs$lyr.1, main = mainct)
        plot(r_runs$lyr.2, main = mainrl)
      }
    }
  }
}

test <- subset(r, 1:365)
print(system.time(test_runs <- app(test, "f_runs", filename = fileName_out,  overwrite = TRUE, wopt= woptList)))

testOut <- f_runs(test[27360])

# find cell with values
for (cntr in 1:(360*720)) print(paste0(cntr, " ", test[cntr][1]))

# ---------------
runlength <- 100
seqLengthCode <- paste0("1{", runlength, ",}") #A regular expression  to get the first item of gregexpr. It says look for  run_length times See http://xenon.stanford.edu/~xusch/regexp/

#no freeze run(s), scenarios -----
for (k in sspChoices) {
  for (modelChoice in modelChoices) {
    for (l in startYearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelChoice_lower <- tolower(modelChoice)
      fileName_in_tasmin <- paste0(locOfClimFiles, modelChoice_lower, "_tasmin_", k, "_", yearSpan, ".tif")
      tmin <- rast(fileName_in_tasmin)
      system.time(tmin_NH <- crop(tmin, extent_NH))
      system.time(tmin_SH <- crop(tmin, extent_SH))
      
      for (hem in hemispheres) {
        for (yearNumber in l:(l + yearRange)) {
          startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
          # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
          # indicesChar <- paste0("X", as.character(indices))
          indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
          indicesCharYear <- paste0("X", as.character(indicesYear))
          system.time(tmin_yr <- terra::subset(get(paste0("tmin_", hem)), indicesCharYear))
          # then use the function like this
          print(system.time(growingSeason <- app(get(paste0("tmin_", hem)), f_noFreeze, seqLengthCode)))
          names(growingSeason) <- c("growingSeason_start", "growingSeason_end")
          
          fileName_out <- paste0("data/cmip6/growingSeasons/growingSeason", hem, "_runlen_", runlength,  i, "_", k, "_", yearNumber, ".tif")
          writeRaster(growingSeasonNH, fileName_out, overwrite = TRUE, wopt = woptList)
        }
      }
    }
  }
}

tmin_SH_winter <- f_hemisphere(r = tmin_SH, hemisphere = "SH", season = "summer", startYear = 2041, yearSpan = 19)


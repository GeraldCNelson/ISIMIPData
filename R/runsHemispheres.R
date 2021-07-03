{
  require(terra)
  source("R/ISIMIPconstants.R")
  source("R/ISIMIPspatialConstants.R")
  source("R/perennialsPrep.R") # creates the data tables majorCropValues_main, majorCropValues_lo and majorCropValues_hi
  
  startYearChoices <-  c(2041, 2081) 
  options(warn = 1) # 2 converts warnings to errors
  locOfRunsFiles <- "data/cmip6/runs/"
  #test files
  i <- "IPSL-CM6A-LR"
  modelChoice_lower <- tolower(i)
  k <- "ssp585"
  l <- 2041
  yearSpan <- paste0(l, "_", l + yearRange)
  fileName_tasmin <- paste0(locOfClimFiles,  modelChoice_lower, "_tasmin", "_", k,  "_", yearSpan, ".tif")
  speciesChoice <- "apple_main"
  rastIn <- rast(fileName_tasmin)
  
  
  f_runs <- function(x, runlength) {
    # number of layers to be returned 
    # element 1 - number of runs that meet the logic criterion
    # element 2 - the total length of all runs that meet the run criterion
    # element 3 - first start day number
    # element 4 - first end day number
    runResult <- c(NA, NA, NA, NA) 
    if (is.nan(as.numeric(x[1]))) {
      return(runResult)
    }
    seqLengthCode <- paste0("1{", runlength, ",}") 
    # A regular expression  to get the first item of gregexpr. It says look for  run_length times See http://xenon.stanford.edu/~xusch/regexp/
    # seqLengthCode - what's the minimum lenthg of a run that meets the logic entry
    # logicString - what condition is evaluated ; e.g., x > 0
    # g[[1]] - if positive, start position of run; if there are two entries, there are 2 runs that satisfy the condition and the values are the starting position of each run
    #attributes(g)$match.length - if positive, length of elements in run
    g <- gregexpr(seqLengthCode, paste(+eval(parse(text = logicString)), collapse = ""))[[1]] # The + converts TRUE and FALSE to 1 and 0
    if ((g[1] == -1)) { # no need to write to growing season if g returns -1, return 0s
      runResult <- c(0, 0, 0, 0)
      return(runResult) 
    }
    startDays <- unlist(g) # start day(s) of the run(s)
    runlengths <- as.numeric(attributes(g)$match.length)
    startDay_r1 <- startDays[1]
    endDay_r1 <- startDay_r1 + runlengths[1]
    runLengths <- sum(runlengths)
    runResult <- c(length(startDays), runLengths, startDay_r1, endDay_r1)
    return(runResult)
  }
  
  
  f_runsSetup <- function(k, l, logicDirection, climVal) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (runlength in runlengthChoices) {
      for (modelChoice in modelChoices) {
        modelChoice_lower <- tolower(modelChoice)
        logicString <- paste0("x ", logicDirection, " ", climVal)
        fileName_in <- paste0(locOfClimFiles, modelChoice_lower, "_", climateVariable, "_", k, "_", yearSpan, ".tif")
        r <- rast(fileName_in)
        for (hem in hemispheres) {
          endYear <- l + yearRange # for NH
          if (hem == "SH") endYear <- l + yearRange - 1
          for (yearNumber in l:endYear) {
            print(paste0("working on ssp: ", k, ", start year: ", l, ", model choice: ", modelChoice, ", hemisphere: ", hem, ", year: ", yearNumber))
            if (hem == "SH")  {startDate <-  paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber + 1, "-06-30")} # in southern hemisphere search July 1 to June 30 of the next year.
            if (hem == "NH")  {startDate <-  paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")}
            indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
            indicesChar <- paste0("X", indices)
            r_yr <- subset(r, indicesChar)
            r_yr <- crop(r_yr, get(paste0("extent_", hem)))
            print(system.time(r_runs <- app(r_yr, f_runs, runlength)))
            if (yearNumber == l ) {
              print(paste0("yearNumber: ", yearNumber))
              runs_ct <- subset(r_runs, 1)
              runs_length <- subset(r_runs, 2)
              startday_1 <- subset(r_runs, 3)
              endday_1 <- subset(r_runs, 4)
              
            } else {
              runs_ct <- c(runs_ct, subset(r_runs, 1))
              runs_length <- c(runs_length, subset(r_runs, 2))
              startday_1 <- c(startday_1, subset(r_runs, 3))
              endday_1 <- c(endday_1, subset(r_runs, 4))
            }
          }
          
          print("runs_ct")
          print(runs_ct)
          print("runs_length")
          print(runs_length)
          print("startday_1")
          print(startday_1)
          print("endday_1")
          print(endday_1)
          
          fileName_ct_out <- paste0(locOfRunsFiles, "runs_ct_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          fileName_length_out <- paste0(locOfRunsFiles, "runs_length_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          fileName_startDay1_out <- paste0(locOfRunsFiles, "startday_1_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          fileName_endDay1_out <- paste0(locOfRunsFiles, "endday_1_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          
          print(system.time(writeRaster(runs_ct, filename = fileName_ct_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(system.time(writeRaster(runs_length, filename = fileName_length_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(system.time(writeRaster(startday_1, filename = fileName_startDay1_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(system.time(writeRaster(endday_1, filename = fileName_endDay1_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(paste0("fileName_ct_out: ", fileName_ct_out))
          print(paste0("fileName_length_out: ", fileName_length_out))
          print(paste0("fileName_startDay1_out: ", fileName_startDay1_out))
          print(paste0("fileName_endDay1_out: ", fileName_endDay1_out))
        }
        mainstart <- paste0("Start day of at least 100 consecutive days of where temp is greater than 0°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
        mainend <- paste0("End day of at least 100 consecutive days of at least 100 where temp is greater than 0°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
        plot(startday_1$lyr.3, main = mainstart)
        plot(endday_1$lyr.4, main = mainend)
      }
    }
  }
  # perennials hemisphere specific runs calculations -----
  {
    climVal <- 0 
    test_logic <- paste0("x > ", climVal)
    logicDirection <- ">"
    if (logicDirection == ">") ldtext <-"gt"
    if (logicDirection == "<") ldtext <-"lt"
    runlengthChoices <- c(100) # at least 100 days of tmin > 0
    climateVariable <- "tasmin"
  }
}


# runs, scenarios -----
for (runlength in runlengthChoices) {
  for (k in sspChoices) {
    for (l in startYearChoices) {
      f_runsSetup(k, l, logicDirection, climVal)
    }
  }
}

# runs, historical -----
k = "historical"
l = 1991
f_runsSetup(k, l, logicDirection, climVal)

#  GDDs sum start  -----
{
  runlength = 100 # minimum number of days above climVal
  ldtext <- "gt" # logic direction - gt, ge, ld, le
  climVal <- 0 # temperature threshold
  climateVariable <- "tasmin" # variable logic is applied to
  varietiesChoice <- "varieties_main"
  varietiesChoiceSuffix <- gsub("varieties", "", varietiesChoice) # whether to use lo, main, or hi cp values
  cropVals <- eval(parse(text = (paste0(" majorCropValues", varietiesChoiceSuffix))))
  speciesChoices <- unique(cropVals$cropName)
}

f_gddSums <- function(k, l) {
  yearSpan <- paste0(l, "_", l + yearRange)
  for (modelChoice in modelChoices) {
    modelChoice_lower <- tolower(modelChoice)
    for (hem in hemispheres) {
      fileName_gdd_in <- paste0(locOfgddsFiles, modelChoice_lower, "_", hem, "_", "gdd", "_", speciesChoice, "_", k, "_", yearSpan, ".tif")
      gdds <- rast(fileName_gdd_in)
      gdds <- crop(gdds, get(paste0("extent_", hem)))
      if (hem == "SH")  {startDate <-  paste0(l, "-07-01"); endDate <- paste0(l + yearRange, "-06-30")} # in southern hemisphere search July 1 to June 30 of the next year. NH is just the calendar year
      if (hem == "NH")  {startDate <-  paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")} # in southern hemisphere search July 1 to June 30 of the next year. NH is just the calendar year
      indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
      indicesChar <- paste0("X", indices)
      indicesYr <- unique(as.numeric(format(indices, "%Y")))
      if (hem == "SH") indicesYr <- indicesYr[1:yearRange]
      if (length(gdds) == length(indicesChar)) gdds <- subset(gdds, indicesChar) # if SH, gets rid of the first 1/2 year and  last 1/2 year. may not be necessary because I think gdds in sh file already have this done. If statement may capture this
      fileName_startDay1_in <- paste0(locOfRunsFiles, "startday_1_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
      fileName_endDay1_in <- paste0(locOfRunsFiles, "endday_1_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
      startDay <- rast(fileName_startDay1_in)
      names(startDay) <- sort(unique(indicesYr))
      endDay <- rast(fileName_endDay1_in)
      names(startDay) <-names(endDay) <- sort(unique(indicesYr))
      
      # now do calc by year
      for (yearNumber in 1:nlyr(startDay)) {
        print(paste0("Working on ssp:" , k, ", startYear: ", l, ", hem: ", hem, ", model: ", modelChoice, ", yearNumber: ", yearNumber))
        startDay_yr <- subset(startDay, yearNumber)
        endDay_yr <- subset(endDay, yearNumber)
        startYear <- l + yearNumber - 1
        
        if (hem == "SH")  {
          startDate <-  paste0(startYear, "-07-01"); endDate <- paste0(startYear + 1, "-06-30")} # in southern hemisphere search July 1 to June 30 of the next year. NH is just the calendar year
        if (hem == "NH")  {
          startDate <-  paste0(startYear, "-01-01"); endDate <- paste0(startYear, "-12-31")
        } # in southern hemisphere search July 1 to June 30 of the next year. NH is just the calendar year
        indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
        indicesChar <- paste0("X", indices)
        #            print(system.time(sum_gdds <- app(gdds_yr, f_sumVec, startDay_yr, endDay_yr)))
        if ((hem == "SH" & yearNumber < 20) | (hem == "NH")) {
          gdds_yr <- subset(gdds, indicesChar)
          print(paste0("Just before browser, iteration number: ",  yearNumber))
          #              browser()
          print(system.time(sum_gdds <- rapp(gdds_yr, startDay_yr, endDay_yr, sum)))
          if (yearNumber == 1 ) {
            period_sums <- sum_gdds
          } else {
            period_sums <- c(period_sums, sum_gdds)
          }
          fileName_gddSums_out <- paste0(locOfgddsFiles, "gddSum", "_", modelChoice_lower, "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
          print(system.time(writeRaster(period_sums, filename = fileName_gddSums_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(paste0("fileName_gddSums_out: ", fileName_gddSums_out))
          gc()
        }
      }
    }
  }
}

# gdd sums, scenarios ------
for (speciesChoice in speciesChoices) {
  for (k in sspChoices) {
    for (l in startYearChoices) {
      f_gddSums(k, l)
    }
  }
}

# gdd sums, historical ------
k <- "historical"
l <- 1991
for (speciesChoice in speciesChoices) {
  f_gddSums(k, l)
}





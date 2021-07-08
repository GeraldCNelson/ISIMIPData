{
  require(terra)
  source("R/ISIMIPconstants.R")
  source("R/ISIMIPspatialConstants.R")
  
  options(warn = 1) # 2 converts warnings to errors
  #test values
  i <- "IPSL-CM6A-LR"
  modelChoice_lower <- tolower(i)
  k <- "ssp585"
  l <- 2041
  yearSpan <- paste0(l, "_", l + yearRange)
  fileName_tasmin <- paste0(locOfClimFiles,  modelChoice_lower, "_tasmin", "_", k,  "_", yearSpan, ".tif")
  rastIn <- rast(fileName_tasmin)
  
  # variables that define the runs parameters. This is the only place they are assigned values
  {
    climVal <- -2 # changed from 0 because subject experts say plants can tolerate this
    test_logic <- paste0("x > ", climVal)
    logicDirection <- ">"
    if (logicDirection == ">") ldtext <-"gt"
    if (logicDirection == "<") ldtext <-"lt"
    runlengthChoices <- c(100) # at least 100 days of tmin > 0
    climateVariable <- "tasmin"
    runsParms <- c(climVal, test_logic, logicDirection, ldtext, runlengthChoices, climateVariable)
  }
  
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
  
  f_runsSetup <- function(k, l, runsParms) {
    logicDirection <- runsParms[3]
    climVal <- runsParms[1]
    climateVariable <- runsParms[6]
    ldtext <- runsParms[4]
    yearSpan <- paste0(l, "_", l + yearRange)
    for (runlength in runsParms[5]) {
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
        mainstart <- paste0("Start day of at least 100 consecutive days of where temp is greater than ", climVal, "°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
        mainend <- paste0("End day of at least 100 consecutive days of at least 100 where temp is greater than ", climVal, "°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
        plot(startday_1$lyr.3, main = mainstart)
        plot(endday_1$lyr.4, main = mainend)
      }
    }
  }
  f_readRast_runs <- function(modelChoice, k, l, hem, runType, runsParms) {
    logicDirection <- runsParms[3]
    climVal <- runsParms[1]
    climateVariable <- runsParms[6]
    ldtext <- runsParms[4]
    yearSpan <- paste0(l, "_", l + yearRange)
    modelChoice_lower <- tolower(modelChoice)
    fileName_in = paste0(locOfRunsFiles, runType, "_", climateVariable, "_", modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
    print(paste0("runType: ", runType, ", k: ", k, ", modelChoice: ", modelChoice, ", fileName in: ", fileName_in))
    r <- rast(fileName_in)
  }
}

# perennials hemisphere specific runs calculations -----
# runs, scenarios -----
for (runlength in runsParms[5]) {
  for (k in sspChoices) {
    for (l in startYearChoices) {
      f_runsSetup(k, l, runsParms)
    }
  }
}

# runs, historical -----
k = "historical"
l = 1991
f_runsSetup(k, l, runsParms)

# runs , means by model, historical -----
logicDirection <- runsParms[3]
climVal <- runsParms[1]
climateVariable <- runsParms[6]
ldtext <- runsParms[4]

k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
# startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
# indices <- seq(as.Date(startDate), as.Date(endDate), 1)
# indices_day <- as.numeric(format(indices, format = "%j"))
for (modelChoice in modelChoices) {
  modelChoice_lower <- tolower(modelChoice)
  for (hem in hemispheres) {
    fileName_ct_in <- paste0(locOfRunsFiles, "runs_ct_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
    fileName_length_in <- paste0(locOfRunsFiles, "runs_length_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
    fileName_startDay1_in <- paste0(locOfRunsFiles, "startday_1_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
    fileName_endDay1_in <- paste0(locOfRunsFiles, "endday_1_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
    fileName_ct_out <- paste0(locOfRunsFiles, "runs_ct_mean_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
    fileName_length_out <- paste0(locOfRunsFiles, "runs_length_mean_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
    fileName_startDay1_out <- paste0(locOfRunsFiles, "startday_1_mean_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
    fileName_endDay1_out <- paste0(locOfRunsFiles, "endday_1_mean_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
    ct_in <- rast(fileName_ct_in)
    length_in <- rast(fileName_length_in)
    startDay1_in <- rast(fileName_startDay1_in)
    endDay1_in <- rast(fileName_endDay1_in)
    test_cp <- app(ct_in, mean, filename = fileName_ct_out, overwrite = TRUE, wopt = woptList)
    test_length <- app(length_in, mean, filename = fileName_length_out, overwrite = TRUE, wopt = woptList)
    test_startDay <- app(startDay1_in, mean, filename = fileName_startDay1_out, overwrite = TRUE, wopt = woptList)
    test_endDay <- app(endDay1_in, mean, filename = fileName_endDay1_out, overwrite = TRUE, wopt = woptList)
  }
}

# runs , means by model, scenarios -----
logicDirection <- runsParms[3]
climVal <- runsParms[1]
climateVariable <- runsParms[6]
ldtext <- runsParms[4]
for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    # startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    # indices_day <- as.numeric(format(indices, format = "%j"))
    for (modelChoice in modelChoices) {
      modelChoice_lower <- tolower(modelChoice)
      for (hem in hemispheres) {
        fileName_ct_in <- paste0(locOfRunsFiles, "runs_ct_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
        fileName_length_in <- paste0(locOfRunsFiles, "runs_length_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
        fileName_startDay1_in <- paste0(locOfRunsFiles, "startday_1_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
        fileName_endDay1_in <- paste0(locOfRunsFiles, "endday_1_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
        fileName_ct_out <- paste0(locOfRunsFiles, "runs_ct_mean_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
        fileName_length_out <- paste0(locOfRunsFiles, "runs_length_mean_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
        fileName_startDay1_out <- paste0(locOfRunsFiles, "startday_1_mean_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
        fileName_endDay1_out <- paste0(locOfRunsFiles, "endday_1_mean_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
        ct_in <- rast(fileName_ct_in)
        length_in <- rast(fileName_length_in)
        startDay1_in <- rast(fileName_startDay1_in)
        endDay1_in <- rast(fileName_endDay1_in)
        test_cp <- app(ct_in, mean, filename = fileName_ct_out, overwrite = TRUE, wopt = woptList)
        test_length <- app(length_in, mean, filename = fileName_length_out, overwrite = TRUE, wopt = woptList)
        test_startDay <- app(startDay1_in, mean, filename = fileName_startDay1_out, overwrite = TRUE, wopt = woptList)
        test_endDay <- app(endDay1_in, mean, filename = fileName_endDay1_out, overwrite = TRUE, wopt = woptList)
      }
    }
  }
}


# ensemble mean, runs -----

# ensemble mean runs, historical -----
logicDirection <- runsParms[3]
climVal <- runsParms[1]
climateVariable <- runsParms[6]
ldtext <- runsParms[4]
runlength <- runsParms[5]
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
runType <- "runs_length" # choices are c("runs_ct", "runs_length", "startday_1", "endday_1")
for (hem in hemispheres) {
  x <- lapply(modelChoices, f_readRast_runs, k, l, hem, runType, runsParms)
  r <- rast(x)
  indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
  fileName_out <- paste0(locOfRunsFiles, "ensemble_", runType, "_", climateVariable, "_", modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
  cat(paste0("ensemble ssp: ", k, ", start year: ", l , ", fileName out: ", fileName_out))
  print(system.time(r_mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
}

# ensemble mean runs, scenarios -----
logicDirection <- runsParms[3]
climVal <- runsParms[1]
climateVariable <- runsParms[6]
ldtext <- runsParms[4]
runlength <- runsParms[5]
for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    runType <- "runs_length" # choices are c("runs_ct", "runs_length", "startday_1", "endday_1")
    for (hem in hemispheres) {
      x <- lapply(modelChoices, f_readRast_runs, k, l, hem, runType, runsParms)
      r <- rast(x)
      indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
      fileName_out <- paste0(locOfRunsFiles, "ensemble_", runType, "_", climateVariable, "_", modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
      cat(paste0("ensemble ssp: ", k, ", start year: ", l , ", fileName out: ", fileName_out))
      print(system.time(r_mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
    }
  }
}










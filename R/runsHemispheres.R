{
  require(terra)
  source("R/ISIMIPconstants.R")
  source("R/ISIMIPspatialConstants.R")
  startYearChoices <-  c(2041, 2081) 
  options(warn = 2) # 2 converts warnings to errors
  
  #test files
  i <- "IPSL-CM6A-LR"
  modelChoice_lower <- tolower(i)
  k <- "ssp585"
  l <- 2041
  yearSpan <- paste0(l, "_", l + yearRange)
  fileName_tasmin <- paste0(locOfClimFiles,  modelChoice_lower, "_tasmin", "_", k,  "_", yearSpan, ".tif")
  rastIn <- rast(fileName_tasmin)
  
  
  f_runs <- function(x) {
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
      #    print("no runs")
    } else {
      startDays <- unlist(g) # start day(s) of the run(s)
      runlengths <- as.numeric(attributes(g)$match.length)
      startDay_r1 <- startDays[1]
      endDay_r1 <- startDay_r1 + runlengths[1]
      runLengths <- sum(runlengths)
      runResult <- c(length(startDays), runLengths, startDay_r1, endDay_r1)
    }
    return(runResult)
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
  varName <- "tasmin"
}

# runs, scenarios -----
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
          for (yearNumber in l:yrs) {
            if (hem == "SH")  {startDate <-  paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber + 1, "-06-30")} # in southern hemisphere search July 1 to June 30 of the next year.
            if (hem == "NH")  {startDate <-  paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber + 1, "-12-31")}
            indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
            indicesChar <- paste0("X", indices)
            r_yr <- subset(r, indicesChar)
            r_yr <- crop(r_yr, get(paste0("extent_", hem)))
            print(system.time(r_runs <- app(r_yr, f_runs)))
            if (yearNumber == l ) {
              runs_ct <- subset(r_runs, 1)
              runs_length <- subset(r_runs, 2)
            } else {
              runs_ct <- c(runs_ct, subset(r_runs, 1))
              runs_length <- c(runs_length, subset(r_runs, 2))
            }
          }
          fileName_ct_out <- paste0("data/cmip6/runs/", "runs_ct_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          fileName_length_out <- paste0("data/cmip6/runs/", "runs_length_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          fileName_startDay1_out <- paste0("data/cmip6/runs/", "startday_1_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          fileName_endDay1_out <- paste0("data/cmip6/runs/", "endday_1_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          print(system.time(writeRaster(runs_ct, filename = fileName_ct_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(system.time(writeRaster(runs_length, filename = fileName_length_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(system.time(writeRaster(startday_1, filename = fileName_startDay1_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(system.time(writeRaster(endday_1, filename = fileName_endDay1_out,  overwrite = TRUE, wopt= woptList))); flush.console()
          print(paste0("fileName_ct_out: ", fileName_ct_out))
          print(paste0("fileName_startDay1_out: ", fileName_startDay1_out))
          print(paste0("fileName_endDay1_out: ", fileName_endDay1_out))
        }
        mainct <- paste0("Count of consecutive days of at least 100 where temp is greater than 0°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
        mainrl <- paste0("Locations with at least 100 consecutive days where temp is greater than 0°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
        plot(r_runs$lyr.1, main = mainct)
        plot(r_runs$lyr.2, main = mainrl)
      }
    }
  }
}

# runs, historical -----
k = "historical"
l = 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (runlength in runlengthChoices) {
  for (modelChoice in modelChoices) {
    modelChoice_lower <- tolower(modelChoice)
    logicString <- paste0("x ", logicDirection, " ", climVal)
    fileName_in <- paste0(locOfClimFiles, modelChoice_lower, "_", varName, "_", k, "_", yearSpan, ".tif")
    r <- rast(fileName_in)
    for (hem in hemispheres) {
      yrs <- l + yearRange # for NH
      if (hem == "SH") yrs <- l + yearRange - 1
      for (yearNumber in l:yrs) {
        print(paste0("working on ssp: ", k, ", start year: ", l, ", model choice: ", modelChoice, ", hemisphere: ", hem, ", year: ", yearNumber))
        if (hem == "SH")  {startDate <-  paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber + 1, "-06-30")} # in southern hemisphere search July 1 to June 30 of the next year.
        if (hem == "NH")  {startDate <-  paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber + 1, "-12-31")}
        indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
        indicesChar <- paste0("X", indices)
        r_yr <- subset(r, indicesChar)
        r_yr <- crop(r_yr, get(paste0("extent_", hem)))
        print(system.time(r_runs <- app(r_yr, f_runs)))
        if (yearNumber == l ) {
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
      fileName_ct_out <- paste0("data/cmip6/runs/", "runs_ct_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
      fileName_length_out <- paste0("data/cmip6/runs/", "runs_length_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
      fileName_startDay1_out <- paste0("data/cmip6/runs/", "startday_1_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
      fileName_endDay1_out <- paste0("data/cmip6/runs/", "endday_1_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
      print(system.time(writeRaster(runs_ct, filename = fileName_ct_out,  overwrite = TRUE, wopt= woptList))); flush.console()
      print(system.time(writeRaster(runs_length, filename = fileName_length_out,  overwrite = TRUE, wopt= woptList))); flush.console()
      print(system.time(writeRaster(startday_1, filename = fileName_startDay1_out,  overwrite = TRUE, wopt= woptList))); flush.console()
      print(system.time(writeRaster(endday_1, filename = fileName_endDay1_out,  overwrite = TRUE, wopt= woptList))); flush.console()
      print(paste0("fileName_ct_out: ", fileName_ct_out))
      print(paste0("fileName_length_out: ", fileName_length_out))
      print(paste0("fileName_startDay1_out: ", fileName_startDay1_out))
      print(paste0("fileName_endDay1_out: ", fileName_endDay1_out))
    }
    mainct <- paste0("Count of consecutive days of at least 100 where temp is greater than 0°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
    mainrl <- paste0("Locations with at least 100 consecutive days where temp is greater than 0°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
    plot(r_runs$lyr.1, main = mainct)
    plot(r_runs$lyr.2, main = mainrl)
  }
}

# get GDDs during first of the 'growing' periods

for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (modelChoice in modelChoices) {
      for (speciesChoice in speciesChoices) {
        for (hem in hemispheres) {
          fileName_gdd_in <- paste0(locOfgddsFiles, modelChoice_lower, "_", hem, "_", "gdd", "_", speciesChoice, "_", k, "_", yearSpan, ".tif")
         gdds <- rast(fileName_gdd_in)
          if (hem == "SH")  {startDate <-  paste0(l, "-07-01"); endDate <- paste0(l + yearRange - 1, "-06-30")} # in southern hemisphere search July 1 to June 30 of the next year.
          indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
          indicesChar <- paste0("X", indices)
          gdds <- subset(gdds, indicesChar)
          fileName_startDay1_in <- paste0("data/cmip6/runs/", "startday_1_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          fileName_endDay1_in <- paste0("data/cmip6/runs/", "endday_1_", varName,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
          startDay <- rast(fileName_startDay1_in)
          endDay <- rast(fileName_endDay1_in)
          
          
        }
      }
    }
}
  }
  
  
  
  
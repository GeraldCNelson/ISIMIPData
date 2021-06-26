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
        mainct <- paste0("Count of consecutive days of at least 100 where temp is greater than 0°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
        mainrl <- paste0("Consecutive days of at least 100 where temp is greater than 0°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
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
    mainct <- paste0("Count of consecutive days of at least 100 where temp is greater than 0°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
    mainrl <- paste0("Consecutive days of at least 100 where temp is greater than 0°C, \nssp: ", k, ", yearspan: ", yearSpan, ", model: ", modelChoice)
    plot(r_runs$lyr.1, main = mainct)
    plot(r_runs$lyr.2, main = mainrl)
  }
}



# create dewpoint files
{
  library(terra)
  library(Rcpp)
  sourceCpp("R/cpp/dewpoint.cpp")
  source("R/ISIMIPconstants.R")
  
  # formula from https://www.omnicalculator.com/physics/dew-point#howto
  
  f_tDewplocal <- function(hurs, tas) {
    if (is.na(hurs[1])) {return(hurs)}
    print(tas[1])
    a <- 17.62
    b <- 243.12
    alpha <- log(hurs/100.0) + a * tas/(b + tas)
    tDew = b * alpha / (a - alpha)
    return(tDew)
  }
  #  exp = 7.5 * tas/(237.7 + tas)
  #  svp = 0.611 * 10.0^exp
  #  vp = svp * hurs/100.0
  #  y = log10(vp/0.611)/7.5
  # tDew = (y * 237.7)/(1.0 - y)
  #}
  
  #test data
  l <- 2041
  k <- "ssp585"
  yearRange <- 19
  yearSpan <- paste0(l, "_", l + yearRange)
  modelChoice <- "GFDL-ESM4"
  modelChoice_lower <- tolower(modelChoice)
  climFiles  <- list.files(locOfClimFiles)
  climFiles <- climFiles[!grepl("aux.xml", climFiles, fixed = TRUE)]
  climFiles <- paste0("climData/", climFiles)
  
  f_dptCombined <- function(k, l, modelChoice, climFiles) {
    modelChoice_lower <- tolower(modelChoice)
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_out <- paste0(locOfClimFiles,  modelChoice_lower, "_dwp_", k, "_", yearSpan, ".tif")
    if (!fileName_out %in% climFiles) {
      fileName_tas <- paste0(locOfClimFiles,  modelChoice_lower, "_tas_", k, "_", yearSpan, ".tif")
      fileName_hurs <-  paste0(locOfClimFiles,  modelChoice_lower, "_hurs_", k, "_", yearSpan, ".tif")
      tas <- rast(fileName_tas)
      hurs <- rast(fileName_hurs)
      
      v_dewpoint <- sds(hurs, tas)
      print(system.time(dewp <- lapp(v_dewpoint, f_tDewplocal)))
      fileName_out <- paste0(locOfClimFiles,  modelChoice_lower, "_dwp_", k, "_", yearSpan, ".tif")
      print(system.time(writeRaster(dewp, filename = fileName_out,  overwrite = TRUE,  wopt= woptList)))
      print(paste0("dewpoint file name out: ", fileName_out))
      print("--------------------")
    }
  }
}
# dewpt, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (modelChoice in modelChoices) {
      f_dptCombined(k, l, modelChoice, climFiles)
    }
  }
}

# dewpoint, historical -----
k <- "historical"
l <- 1991
for (modelChoice in modelChoices) {
  f_dptCombined(k, l, modelChoice, climFiles)
}

# create dewpoint files
{
  library(terra)
  library(meteor)
  # library(Rcpp)
  # sourceCpp("R/cpp/dewpoint.cpp")
  source("R/ISIMIPconstants.R")
  terraOptions(memfrac = 2,  ncopies = 1, progress = 10, tempdir =  "data/ISIMIP", verbose = FALSE) # need to use a relative path
  
  
  #test data
  l <- 2041
  k <- "ssp585"
  yearSpan <- paste0(l, "_", l + yearRange)
  modelChoice <- "GFDL-ESM4"
  modelChoice_lower <- tolower(modelChoice)
  
  # get list of already completed files
  climFiles  <- list.files(locOfClimFiles)
  climFiles <- climFiles[!grepl("aux.xml", climFiles, fixed = TRUE)]
  climFiles <- paste0(locOfClimFiles, climFiles)
  
  f_dptCombined <- function(k, l, modelChoice, climFiles) {
    modelChoice_lower <- tolower(modelChoice)
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_out <- paste0(locOfClimFiles,  modelChoice_lower, "_dwp_", k, "_", yearSpan, ".tif")
    if (!fileName_out %in% climFiles) {
      fileName_tas <- paste0(locOfClimFiles,  modelChoice_lower, "_tas_", k, "_", yearSpan, ".tif")
      fileName_hurs <-  paste0(locOfClimFiles,  modelChoice_lower, "_hurs_", k, "_", yearSpan, ".tif")
      tas <- rast(fileName_tas)
      hurs <- rast(fileName_hurs)
      dewPointHolder <- c()
      for (i in l:(l + yearRange)) {
        startDate <- paste0(i, "-01-01"); endDate <- paste0(i, "-12-31") 
        dates <- seq(as.Date(startDate), as.Date(endDate), 1)
        indices <- seq(as.Date(startDate), as.Date(endDate ), by = "days")
        indicesChar <- paste0("X", indices)
        tas_yr <- subset(tas, indicesChar)
        hurs_yr <- subset(hurs, indicesChar)
        
        v_dewpoint <- sds( hurs_yr, tas_yr)
        print(system.time(dewp <- lapp(v_dewpoint, f_tDewp)))
        names(dewp) <- indicesChar
        dewPointHolder <- c(dewPointHolder, dewp)
      }
      dewp_out <- rast(dewPointHolder)
      print(system.time(writeRaster(dewp_out, filename = fileName_out,  overwrite = TRUE,  wopt= woptList)))
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

# find old dwp files
dwpFiles  <- climFiles[grepl("_dwp", climFiles, fixed = TRUE)]


# old code storage below ------
# # formula from https://www.omnicalculator.com/physics/dew-point#howto
# 
# f_tDewplocal <- function(hurs, tas) {
#   print(str(tas))
#   print(str(hurs))
#   
#   if (is.na(hurs[,1])) {return(hurs)}
#   # print(tas[1])
#   a <- 17.62
#   b <- 243.12
#   alpha <- log(hurs/100.0) + a * tas/(b + tas)
#   tDew = b * alpha / (a - alpha)
#   return(tDew)
# }
# # exp = 7.5 * tas/(237.7 + tas)
# # svp = 0.611 * 10.0^exp
# # vp = svp * hurs/100.0
# # y = log10(vp/0.611)/7.5
# # tDew = (y * 237.7)/(1.0 - y)
# }
# 
# # from SVP.R in meteor
# saturatedVaporPressure <- function(tas) {
#   .611 * 10^(7.5 * tas / (237.7 + tas))  #kpa
# }
# 
# tDew <- function(tas, hurs) {
#   #  tmp_out <<- tmp
#   # relh <- pmin(relh, 100)
#   # relh <- pmax(relh, 0)
#   svp <- saturatedVaporPressure(tas)
#   vp <- svp * hurs / 100
#   y <- log10(vp /.611) / 7.5
#   dew <- (y * 237.7) / (1 - y)
#   return(dew)
# }

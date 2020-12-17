# create ensemble daily means for 20 year periods
library("terra")
#library("crayon")

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
  terraOptions(memfrac = 4, progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path, memfrac = .9,
}else{
  terraOptions(memfrac = .6,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
}

woptList <- list(gdal=c("COMPRESS=LZW"))

locOfFiles <- "data/bigFiles/"
climateVars <- c("tasmax", "tasmin", "tas", "pr", "hurs", "ps", "rsds", "sfcWind") 
climateVars <- c( "tasmax", "tasmin")
sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp126") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
#startyearChoices <-  c(2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 19
readRast_climVar_ensemble <- function(i) {
  modelName.lower <- tolower(i)
  fileNameIn <- paste0("data/bigFiles/dyMean20yr_", modelName.lower, "_", k,  "_", m, "_global_daily_", yearSpan, ".tif") # note yearSpan here
  print(fileNameIn)
  r <- rast(fileNameIn)
}

dailyMean <- function() {
  yearSpan <- paste0(l, "_", l + yearRange)
  x <- lapply(modelChoices, readRast_climVar_ensemble)
  r <- rast(x)
  indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
  fileName_out <- paste0("data/bigFiles/ensemble_dyMean20yr_", k,  "_", m, "_global_daily_", yearSpan, ".tif") # note yearSpan here
  print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
}

# daily ensemble means of climate variables -----
for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (m in climateVars) {
      dailyMean()
    }
  }
}

# historical daily ensemble means of climate variables -----
k <- "historical"
l = 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (m in climateVars) {
  dailyMean()
}





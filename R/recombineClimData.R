# recombine climate files to different combinations of years. Only works on the straight ISIMIP files.
library(data.table)
#library(raster)
library(terra)

#pathPrefix <- getwd()

dirList <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
dirList_lower <- tolower(dirList)
sspChoice <- c("ssp126") #"ssp126", "ssp585"
varsToCombine <- c("tasmax", "tasmin", "pr", "hurs")
varsToCombine <- c("rsds", "sfcwind")
# filesInDir <- list.files("/Volumes/PassportMac/ISIMIP/cmip6/", full.names = TRUE, recursive = TRUE)
#startYearChoices <- c(2031, 2071)
startYearChoices <- c(2031, 2081)
yearRangeOld <- 9
#yearRangeNew <- 29
yearRangeNew <- 19
woptList <- list(gdal=c("COMPRESS=LZW"))

#testvalues
l = 1
j = "ssp126"
k = "tasmax"
l = 2031

for (i in 1:length(dirList)) {
  for (j in sspChoice) {
    for (k in varsToCombine) {
      for (l in startYearChoices) {
        print(paste0("working on: directory: ", dirList[i], ", ssp ", j, ", variable ", k, ", start year ", l))
        yearSpan_r1 <- paste0(l, "_", l + yearRangeOld)
        yearSpan_r2 <- paste0(l+10, "_", l+10 + yearRangeOld)
        yearSpan_r3 <- paste0(l+20, "_", l+20 + yearRangeOld)
        r1 <- rast(paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/", j, "/", dirList[i], "/", dirList_lower[i], "_", j,  "_", k, "_", "global_daily", "_", yearSpan_r1, ".tif"))
        r2 <- rast(paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/", j, "/", dirList[i], "/", dirList_lower[i], "_", j,  "_", k, "_", "global_daily", "_", yearSpan_r2, ".tif"))
        r3 <- rast(paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/", j, "/", dirList[i], "/", dirList_lower[i], "_", j,  "_", k, "_", "global_daily", "_", yearSpan_r3, ".tif"))
        yearSpan_final <- paste0(l, "_", l + yearRangeNew)
        rNew <- c(r1, r2, r3)
        rNewOut <- tapp(rNew, 1:12, fun = mean)
        names(rNewOut) <- month.abb
        
        outFileName <-  paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/", j, "/", dirList[i], "/", dirList_lower[i], "_", j,  "_", k, "_", "global_daily", "_", yearSpan_final, ".tif")
        print(paste0("out file name: ", outFileName))
        print(system.time(writeRaster(rNewOut, outFileName,  overwrite = TRUE, wopt= woptList))); flush.console()
        
      }
    }
  }
}

# for observed data
startYears <- c(1981) 
dirList <- c("observed")
#test vars
j = i = "observed"
l = 1981

# ssp choice not relevant for observed data.
for (k in varsToCombine) {
  for (l in startYears) {
    print(paste0("working on: directory ", i, ", variable ", k, ", start year ", l))
    yearSpan_r1 <- paste0(l, "_", l + yearRangeOld)
    yearSpan_r2 <- paste0(l+10, "_", l+10 + yearRangeOld)
    yearSpan_r3 <- paste0(l+20, "_", l+20 + yearRangeOld)
    
    r1 <- rast(paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/", j, "/", "gswp3-w5e5_obsclim", "_", k, "_", "global_daily", "_", yearSpan_r1, ".tif"))
    r2 <- rast(paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/", j, "/", "gswp3-w5e5_obsclim", "_", k, "_", "global_daily", "_", yearSpan_r2, ".tif"))
    r3 <- rast(paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/", j, "/", "gswp3-w5e5_obsclim", "_", k, "_", "global_daily", "_", yearSpan_r3, ".tif"))
    yearSpan_final <- paste0(l, "_", l + yearRangeNew)
    rNew <- c(r1, r2, r3)
    rNewOut <- tapp(rNew, 1:12, fun = mean)
    names(rNewOut) <- month.abb
    
    outFileName <-  paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/", j, "/", "gswp3-w5e5_obsclim", "_", k, "_", "global_daily", "_", yearSpan_final, ".tif")
    print(system.time(writeRaster(rNewOut, outFileName,  overwrite = TRUE, wopt= woptList))); flush.console()
  }
}

# now do ensemble data
locOfCMIP6ncFiles <- "data-raw/ISIMIP/cmip6/unitsCorrected/"
sspChoices <- c("ssp126", "ssp585")
startYearChoices <- c(2031, 2071)
dirList <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
dirList_lower <- tolower(dirList)

k <- "ssp585"
l <- 2031
j = "rsds"
i = 2031

for (k in sspChoices) {
  for (l in startYearChoices) {
    for (j in varsToCombine) {
      for (i in 1:length(dirList)) {
        yearSpan_final <- paste0(l, "_", l + yearRangeNew)
        inFileName <-  paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/", k, "/", dirList[i], "/", dirList_lower[i],  "_", k, "_",  j, "_", "global_daily", "_", yearSpan_final, ".tif")
        rin <- rast(inFileName)
        rname <- paste0("r_", dirList[i])
        assign(rname, rin)
      }
      rout <- c(get(paste0("r_", dirList)))
      rEnsemble <- tapp(rout, 1:12, fun = mean)
      outFileName <- paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/ensemble/ensemble_", k, "_",  j, "_", "global_daily", "_", yearSpan_final, ".tif")
      print(system.time(writeRaster(rEnsemble, outFileName,  overwrite = TRUE, wopt= woptList))); flush.console()
    }
  }
}

# for observed data
dirList <- c("observed")
for (j in varsToCombine) {
  fileIn <- paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/observed/gswp3-w5e5_obsclim_", j, "_global_daily_1981_2010.tif")
  fileout <- paste0("/Volumes/PassportMac/ISIMIP/cmip6/unitsCorrectedMean/ensemble/gswp3-w5e5_obsclim_", j, "_global_daily_1981_2010.tif")
  file.copy(from = fileIn, to = fileout, overwrite = TRUE)
}
  


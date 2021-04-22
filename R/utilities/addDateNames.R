library(data.table)
library(terra)
woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))
terraOptions(memfrac = 2, progress = 0, tempdir =  "data/ISIMIP", verbose = TRUE)

holder <- data.table()
climateVars <- c("tasmin", "tasmax")
climateVars <- c("hurs")
sspChoices <- c("ssp126", "ssp585") 
startYearChoices <-  c(2041, 2081) 
locOfFiles <- "data/bigFiles/"
yearRange = 19
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") 

for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (m in climateVars) {
      for (i in modelChoices) {
        modelChoice <- tolower(i)
        fileName_in <- paste0("data/bigFiles/", modelChoice, "_", k, "_", m, "_global_daily_", yearSpan, ".tif")
        r <- rast(fileName_in)
        if (!grepl("X", names(r)[1], fixed = "TRUE")) {
          print(fileName_in)
          print(head(names(r)))
          holder <- rbind(holder, fileName_in)
        }
      }
    }
  }
}


test1 <- rast("/Volumes/ExtremeSSD3/climate3b/ssp126/MRI-ESM2-0/mri-esm2-0_r1i1p1f1_w5e5_ssp126_tasmax_global_daily_2041_2050.nc")

for (i in 1:nrow(holder)) {
  
  fileName_split <- holder[i]
  split_out <- unlist(strsplit(as.character(holder[i]), split = "_", fixed = "TRUE"))
  split_out_path <- unlist(strsplit(split_out[1], "/"))
  
  ten_years <- paste0(as.numeric(split_out[6]), "_", as.numeric(split_out[6]) + 9)
  ten_years_2 <- paste0(as.numeric(split_out[6]) + 10, "_", as.numeric(split_out[6]) + 19)
  twenty_years <- paste0(as.numeric(split_out[6]), "_", as.numeric(split_out[6]) + 19)
  connector <- "r1i1p1f1_w5e5"
  if (split_out_path[3] == "ukesm1-0-ll") connector <- "r1i1p1f2_w5e5"
  fileName_in_test1 <- paste0("/Volumes/ExtremeSSD3/climate3b/", split_out[2], "/", toupper(split_out_path[3]), "/",  split_out_path[3], "_", connector, "_", split_out[2], "_", split_out[3], "_global_daily_", ten_years, ".nc")
  
  fileName_in_test2 <- paste0("/Volumes/ExtremeSSD3/climate3b/", split_out[2], "/", toupper(split_out_path[3]), "/",  split_out_path[3], "_", connector, "_", split_out[2], "_", split_out[3], "_global_daily_", ten_years_2, ".nc")
  
  l <- as.numeric(split_out[6])
  startDate <- paste0(l, "-01-01")
  endDate <- paste0(l + yearRange, "-12-31")
  indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  indices <- paste0("X", indices)
  
  test1 <- rast(fileName_in_test1)
  test2 <- rast(fileName_in_test2)
  test3 <- c(test1, test2)
  test4 <- test3 - 273.15
  names(test4) <- indices
  
  fileName_out <- paste0("data/bigFiles/", split_out_path[3], "_", split_out[2], "_", split_out[3], "_", "global_daily_",twenty_years, ".tif")
  
  writeRaster(test4, filename = fileName_out,  overwrite = TRUE, wopt= woptList)
  print(paste0("filename out: ", fileName_out))
}

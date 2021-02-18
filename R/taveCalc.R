# tas calculations from tmin and tmax

#library(raster)
library(terra)
woptList <- list(gdal=c("COMPRESS=LZW"))
terraOptions(memfrac = 2,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path

bigFileList <- list.files("/Volumes/ExtremeSSD2/bigFiles", full.names = TRUE)
bigFileList <- bigFileList[!grepl("aux.xml", bigFileList, fixed = TRUE)]

#small group of files
bigFileList <- bigFileList[grepl("ukesm1-0-ll_ssp126_tasmax_global_daily", bigFileList, fixed = TRUE)]

  bigFileList_tasmax <- bigFileList[grepl("tasmax", bigFileList, fixed = TRUE)]
bigFileList_tasmin <- gsub("tasmax", "tasmin", bigFileList_tasmax)

for (i in 1:length(bigFileList_tasmax)) {
  tmax <- rast(bigFileList_tasmax[i])
  tmin <- rast(bigFileList_tasmin[i])
  print(paste0("max tmax: ", max(minmax(tmax)), ", min tmax: ", min(minmax(tmax))))
  print(paste0("max tmin: ", max(minmax(tmin)), ", min tmin: ", min(minmax(tmin))))
  # system.time(tmax <- tmax + 0)
  # system.time(tmin <- tmin + 0)
  print(system.time(tas <- (tmax + tmin)/2))
  tasOut <-  gsub("tasmax", "tas", bigFileList_tasmax[i])
  print(paste0("file name out: ", tasOut))
  print(system.time(writeRaster(tas, tasOut, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))))
  gc()
}


# alreadyDone <- c("mri-esm2-0_historical_tas_global_daily_1991_2010.tif", "mri-esm2-0_historical_tas_global_daily_1991_2010.tif.aux.xml", "mpi-esm1-2-hr_ssp585_tas_global_daily_2081_2100.tif", "mpi-esm1-2-hr_ssp585_tas_global_daily_2081_2100.tif.aux.xml", "mpi-esm1-2-hr_ssp585_tas_global_daily_2041_2060.tif", "mpi-esm1-2-hr_ssp585_tas_global_daily_2041_2060.tif.aux.xml", "mpi-esm1-2-hr_ssp126_tas_global_daily_2081_2100.tif", "mpi-esm1-2-hr_ssp126_tas_global_daily_2081_2100.tif.aux.xml", "mpi-esm1-2-hr_ssp126_tas_global_daily_2041_2060.tif", "mpi-esm1-2-hr_ssp126_tas_global_daily_2041_2060.tif.aux.xml", "mpi-esm1-2-hr_historical_tas_global_daily_1991_2010.tif", "mpi-esm1-2-hr_historical_tas_global_daily_1991_2010.tif.aux.xml", "ipsl-cm6a-lr_ssp585_tas_global_daily_2081_2100.tif", "ipsl-cm6a-lr_ssp585_tas_global_daily_2081_2100.tif.aux.xml", "ipsl-cm6a-lr_ssp585_tas_global_daily_2041_2060.tif", "ipsl-cm6a-lr_ssp585_tas_global_daily_2041_2060.tif.aux.xml", "ipsl-cm6a-lr_ssp126_tas_global_daily_2081_2100.tif", "ipsl-cm6a-lr_ssp126_tas_global_daily_2081_2100.tif.aux.xml", "ipsl-cm6a-lr_ssp126_tas_global_daily_2041_2060.tif", "ipsl-cm6a-lr_ssp126_tas_global_daily_2041_2060.tif.aux.xml", "ipsl-cm6a-lr_historical_tas_global_daily_1991_2010.tif", "ipsl-cm6a-lr_historical_tas_global_daily_1991_2010.tif.aux.xml", "gfdl-esm4_ssp585_tas_global_daily_2081_2100.tif", "gfdl-esm4_ssp585_tas_global_daily_2081_2100.tif.aux.xml", "gfdl-esm4_ssp585_tas_global_daily_2041_2060.tif", "gfdl-esm4_ssp585_tas_global_daily_2041_2060.tif.aux.xml", "gfdl-esm4_ssp126_tas_global_daily_2081_2100.tif", "gfdl-esm4_ssp126_tas_global_daily_2081_2100.tif.aux.xml", "gfdl-esm4_ssp126_tas_global_daily_2041_2060.tif", "gfdl-esm4_ssp126_tas_global_daily_2041_2060.tif.aux.xml", "gfdl-esm4_historical_tas_global_daily_1991_2010.tif", "gfdl-esm4_historical_tas_global_daily_1991_2010.tif.aux.xml")
# alreadyDone <- alreadyDone[!grepl("aux.xml", alreadyDone, fixed = TRUE)]
# alreadyDone <- paste0("data/bigFiles/", alreadyDone)
# alreadyDone <- gsub("tas", "tasmax", alreadyDone)
# bigFileList_tasmax <- bigFileList_tasmax[!bigFileList_tasmax %in% alreadyDone]


tmax <- rast(bigFileList_tasmax[i])
tmin <- rast(bigFileList_tasmin[i])
print(system.time(tas <- (tmax + tmin)/2))
tasOut <-  gsub("tasmax", "tas", bigFileList_tasmax[i])
print(paste0("file name out: ", tasOut))
print(system.time(writeRaster(tas, tasOut, format = "GTiff", overwrite = TRUE, wopt=list(gdal="COMPRESS=LZW"))))
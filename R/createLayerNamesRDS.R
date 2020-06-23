# create RDS for the names of each of the laybers in the  .nc files from ISIMIP
# the structure of the layer names is like "X2001.01.01" "X2001.01.02" "X2001.01.03" - one layer for each day in the 10 year period of the .nc file
# because there are leap years, different files will have possible two more layers

library(raster)

yearRange <- 9
yearSpan <- 
  # observed data layer names 2001-2010
  temp <- rast"data-raw/ISIMIP/cmip6/unitsCorrected/observed/gswp3-w5e5_obsclim_tasmax_global_daily_2001_2010.nc")
namesList <- names(temp)
saveRDS(namesList, "data-raw/ISIMIP/ISIMIPLayerNames_2001_2010.RDS")

for (l in c(2021, 2051, 2091)) {
  yearSpan <- paste0(l, "_", l + yearRange)
  # use tasmax from GFDL. Same layer names for all models
  inFile <- paste0("data-raw/ISIMIP/cmip6/unitsCorrected/ssp585/GFDL-ESM4/gfdl-esm4_ssp585_tasmax_global_daily_", yearSpan, ".nc")
  temp <- rastinFile)
  namesList <- names(temp)
  saveRDS(namesList, paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))
}

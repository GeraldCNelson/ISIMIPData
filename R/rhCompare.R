library(terra)

sourceDir <- "data/aveRHfiles/test-rh/"
  fileList_in <- list.files(sourceDir)
  for (i in fileList_in) {
    rh <- rast(paste0(sourceDir, i), format = "ascii")
    setMinMax(rh)
    print(rh)
  }
  
rhminJan <- rast("data/aveRHfiles/test-rh/OBS_RN10_2000_01.asc")
rhminJul <- rast("data/aveRHfiles/test-rh/OBS_RN10_2000_07.asc")
rhmaxJan <- rast("data/aveRHfiles/test-rh/OBS_RX10_2000_01.asc") 
rhmaxJul <- rast("data/aveRHfiles/test-rh/OBS_RX10_2000_07.asc")

# zeros appear to be missing values. Set to NA
rhminJan[rhminJan == 0] <- NA
rhminJul[rhminJul == 0] <- NA
rhmaxJan[rhmaxJan == 0] <- NA
rhmaxJul[rhmaxJul == 0] <- NA
rhminJan <- rhminJan/10
rhminJul <- rhminJul/10
rhmaxJan <- rhmaxJan/10
rhmaxJul <- rhmaxJul/10

rhaveJan <- (rhminJan + rhmaxJan)/2
rhaveJul <- (rhminJul + rhmaxJul)/2
print(rhaveJan)
print(rhaveJul)

rhave_IM_1981 <- rast("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/ensembleMn_mnthMn_10Yr_historical_hurs_1981_1990.tif")
rhave_IM_1991 <- rast("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/ensembleMn_mnthMn_10Yr_historical_hurs_1991_2000.tif")
rhave_IM_2001 <- rast("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/ensembleMn_mnthMn_10Yr_historical_hurs_2001_2010.tif")
rhave <- c(rhave_IM_1981, rhave_IM_1991, rhave_IM_2001)

rhave_IM <- tapp(rhave, names(rhave), mean)
rhave_IM_jan <- subset(rhave, 1)
rhave_IM_jul <- subset(rhave, 7)
rhave_IM_jan
rhave_IM_jul
plot(rhaveJan, main = "PHT rhAve Jan")
plot(rhaveJul, main = "PHT rhAve Jul")
plot(rhave_IM_jan, main = "ensemble mean, rhAve January, 1981-2010")
plot(rhave_IM_jul, main = "ensemble mean, rhAve July, 1981-2010")

rhave_delta_Jan <- rhaveJan - rhave_IM_jan
plot(rhave_delta_Jan, main = "PHT rhAve Jan minus GCN rhAve Jan")

rhave_delta_Jul <- rhaveJul - rhave_IM_jul
plot(rhave_delta_Jul, main = "PHT rhAve Jul minus GCN rhAve Jan")

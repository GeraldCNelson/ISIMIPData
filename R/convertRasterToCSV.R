# create csv files from tmin and tmax
source("R/globallyUsed.R")
terraOptions(memfrac = 1,  progress = 10, tempdir =  "data/ISIMIP/", verbose = TRUE)
yearRange <- 9
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "GFDL-ESM4", "UKESM1-0-LL", "IPSL-CM6A-LR")

# tmax <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmax_global_daily_2051_2060.nc")
# tmin <- rast("NAtemp/ukesm1-0-ll_ssp585_tasmin_global_daily_2051_2060.nc")

#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2051

modelName.lower <- tolower(i)
yearSpan <- paste0(l, "_", l + yearRange)

fileIn_tmax <- paste0(locOfCMIP6ncFiles,  k, "/", i, "/", modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
fileIn_tmin <- paste0(locOfCMIP6ncFiles,  k, "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")+
tmax <- rast(fileIn_tmax)
tmax_df <- as.data.frame(tmax, xy = TRUE)
tmax_df_complete <- tmax_df[complete.cases(tmax_df),]

tmin <- rast(fileIn_tmin)
tmin_df <- as.data.frame(tmin, xy = TRUE)
tmin_df_complete <- tmin_df[complete.cases(tmin_df),]
startDate <- paste0(l, "-01-01")
endDate <- paste0(l + yearRange, "-12-31")
indices <- seq(as.Date(startDate), as.Date(endDate), 1)
indices <- as.character(indices)
names(tmax_df_complete)[3:length(tmax_df_complete)] <- names(tmin_df_complete)[3:length(tmin_df_complete)] <- indices
names(tmax_df_complete)[1:2] <- names(tmin_df_complete)[1:2] <- c("lon", "lat")
fileOut_tmax_complete <- paste0("data/cmip6/chillingUnits/", i, "_", k , "_", yearSpan, "_tmax_df_complete.csv.gz")
fileOut_tmin_complete <- paste0("data/cmip6/chillingUnits/", i, "_", k , "_", yearSpan, "_tmin_df_complete.csv.gz")

write.csv(tmax_df_complete, gzfile(fileOut_tmax_complete), row.names = FALSE)
write.csv(tmin_df_complete, gzfile(fileOut_tmin_complete), row.names = FALSE)

library(terra)
d <- rast(nrows=100, ncols=100, nl=5)
rstart <- rast(d, nlyr=1)
nc <- ncell(d) 
set.seed(88)
values(d) <- cbind(runif(nc, min = -10, max = 10), runif(nc, min = -20, max = 10), 
                   runif(nc, min = -30, max = 10), runif(nc, min = -40, max = 10), runif(nc, min = -50, max = 10))
values(rstart) <- round(runif(nc, min = 1, max = 2))
rend <- rstart + 3

library(Rcpp)
cppFunction('std::vector<double> gtemp(NumericMatrix cal, NumericMatrix wth) {
    std::vector<double> out(cal.nrow(), NAN);
    for (int i=0; i<cal.nrow(); i++) {
      if (!std::isnan(cal(i,0))){
         NumericVector v = wth(i,_);
         size_t start = cal(i,0)-1;
         size_t end = cal(i,1);
         out[i] = std::accumulate(v.begin()+start, v.begin()+end, 0.0);
      }  
    }
    return out;
}')

f <- function(i, v) {
  j <- !is.na(i[,1])
  r <- rep(NA, nrow(i))
  x <- cbind(i[j,,drop=FALSE], v[j,,drop=FALSE])
  r[j] <- apply(x, 1, function(y) sum(y[ (y[1]:y[2])+2 ] )) 
  r
}
cal <-stack(r.start, r.end)
x <- overlay(cal, b, fun= f, recycle=FALSE)

# now do terra stuff - https://rspatial.org/terra/pkg/3-objects.html


# d, rstart, and rend need to be in the terra format, a SpatRaster

# setup some stuff
source("R/globallyUsed.R")

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c("UKESM1-0-LL", "IPSL-CM6A-LR") #"MPI-ESM1-2-HR", "MRI-ESM2-0")# "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, 
#modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"

startyearChoices <-  c(2051) #, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
hemisphereList <- c("Northern", "Southern")
northerHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-  c( -180, 180, -90, 0)

yearRange <- 9
gddsfilesLoc <- "data/cmip6/growingDegreeDays/"

#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2051
m <- "Wheat"
cropName <- m
yearSpan <- paste0(l, "_", l + yearRange)

layerNames <- readRDS(paste0("data-raw/ISIMIP/ISIMIPLayerNames_", yearSpan, ".RDS"))
modelName.lower <- tolower(i)
gddIn_crop <- paste0(gddsfilesLoc, modelName.lower, "_", m, "_", k, "_gdd", "_global_daily_", yearSpan, ".tif")
# use terra function
gdd <- rast(gddIn_crop)


fileNameMask.in <- paste0("data/crops/rasterMask_", tolower(m), ".tif")
mask <- rast(fileNameMask.in)

cropCalendarName <- ann_crop_temp_table[crop %in% cropName, crop.calendar]
cropCalFilesLoc <- paste0("data-raw/crops/cropCalendars/ALL_CROPS_netCDF_0.5deg_filled/")
fileInName <- paste0(cropCalendarName, ".crop.calendar.fill.nc")
#    locNFileIn <- paste0(filesLoc, fileInName, ".gz")
locNFileIn <- paste0(cropCalFilesLoc, fileInName)
R.utils::gunzip(paste0(locNFileIn, ".gz"), remove = FALSE)

croppingCalendar_plant <- rast(locNFileIn, var = "plant")
croppingCalendar_harvest <- rast(locNFileIn, var = "harvest")
croppingCalendar_plant_crop <- mask(croppingCalendar_plant, mask)
croppingCalendar_harvest_crop <- mask(croppingCalendar_harvest, mask)







idx <- c(rstart, rend)
z <- rapp(d, idx, "sum")
# Ultimate goal - create a SpatRaster with two layers for each year from a SpatRaster that has 10 or more years of daily minimum temperature data (tmin)
# layer 1 is the start day (Julian) of the growing season
# layer 2 is the end day (Julian) of the growing season

# The growing season start is defined as the first day after the last freeze and is in a series of days at least 60 long where tmin > 0
# The growing season end day is the last day before the first freeze after the continuous series with tmin > 0

#First generate a vector of tmin values for a single year from a single xy coordinate and write a function that does what you want with a vector.
#To reduce the size of the problem initially, 1. work only on the northern hemisphere and 2 only for a single year in a 10 year SpatRaster. Code for southern hemisphere is below as well.

library(terra)
terraOptions(memfrac = 1,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path

# here's a tentative function that does one year at a time. 
# The code 'g <- gregexpr ...' generates the start and end values of a common characteristic (in this case tmin >0) in the vector extracted.
# The answer at https://stackoverflow.com/questions/64091829/r-how-to-find-a-series-of-common-values-in-a-vector-identifying-growing-season as some more useful code
#here's where the loop structure of some kind needs to go
getGrowingSeason <- function(yearIn) {
  growingSeason <- rast(yearIn, nlyrs = 2)
  values(growingSeason) <- 0
  names(growingSeason) <- c("growingSeason_start", "growingSeason_end")
  terra:::.mem_info(growingSeason, 1)
  
#  system.time( for (i in 1:ncell(yearIn)) {
    system.time( 
      for (i in 8000:28000) {
      cellVector <- yearIn[i]
    if (!is.nan(cellVector[1])) {
      g <- gregexpr("1{60,}", paste(+(cellVector > 0), collapse = ""))[[1]]
      if (!g[[1]] == -1) { # no need to write to growing season if g returns 1
        startGSDay <- g[[1]]
        matchLength <- attributes(g)$match.length
        endGSDay <- startGSDay + matchLength - 1
        GSStartEnd <- as.matrix(c(startGSDay,  endGSDay), nrow = 2, ncol = 1)
        growingSeason$growingSeason_start[i] <- startGSDay
        growingSeason$growingSeason_end[i] <- endGSDay
        terra:::.mem_info(growingSeason, 1)
        
        

      }
    }
  }
  )
  return(growingSeason)
}

minDaysAbove0 <- 60
# test values
l <- 2071
yearRange <- 9
m = "gfdl-esm4"
k = "ssp585"

# Read in and prep the data
fileName_tmin <- paste0("NAtemp/", m, "_", k, "_", "tasmin", "_", "global_daily", "_", yearRange, ".tif")
temp <- "NAtemp/gfdl-esm4_ssp585_tasmin_global_daily_2071_2080.tif"
tmin <- rast(temp)
startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
indices <- seq(as.Date(startDate), as.Date(endDate), 1)
indicesChar <- paste0("X", as.character(indices))
names(tmin) <- indicesChar

startLayer <- paste0(l, "-01-01")
endLayer <- paste0(l, "-12-31")
indicesYear <- seq(as.Date(startLayer), as.Date(endLayer), 1)
indicesCharYear <- paste0("X", as.character(indicesYear))
system.time(tmin_l <- subset(tmin, indicesCharYear))

# Do northern hemisphere
northernHemExtent <- c( -180, 180, 0, 90)
print("time to crop NH")
system.time(NH <- crop(tmin, ext(northernHemExtent)))

startLayer <- paste0(l, "-01-01")
endLayer <- paste0(l, "-12-31")
indicesYear <- seq(as.Date(startLayer), as.Date(endLayer), 1)
indicesCharYear <- paste0("X", as.character(indicesYear))
print("time to remove extra layers")
system.time(NH_yr <- subset(NH, indicesCharYear))
terra:::.mem_info(tmin, 1)

print("time to run function")
system.time(growingSeason_NH <- getGrowingSeason(NH_yr))

# now work on southern hemisphere
southernHemExtent <-  c( -180, 180, -60, 0)
SH <- crop(tmin, ext(southernHemExtent))

# get a year's worth of data starting 1 July
startLayer <- paste0(l, "-07-01")
endLayer <- paste0(l+1, "-06-30")
indicesYear <- seq(as.Date(startLayer), as.Date(endLayer), 1)
indicesCharYear <- paste0("X", as.character(indicesYear))
SH_yr <- subset(SH, indicesCharYear)
print("time to run function")
system.time(growingSeason_SH <- getGrowingSeason(SH_yr))

# graphing stuff
test_NoZeros <- test
test_NoZeros[test_NoZeros == 0] <- NA
test_df <- as.data.frame(test_NoZeros, xy = TRUE)

names(test_df)[names(test_df) == "x"] <- "longitude"
names(test_df)[names(test_df) == "y"] <- "latitude"
test_df_long <- tidyr::pivot_longer(test_df, c("growingSeason_start", "growingSeason_end"), names_to = "gs", values_to = "value")
test_df_long$gs = factor(test_df_long$gs, levels = c("growingSeason_start", "growingSeason_end"))

titleText <- paste0("Growing season start and end")
legendTitle <- "day"

print(titleText)
gc()
colorList <- rev(RColorBrewer::brewer.pal(5, "YlGn"))
g <- ggplot(data = test_df_long) +
  labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  #  labs(x = "", y = "") +
  geom_raster(data = test_df_long, aes(longitude, latitude, fill = value)) + 
  scale_fill_gradientn(colors = colorList, na.value = "transparent") +
  theme(axis.text.x=element_text(size=rel(0.6))) + #, angle=90
  theme(axis.text.y=element_text(size=rel(0.6)))

g <- g +  facet_wrap(vars(gs))
outFilename <- paste0("graphics/cmip6/growingSeasons/gs", "_", l, "NH",".png")
png(filename = outFilename, width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

# testing row/cols, xys, and cell numbers
x <- -95.75
y <- 37.25
cellFromXY( NH_yr, cbind(x,y))
colFromX(NH_yr, x)
rowFromY(NH_yr, y)


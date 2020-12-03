source("R/globallyUsed.R")
# always first write a function for a vector. 

fun <- function(cellVector) {
  startend <- c(NA, NA) 
  if (is.nan(cellVector[1])) {
    return(startend)
  }
  g <- gregexpr("1{60,}", paste(+(cellVector > 0), collapse = ""))[[1]]
  if (!g[[1]] == -1) { # no need to write to growing season if g returns 1
    startend[1] <- g[[1]]
    matchLength <- attributes(g)$match.length
    startend[2] <- startend[1] + matchLength - 1
  }
  return(startend) 
}


locOfFiles <- locOfCMIP6tifFiles
sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c( "MRI-ESM2-0", "UKESM1-0-LL") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2041
yearNumber <- 2043
j <- "tmin"
yearRange <- 19
woptList <- list(gdal=c("COMPRESS=LZW"))

# it looks like this could be done using multiple processors since the memory load is small for each iteration.
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      tmin <- rast(fileName_tasmin)
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
        # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        # indicesChar <- paste0("X", as.character(indices))
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmin_yr <- subset(tmin, indicesCharYear))
        # then use the function like this
        print(system.time(growingSeasonGlobe <- app(tmin_yr, fun)))
        names(growingSeasonGlobe) <- c("growingSeason_start", "growingSeason_end")
        
        fileName_out <- paste0("data/cmip6/growingSeasons/growSeasonGlobe_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(growingSeasonGlobe, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# now work on southern hemisphere
southernHemExtent <-  c( -180, 180, -90, 0)

yearRange <- 18 # one less year because of 6 month offset
woptList <- list(gdal=c("COMPRESS=LZW"))

# it looks like this could be done using multiple processors since the memory load is small for each iteration.
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange + 1) # the +1 here is to get at the original file names
      modelName.lower <- tolower(i)
      fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      tmin <- rast(fileName_tasmin)
      SH <- crop(tmin, ext(southernHemExtent))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber +1, "-06-30")
        # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
        # indicesChar <- paste0("X", as.character(indices))
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmin_yr_SH <- subset(SH, indicesCharYear))
        # then use the function like this
        print(system.time(growingSeasonSH <- app(tmin_yr_SH, fun)))
        names(growingSeasonSH) <- c("growingSeason_start", "growingSeason_end")
        
        fileName_out <- paste0("data/cmip6/growingSeasons/growSeasonSH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(growingSeasonSH, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

  
# graphing stuff
test_df <- as.data.frame(growingSeasonSH, xy = TRUE)

names(test_df)[names(test_df) == "x"] <- "longitude"
names(test_df)[names(test_df) == "y"] <- "latitude"
test_df_long <- tidyr::pivot_longer(test_df, c("growingSeason_start", "growingSeason_end"), names_to = "gs", values_to = "value")
test_df_long$gs = factor(test_df_long$gs, levels = c("growingSeason_start", "growingSeason_end"))

titleText <- paste0("Growing season start and end, Number of days from July 1")
legendTitle <- "day"

print(titleText)
gc()
colorList <- (RColorBrewer::brewer.pal(5, "YlGn"))
g <- ggplot(data = test_df_long) +
  labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  #  labs(x = "", y = "") +
  geom_raster(data = test_df_long, aes(longitude, latitude, fill = value)) + 
  scale_fill_gradientn(colors = colorList, na.value = "transparent") +
  theme(axis.text.x=element_text(size=rel(0.6))) + #, angle=90
  theme(axis.text.y=element_text(size=rel(0.6)))

g <- g +  facet_wrap(vars(gs))
outFilename <- paste0("graphics/cmip6/growingSeasons/gs", "_", l, "SH",".png")
png(filename = outFilename, width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()
  
  



  
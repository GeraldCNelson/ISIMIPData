source("R/globallyUsed.R")

locOfFiles <- locOfCMIP6tifFiles

sspChoices <- c("ssp126", "ssp585") 
#sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
#startyearChoices <-  c(2041) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_historical <- c(1991)
scenarioChoicesEnsemble <- c("historical", sspChoices)
northernHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-c( -180, 180, -90, 0)
hemisphere <- c("NH", "SH")

yearRange <- 19
yearRangeSH <- 18 # one less year because of 6 month offset

woptList <- list(gdal=c("COMPRESS=LZW"))

#test values
i <- "IPSL-CM6A-LR"
k <- "ssp585"
l <- 2041
yearNumber <- 2043

readRast_gs <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/growingSeasons/growingSeason", m, "_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  r <- rast(fileNameIn)
}

readRast_GrowSeasonEnsemble <- function(i) {
  fileNameIn <- paste0("data/cmip6/growingSeasons/growingSeason_", n, m, "_", i, "_", k,  "_", yearSpan, ".tif") # note yearSpan here
  print(fileNameIn)
  r <- rast(fileNameIn)
}

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

# it looks like this could be done using multiple processors since the memory load is small for each iteration.

# NH ------

for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      tmin <- rast(fileName_tasmin)
      system.time(NH <- crop(tmin, ext(northernHemExtent)))
      
      for (yearNumber in l:(l + yearRange)) {
        startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmin_yr_NH <- subset(NH, indicesCharYear))
        print(system.time(growingSeasonNH <- app(tmin_yr_NH, fun)))
        names(growingSeasonNH) <- c("growingSeason_start", "growingSeason_end")
        
        fileName_out <- paste0("data/cmip6/growingSeasons/growingSeasonNH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(growingSeasonNH, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# NH historical by model -----
k = "historical"
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
    tmin <- rast(fileName_tasmin)
    system.time(NH <- crop(tmin, ext(northernHemExtent)))
    
    for (yearNumber in l:(l + yearRange)) {
      startDate <- paste0(yearNumber, "-01-01"); endDate <- paste0(yearNumber, "-12-31")
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmin_yr_NH <- subset(NH, indicesCharYear))
      print(system.time(growingSeasonNH <- app(tmin_yr_NH, fun)))
      names(growingSeasonNH) <- c("growingSeason_start", "growingSeason_end")
      print(summary(growingSeasonNH))
      fileName_out <- paste0("data/cmip6/growingSeasons/growingSeasonNH_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(growingSeasonNH, fileName_out, overwrite = TRUE, woptList = woptList)
    }
  }
}

# SH -----
# it looks like this could be done using multiple processors since the memory load is small for each iteration.
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startyearChoices) {
      gc()
      yearSpan <- paste0(l, "_", l + yearRange) 
      yearSpanSH <- paste0(l, "_", l + yearRangeSH)
      modelName.lower <- tolower(i)
      fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
      tmin <- rast(fileName_tasmin)
      SH <- crop(tmin, ext(southernHemExtent))
      
      for (yearNumber in l:(l + yearRangeSH)) {
        startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber +1, "-06-30")
        indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
        indicesCharYear <- paste0("X", as.character(indicesYear))
        system.time(tmin_yr_SH <- subset(SH, indicesCharYear))
        print(system.time(growingSeasonSH <- app(tmin_yr_SH, fun)))
        names(growingSeasonSH) <- c("growingSeason_start", "growingSeason_end")
        
        fileName_out <- paste0("data/cmip6/growingSeasons/growingSeasonSH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(growingSeasonSH, fileName_out, overwrite = TRUE, woptList = woptList)
      }
    }
  }
}

# SH historical by model -----
locOfFiles_historical <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/"
k <- "historical"
for (i in modelChoices) {
  for (l in startyearChoices_historical) {
    gc()
    yearSpan <- paste0(l, "_", l + yearRange)
    yearSpanSH <- paste0(l, "_", l + yearRangeSH)
    modelName.lower <- tolower(i)
    fileName_tasmin <- paste0(locOfFiles, k,  "/", i, "/", modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
    tmin <- rast(fileName_tasmin)
    system.time(SH <- crop(tmin, ext(southernHemExtent)))
    
    for (yearNumber in l:(l + yearRangeSH)) {
      startDate <- paste0(yearNumber, "-07-01"); endDate <- paste0(yearNumber +1, "-06-30")
      indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
      indicesCharYear <- paste0("X", as.character(indicesYear))
      system.time(tmin_yr_SH <- subset(SH, indicesCharYear))
      print(system.time(growingSeasonSH <- app(tmin_yr_SH, fun)))
      names(growingSeasonSH) <- c("growingSeason_start", "growingSeason_end")
      
      fileName_out <- paste0("data/cmip6/growingSeasons/growingSeasonSH_",  i, "_", k, "_", yearNumber, ".tif")
      writeRaster(growingSeasonSH, fileName_out, overwrite = TRUE, woptList = woptList)
    }
  }
}

# 20 year growing seasons  ------
for (k in sspChoices) {
  for (m in hemisphere)
    for (i in modelChoices) {
      modelName.lower <- tolower(i)
      for (l in startyearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        print(m)
        yearnumberRange <- seq(l, (l + yearRange), 1)
        if (m %in% "SH") {
          yearnumberRange <- seq(l, (l + yearRangeSH), 1)
          yearSpan <- paste0(l, "_", l + yearRangeSH)
        }
        gc()
        x <- lapply(yearnumberRange, readRast_gs)
        x_start <- x
        r <- rast(x)
        index_r_start <-seq(1, nlyr(r), 2)
        index_r_end <-seq(2, nlyr(r), 2)
        r_start <- subset(r, index_r_start)
        r_end <- subset(r, index_r_end)
        fileNameOut_start <- paste0("data/cmip6/growingSeasons/growingSeason_start", m, "_", i, "_", k,  "_", yearSpan, ".tif")
        fileNameOut_end <- paste0("data/cmip6/growingSeasons/growingSeason_end", m, "_", i, "_", k,  "_", yearSpan, ".tif")
        print(paste0("fileNameOut_start out: ", fileNameOut_start))
        print(paste0("fileNameOut_end out: ", fileNameOut_end))
        print(system.time(writeRaster(r_start, fileNameOut_start, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
        print(system.time(writeRaster(r_end, fileNameOut_end, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
        r_start <- r_end <- NULL
        gc()
      }
    }
}

# 20 year historical, both  -----
k <- "historical"
for (m in hemisphere) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    print(m)
    for (l in startyearChoices_historical) {
      yearSpan <- paste0(l, "_", l + yearRange)
      yearnumberRange <- seq(l, (l + yearRange), 1)
      if (m %in% "SH") {
        yearnumberRange <- seq(l, (l + yearRangeSH), 1)
        yearSpan <- paste0(l, "_", l + yearRangeSH)
      }
      gc()
      x <- lapply(yearnumberRange, readRast_gs)
      r <- rast(x)
      index_r_start <-seq(1, nlyr(r), 2)
      index_r_end <-seq(2, nlyr(r), 2)
      r_start <- subset(r, index_r_start)
      r_end <- subset(r, index_r_end)
      fileNameOut_start <- paste0("data/cmip6/growingSeasons/growingSeason_start", m, "_", i, "_", k,  "_", yearSpan, ".tif")
      fileNameOut_end <- paste0("data/cmip6/growingSeasons/growingSeason_end", m, "_", i, "_", k,  "_", yearSpan, ".tif")
      print(paste0("fileNameOut_start out: ", fileNameOut_start))
      print(paste0("fileNameOut_end out: ", fileNameOut_end))
      print(system.time(writeRaster(r_start, fileNameOut_start, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
      print(system.time(writeRaster(r_end, fileNameOut_end, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
      r_start <- r_end <- NULL
    }
  }
}

# ensemble means -----
# combine all the spatrasters by model for the hemisphere, time period, start or end of season and scenario and then take the mean across that combo

for (k in scenarioChoicesEnsemble) {
  for (m in hemisphere)
    for (l in startyearChoices) {
      for (n in c("start", "end")) {
        print(m)
        yearSpan <- paste0(l, "_", l + yearRange)
        yearnumberRange <- seq(l, (l + yearRange), 1)
        if (m %in% "SH") {
          yearnumberRange <- seq(l, (l + yearRangeSH), 1)
          yearSpan <- paste0(l, "_", l + yearRangeSH)
        }
        if (k %in% "historical" & m %in% "NH") yearSpan <- "1991_2010"
        if (k %in% "historical" & m %in% "SH") yearSpan <- "1991_2009"
        
        gc()
        x <- lapply(modelChoices, readRast_GrowSeasonEnsemble)
        r <- rast(x)
        fileName_out <- paste0("data/cmip6/growingSeasons/ensemble_growingSeason_", n, m, "_", k, "_", yearSpan, ".tif")
        system.time(r_mean <- app(r, fun = "mean", filename = fileName_out, overwrite = TRUE, woptList = woptList))
        plot(r_mean, 1, main = paste0("Growing Season ", n, " ", m, ", ", k, ", period ", yearSpan))
      }
    }
}

# ensemble, growing season days -----
for (k in scenarioChoicesEnsemble) {
  for (m in hemisphere)
    for (l in startyearChoices) {
      print(m)
      yearSpan <- paste0(l, "_", l + yearRange)
      yearnumberRange <- seq(l, (l + yearRange), 1)
      if (m %in% "SH") {
        yearnumberRange <- seq(l, (l + yearRangeSH), 1)
        yearSpan <- paste0(l, "_", l + yearRangeSH)
      }
      
      if (k %in% "historical" & m %in% "NH") yearSpan <- "1991_2010"
      if (k %in% "historical" & m %in% "SH") yearSpan <- "1991_2009"
      fileName_in_start <- paste0("data/cmip6/growingSeasons/ensemble_growingSeason_", "start", m, "_", k, "_", yearSpan, ".tif")
      fileName_in_end <- paste0("data/cmip6/growingSeasons/ensemble_growingSeason_", "end", m, "_", k, "_", yearSpan, ".tif")
      r_start <- rast(fileName_in_start)
      r_end <- rast(fileName_in_end)
      r_days <- r_end - r_start
      fileName_out_days <- paste0("data/cmip6/growingSeasons/ensemble_growingSeason_days", m, "_", k, "_", yearSpan, ".tif")
      print(paste0("fileName out: ", fileName_out))
      print(system.time(writeRaster(r_days, fileName_out_days, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
      
      plot(r_days, 1, main = paste0("Growing Season Days ", m, ", ", k, ", period ", yearSpan))
    }
}

# mosaic NH and SH -----
#mosaicFileTypes <- c("frostCt", "extremeHtCt")
locofFilesToMosaic <- "data/cmip6/"

#for (i in mosaicFileTypes) {
for (k in scenarioChoicesEnsemble) {
  for (l in startyearChoices) {
    yearRange <- 19
    yearSpan <- paste0(l, "_", l + yearRange)
    yearSpanSH <- paste0(l, "_", l + yearRangeSH)
    if (k %in% "historical") yearSpan <- "1991_2010"
    if (k %in% "historical") yearSpanSH <- "1991_2009"
    NH <- paste0(locofFilesToMosaic, "growingSeasons", "/ensemble_growingSeason", "NH", "_", k, "_", yearSpan, ".tif")
    print(NH)
    NH <- raster(NH) # read in as raster to use with mosaic
    SH <- paste0(locofFilesToMosaic, "growingSeasons", "/ensemble_growingSeason", "SH", "_", k, "_", yearSpanSH, ".tif")
    print(SH)
    SH <- raster(SH)
    SHdiff <- SH[[2]] - SH[[1]]
    test <- mosaic(NH, SH, tolerance=0.05, fun = "mean", filename="")
    titleText <- paste0("Growing Season Start ", k, ", period ", yearSpan)
    
    plot(test, 1, main = titleText)
  }
  #}
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



# # fix growing season file names
# filelist.in <- list.files(paste0(getwd(), "/", "data/cmip6/growingSeasons"), full.names = TRUE)
# filelist.in <- gsub("//", "/", filelist.in)
# for (i in filelist.in) {
#   fileName_in <- i
#   print(i)
#   fileName_out <- gsub("growSeason", "growingSeason", i)
#   print(fileName_out)
#   file.rename(fileName_in, fileName_out)
# }




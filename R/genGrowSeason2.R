# calculates periods where the temperature is above 0 for at least minimumGrwSeasonLength days
{
    # source("R/globallyUsed.R")
  locOfFiles <- "data/bigFiles/"
  
  sspChoices <- c("ssp126", "ssp585") 
  #sspChoices <- c("ssp585") 
  options(warn=0) # convert warnings to errors
    
  # constants
  tminExtremeVal <- -30
  minimumGrwSeasonLength = 100
  seqLengthCode <- paste0("1{", minimumGrwSeasonLength, ",}")
  
  #test values
  i <- "IPSL-CM6A-LR"
  k <- "ssp585"
  l <- 2041
  yearNumber <- 2043
  m = "SH"
  
  f_readRast_gs <- function(yearNumber) {
    fileName_in <- paste0("data/cmip6/growingSeasons/growingSeason_", m, "_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
    r <- rast(fileName_in)
  }
  
  f_readRast_GrowSeasonEnsemble <- function(i) {
    fileName_in <- paste0("data/cmip6/growingSeasons/growingSeason_", n, m, "_", i, "_", k,  "_", yearSpan, ".tif") # note yearSpan here
    print(fileName_in)
    r <- rast(fileName_in)
  }
  
  f_growSeason <- function(cellVector) {
    startend <- c(NA, NA) 
    if (is.nan(cellVector[1])) {
      return(startend)
    }
    g <- gregexpr(seqLengthCode, paste(+(cellVector > 0), collapse = ""))[[1]]
    if (!g[[1]] == -1) { # no need to write to growing season if g returns 1
      startend[1] <- g[[1]] # the starting day of the sequence
      matchLength <- as.numeric(attributes(g)$match.length) # the number of days in the sequence of days that match at least the minimum length required
      startend[2] <- startend[1] + as.numeric(matchLength) - 1
    }
    return(startend) 
  }
  
  f_extremeCold <- function() {
    fileName_tasmin <- paste0(locOfFiles, "ensembleMn_dailyMn_20Yr_", k, "_tasmin_", yearSpan, ".tif")
    tmin <- rast(fileName_tasmin)
    print(system.time(extremeColdCt <- sum(tmin < tminExtremeVal, na.rm = TRUE)))
    fileNameStart <- paste0("extremeColdMinus", gsub("-", "", tminExtremeVal))
    fileName_out <- paste0("data/cmip6/growingSeasons/", fileNameStart, "_", k, "_", yearSpan, ".tif")
    print(paste0("fileName_out: ", fileName_out))
    print(extremeColdCt)
    print(system.time(writeRaster(extremeColdCt, fileName_out, overwrite = TRUE, wopt = woptList)))
  }
}


f_growSeasonStartEnd <- function() {
  yearnumberRange <- seq(l, (l + yearRange), 1)
  if (hem == "SH") {
    yearnumberRange <- seq(l, (l + yearRangeSH), 1)
    yearSpan <- paste0(l, "_", l + yearRangeSH)
  }
  x <- lapply(yearnumberRange, f_readRast_gs)
  x_start <- x
  r <- rast(x)
  index_r_start <-seq(1, nlyr(r), 2)
  index_r_end <-seq(2, nlyr(r), 2)
  r_start <- subset(r, index_r_start)
  r_end <- subset(r, index_r_end)
  fileName_out_start <- paste0("data/cmip6/growingSeasons/growingSeason_start", m, "_", i, "_", k,  "_", yearSpan, ".tif")
  fileName_out_end <- paste0("data/cmip6/growingSeasons/growingSeason_end", m, "_", i, "_", k,  "_", yearSpan, ".tif")
  print(paste0("fileName_out_start out: ", fileName_out_start))
  print(paste0("fileName_out_end out: ", fileName_out_end))
  print(system.time(writeRaster(r_start, fileName_out_start, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
  print(system.time(writeRaster(r_end, fileName_out_end, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
  r_start <- r_end <- NULL
}

# 20 year growing seasons  ------
for (k in sspChoices) {
  for (hem in hemispheres)
    for (i in modelChoices) {
      modelName.lower <- tolower(i)
      for (l in startYearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        yearnumberRange <- seq(l, (l + yearRange), 1)
        print(hem)
        fileName_in <- paste0("data/cmip6/runs/", modelName.lower, "_tasmin_", m, "_summer_", k, "_", yearSpan, ".tif")
        tmin <- rast(fileName_in)
        
        for (yearnum in yearnumberRange)
        if (hem =="SH") {
          startDate <-  paste0(yearnum, "-07-01"); endDate <- paste0(yearnum + 1, "-06-30")
        indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
        }
        print(system.time(growingSeason <- tapp(tmin_yr, f_growSeason)))
        names(growingSeason) <- c("growingSeason_start", "growingSeason_end")
        
        fileName_out <- paste0("data/cmip6/growingSeasons/growingSeasonNH_",  i, "_", k, "_", yearNumber, ".tif")
        writeRaster(growingSeasonNH, fileName_out, overwrite = TRUE, wopt = woptList)
        gc()
      }
    }
}

# 20 year historical, both  -----
k <- "historical"
for (hem in hemispheres) {
  for (i in modelChoices) {
    modelName.lower <- tolower(i)
    print(hem)
    for (l in startYearChoices_historical) {
      yearSpan <- paste0(l, "_", l + yearRange)
      yearnumberRange <- seq(l, (l + yearRange), 1)
      if (hem == "SH") {
        yearnumberRange <- seq(l, (l + yearRangeSH), 1)
        yearSpan <- paste0(l, "_", l + yearRangeSH)
      }
      gc()
      x <- lapply(yearnumberRange, f_readRast_gs)
      r <- rast(x)
      index_r_start <-seq(1, nlyr(r), 2)
      index_r_end <-seq(2, nlyr(r), 2)
      r_start <- subset(r, index_r_start)
      r_end <- subset(r, index_r_end)
      fileName_out_start <- paste0("data/cmip6/growingSeasons/growingSeason_start", m, "_", i, "_", k,  "_", yearSpan, ".tif")
      fileName_out_end <- paste0("data/cmip6/growingSeasons/growingSeason_end", m, "_", i, "_", k,  "_", yearSpan, ".tif")
      print(paste0("fileName_out_start out: ", fileName_out_start))
      print(paste0("fileName_out_end out: ", fileName_out_end))
      print(system.time(writeRaster(r_start, fileName_out_start, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
      print(system.time(writeRaster(r_end, fileName_out_end, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW")))); flush.console()
      r_start <- r_end <- NULL
    }
  }
}

#extreme cold, scenarios -----
for (k in sspChoices) {
  for (i in modelChoices) {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      f_extremeCold()
    }
  }
}

#extreme cold, historical -----
k = "historical"
l = 1991
yearSpan <- paste0(l, "_", l + yearRange)
f_extremeCold()

# ensemble means -----
# combine all the spatrasters by model for the hemisphere, time period, start or end of season and scenario and then take the mean across that combo

for (k in scenarioChoicesEnsemble) {
  for (hem in hemispheres) {
    for (l in startYearChoices) {
      for (n in c("start", "end")) {
        print(hem)
        yearSpan <- paste0(l, "_", l + yearRange)
        yearnumberRange <- seq(l, (l + yearRange), 1)
        if (hem == "SH") {
          yearnumberRange <- seq(l, (l + yearRangeSH), 1)
          yearSpan <- paste0(l, "_", l + yearRangeSH)
        }
        if (k %in% "historical" & hem == "NH") yearSpan <- "1991_2010"
        if (k %in% "historical" & hem == "SH") yearSpan <- "1991_2009"
        
        gc()
        x <- lapply(modelChoices, f_readRast_GrowSeasonEnsemble)
        r <- rast(x)
        fileName_out <- paste0("data/cmip6/growingSeasons/ensemble_growingSeason_", n, m, "_", k, "_", yearSpan, ".tif")
        system.time(r_mean <- app(r, fun = "mean", filename = fileName_out, overwrite = TRUE, wopt = woptList))
        plot(r_mean, 1, main = paste0("Growing Season ", n, " ", m, ", ", k, ", period ", yearSpan))
      }
    }
  }
}

# ensemble, growing season days -----
for (k in scenarioChoicesEnsemble) {
  for (hem in hemispheres)
    for (l in startYearChoices) {
      print(hem)
      yearSpan <- paste0(l, "_", l + yearRange)
      yearnumberRange <- seq(l, (l + yearRange), 1)
      if (hem == "SH") {
        yearnumberRange <- seq(l, (l + yearRangeSH), 1)
        yearSpan <- paste0(l, "_", l + yearRangeSH)
      }
      
      if (k %in% "historical" & hem == "NH") yearSpan <- "1991_2010"
      if (k %in% "historical" & hem == "SH") yearSpan <- "1991_2009"
      fileName_in_start <- paste0("data/cmip6/growingSeasons/ensemble_growingSeason_", "start", m, "_", k, "_", yearSpan, ".tif")
      fileName_in_end <- paste0("data/cmip6/growingSeasons/ensemble_growingSeason_", "end", m, "_", k, "_", yearSpan, ".tif")
      r_start <- rast(fileName_in_start)
      r_end <- rast(fileName_in_end)
      r_days <- r_end - r_start
      fileName_out_days <- paste0("data/cmip6/growingSeasons/ensemble_growingSeason_days", m, "_", k, "_", yearSpan, ".tif")
      print(paste0("fileName out: ", fileName_out))
      print(system.time(writeRaster(r_days, fileName_out_days, overwrite=TRUE, wopt = woptList))); flush.console()
      
      plot(r_days, 1, main = paste0("Growing Season Days ", m, ", ", k, ", period ", yearSpan))
    }
}

# mosaic NH and SH -----
#mosaicFileTypes <- c("frostCt", "extremeHtCt")
locofFilesToMosaic <- "data/cmip6/"

#for (i in mosaicFileTypes) {
for (k in scenarioChoicesEnsemble) {
  for (l in startYearChoices) {
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

# graphing gs start and end -----
#  do graphics -----
library(ggplot2)
library(RColorBrewer)
library(rworldmap)
library(maps)
#remotes::install_github("ropensci/rnaturalearthhires") need to do once to get the library from github
library(rnaturalearthhires)
library(ggspatial)
library(data.table)
library(readxl)
library(sf)
library(dplyr)


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
outFilename <- paste0(lofOfGraphicsFiles, "growingSeasons/gs", "_", l, "SH",".png")
png(filename = outFilename, width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

# graph extreme cold days with 0-1 -----

f_graphExtremDays <- function() {
  fileNameStart <- paste0("extremeColdMinus", gsub("-", "", tminExtremeVal))
  fileName_in <- paste0("data/cmip6/growingSeasons/", fileNameStart, "_", k, "_", yearSpan, ".tif")
  
  r <- rast(fileName_in)
  r <- crop(r, ext_noAntarctica)
  r <- project(r, crsRob)
  
  r_df <- as.data.frame(r, xy = TRUE)
  names(r_df) <- c("x", "y", "value")
  titleText <- paste0("Number of days below ", tminExtremeVal, "°C, \nscenario:  ", k,  ", year span: ", gsub("_", "-", yearSpan))
  legendTitle <- paste("days below ", tminExtremeVal)
  #        colorList <- (RColorBrewer::brewer.pal(2, "YlOrRd"))
  colorList <- c("white", "green")
  #    custom_bins = c(0, 1)
  g <- ggplot(data = coastline_cropped) +
    labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
    labs(x = "", y = "") +
    geom_raster(data = r_df, aes(x, y, fill = value), show.legend = TRUE) +
    scale_fill_gradientn(colours=c("white","green")) +
    geom_sf(color = "ghostwhite", lwd = 0.2) +
    theme(panel.background = element_rect(fill = "aliceblue"))
  #          theme(legend.text.align = 1) +
  #   theme(legend.position = "none")
  outFilename <- paste0(lofOfGraphicsFiles, "growingSeasons/extremeColdCt_", k, "_", yearSpan, ".png")
  
  ggsave(filename = outFilename, plot = g, width = 6, height = 6, units = "in", dpi = 300)
  print(paste0("out file name: ", outFilename))
  g <- NULL
}

for (k in sspChoices) {
  #  k = "ssp585"
  for (l in startYearChoices) {
    # l <- 2041
    yearSpan <- paste0(l, "_", l + yearRange)
    f_graphExtremDays()
  }
}


k = "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
f_graphExtremDays()

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

# new approach from Robert
threshold <- 60
n <- 100
f_ff <- function(x) {
  y <- x > threshold
  y[is.na(y)] <- FALSE
  a <- ave(y, cumsum(!y), FUN=cumsum)
  m <- max(a)
  if (m < n) return (c(NA, NA))
  i <- which(a == m)[1]
  c(i-m+1, i)
}



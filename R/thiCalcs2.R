# calculate animal climate stress
#source("R/globallyUsed.R")
library("terra")
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else {## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
if (get_os() %in% "osx") {
  terraOptions(memfrac = .9, progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path, memfrac = .9,  
}else{
  terraOptions(memfrac = .6,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
}

locOfFiles <- locOfCMIP6tifFiles
locOfFiles <- "data/bigFiles/"
speciesChoice <- c("humans", "cattle", "goat", "swine", "chicken", "sheep") 
#speciesChoice <- c("humans") 

sspChoices <- c("ssp126", "ssp585") 
sspChoices <- c("ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
# modelChoices <- c(  "GFDL-ESM4", "IPSL-CM6A-LR") #, "GFDL-ESM4") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2041, 2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices <-  c(2081) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_historical <- c(1991)
scenarioChoicesEnsemble <- c("historical", sspChoices)

yearRange <- 19
woptList <- list(gdal=c("COMPRESS=LZW"))

#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2081
yearNumber <- 2043
m <- "cattle"

readRast_thi_cattle <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/THI/thi.cattle_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  r <- rast(fileNameIn)
}

readRast_thi_Ensemble <- function(i) {
  fileNameIn <- paste0("data/cmip6/THI/thi.", m, "_", i, "_", k,  "_", yearSpan, ".tif") # note yearSpan here
  print(fileNameIn)
  r <- rast(fileNameIn)
  indices <- seq(from = 1, to = nlyr(r), 1)
  # startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
  # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
  indices <- paste0("X", as.character(indices))
  names(r) <- indices
  r
}

readRast_thi_sheep <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/THI/thi.sheep_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  print(fileNameIn)
  r <- rast(fileNameIn)
}
readRast_thi_goat <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/THI/thi.goat_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  print(fileNameIn)
  r <- rast(fileNameIn)
}
readRast_thi_chicken <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/THI/thi.chicken_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  print(fileNameIn)
  r <- rast(fileNameIn)
}

readRast_thi_swine <- function(yearNumber) {
  fileNameIn <- paste0("data/cmip6/THI/thi.swine_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
  print(fileNameIn)
  r <- rast(fileNameIn)
}


THIfun_cattle <- function(rh, tmax) {
  thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))
  thi[tmax < 20] <- 0
  thi <- round(thi, 2)
}

THIfun_sheep <- function(rh, tmax) {
  thi <- tmax - ((0.31 - (0.31 * (rh / 100))) * (tmax - 14.4)) 
  thi[tmax < 20] <- 0
  thi <- round(thi, 2)
}

THIfun_goat <- function(rh, tmax) {
  thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) # should this be the same as cattle?
  thi[tmax < 20] <- 0
  thi <- round(thi, 2)
}

THIfun_swine <- function(rh, tmax) {
  thi <- tmax - (0.55 - (0.0055 * rh) * (tmax - 14.5))
  thi[tmax < 20] <- 0
  thi <- round(thi, 2)
}

THIfun_chicken <- function(tmax, tmin) {
  thi <- (0.60 * tmax + 0.40 * tmin) # using broiler formula. Note. no rh needed
  thi[tmax < 20] <- 0
  thi[thi > 100] <- 100
  thi <- round(thi, 2)
}

THIfun_humans <-  function(rh, tave) {
  thi <- 100/(1+(((-12.28*log(rh)+87.99)/tave))^(-2.21*log(rh)+2.63))
  thi[thi > 100] <- 100
  thi <- round(thi, 2)
}
#THI scenarios -----
speciesChoice <- c("humans") #, "goat")
for (k in sspChoices) {
  for (l in startyearChoices) {
    for (i in modelChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      modelName.lower <- tolower(i)
      fileName_tasmax <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
      tmax <- rast(fileName_tasmax)
      fileName_rh <- paste0(locOfFiles, modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
      rh <- rast(fileName_rh)
      
      startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), 1)
      indices <- paste0("X", as.character(indices))
      indices_day <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
      indices_day <- as.numeric(indices_day)
      
      system.time(tmax.mean <- tapp(tmax, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      tmax <- NULL; gc()
      
      system.time(rh.mean <- tapp(rh, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      rh <- NULL; gc()
      comb <- sds(rh.mean, tmax.mean)
      for (m in speciesChoice) {
        
        fileName_out <- paste0("data/cmip6/THI/thi.", m, "_",  i, "_", k, "_", yearSpan, ".tif")
        print(paste0("fileName out: ", fileName_out))
        funName <- paste0("THIfun_", m)
        if (m %in% "chicken") {
          fileName_tasmin <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
          tmin <- rast(fileName_tasmin)
          system.time(tmin.mean <- tapp(tmin, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
          comb_chicken <- sds(tmax.mean, tmin.mean)
          print(system.time(r_out <- lapp(comb_chicken, THIfun_chicken, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
          if (m %in% "humans") {
            fileName_tas <- paste0(locOfFiles,  modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
            tas <- rast(fileName_tas)
            system.time(tas.mean <- tapp(tas, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
            comb_humans <- sds(tas.mean, tmin.mean)
            print(system.time(r_out <- lapp(comb_humans, THIfun_humans, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
          } else {
            
            print(system.time(r_out <- lapp(comb, fun = funName, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
            
            #         print(system.time(r_out <- lapp(comb, fun = funName, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
            print(paste0("app done"))
            r_out <- NULL
            #print(system.time(writeRaster(r_out, filename = fileName_out, overwrite=TRUE, wopt = woptList))); flush.console()
          }
        }
      }
    }
  }
}

#THI historical -----
k <- "historical"
speciesChoice <- c("humans") #, "goat")
for (l in startyearChoices_historical) {
  for (i in modelChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    modelName.lower <- tolower(i)
    fileName_tasmax <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
    tmax <- rast(fileName_tasmax)
    fileName_rh <- paste0(locOfFiles, modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
    rh <- rast(fileName_rh)
    
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices <- paste0("X", as.character(indices))
    indices_day <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
    indices_day <- as.numeric(indices_day)
    
    system.time(tmax.mean <- tapp(tmax, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
    tmax <- NULL; gc()
    
    system.time(rh.mean <- tapp(rh, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
    rh <- NULL; gc()
    comb <- sds(rh.mean, tmax.mean)
    for (m in speciesChoice) {
      
      fileName_out <- paste0("data/cmip6/THI/thi.", m, "_",  i, "_", k, "_", yearSpan, ".tif")
      print(paste0("fileName out: ", fileName_out))
      funName <- paste0("THIfun_", m)
      if (m %in% "chicken") {
        fileName_tasmin <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmin_global_daily_", yearSpan, ".tif")
        tmin <- rast(fileName_tasmin)
        system.time(tmin.mean <- tapp(tmin, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
        comb_chicken <- sds(tmax.mean, tmin.mean)
        print(system.time(r_out <- lapp(comb_chicken, THIfun_chicken, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
        if (m %in% "humans") {
          fileName_tas <- paste0(locOfFiles,  modelName.lower, "_", k, "_tas_global_daily_", yearSpan, ".tif")
          tas <- rast(fileName_tas)
          system.time(tas.mean <- tapp(tas, indices_day, fun = mean, na.rm = TRUE))#, filename = fileName_out, overwrite = TRUE, woptList = woptList))
          comb_humans <- sds(tas.mean, tmin.mean)
          print(system.time(r_out <- lapp(comb_humans, THIfun_humans, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
        } else {
          print(system.time(r_out <- lapp(comb, fun = funName))) #, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
          print(system.time((writeRaster(r_out, filename = fileName_out, overwrite =TRUE, wopt = woptList))))
          
          #         print(system.time(r_out <- lapp(comb, fun = funName, filename = fileName_out, overwrite =TRUE, wopt = woptList)))
          print(paste0("app done"))
          r_out <- NULL
          #print(system.time(writeRaster(r_out, filename = fileName_out, overwrite=TRUE, wopt = woptList))); flush.console()
        }
      }
    }
  }
}

# ensemble means -----
# combine all the spatrasters by model for the hemisphere, time period, start or end of season and scenario and then take the mean across that combo

for (k in scenarioChoicesEnsemble) {
  #  k <- "ssp585"
  for (startYear in startyearChoices) {
    l <- startYear
    yearSpan <- paste0(l, "_", l + yearRange)
    yearnumberRange <- seq(l, (l + yearRange), 1)
    if (k %in% "historical") {yearSpan <- "1991_2010"; l <- 1991}
    gc()
    
    # startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    # indices <- paste0("X", as.character(indices))
    # indices_day <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
    # indices_day <- as.numeric(indices_day)
     for (m in speciesChoice) {
   # for (m in "humans") {
      print(paste0("species Choice m: ", m))
      #     funName <- paste0("readRast_thi_", m, "Ensemble")
      
      x <- lapply(modelChoices, readRast_thi_Ensemble)
      indices_day <- seq(1, nlyr(x[[1]]), 1)
      r <- rast(x)
      print(r)
      #      fileName_out <- paste0("data/cmip6/THI/ensemble_thi.cattle_", k, "_", yearSpan, ".tif")
      fileName_out <- paste0("data/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
      videoName_out <- paste0("graphics/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".mp4")
      print(paste0("fileName out: ", fileName_out))
      #     x.cv <- tapp(x, indices_day, fun = cv, na.rm = TRUE)
      print(Sys.time())
      system.time(r.mean <- tapp(r, indices_day, fun = mean, na.rm = TRUE, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      # r.mean.10 <- subset(r.mean, 1:10)
      # r.mean.10 <- r.mean.10 *1
       r.mean_stack <- stack(r.mean)
       names(r.mean_stack) <- gsub("X", "Day ", names(r.mean_stack))
      par(usr = c(-150, 150, -90, 90))
      title_animate <- paste0("Heat Productivity Index, ", m, " ", k, ", period ", gsub("_", "-", yearSpan))
      dev.new(width=9, height=6, noRStudioGD = TRUE)
      system.time(animation::saveVideo(animate(r.mean_stack, n=1, pause = 0.05, sub = title_animate, 
                                               col = colorList, xlim = c(-180, 180), ylim = c(-60, 80)), videoName_out))
      print(Sys.time())
      title <- paste0("Ensemble mean, THI ", m, "," , k, ", period ", yearSpan, ", Jan 1.")
      plot(r.mean, 150, main = paste0("Ensemble mean, Heat Productivity Index, ", m, " ", k, ", period ", yearSpan, ", day 150"))
    }
  }
}

# days with max stress
library(data.table)
funextTHI <- function(cellVector) {
  if (m %in% "humans") {
    extremeCt <- sum(cellVector < extremeStress, na.rm = FALSE)
  } else {extremeCt <- sum(cellVector > extremeStress, na.rm = FALSE)
  }
  return(extremeCt) 
}

bpList <- as.data.table(readxl::read_excel("data-raw/animals/AnimalbreakpointslistRaw.xlsx"))
for (m in speciesChoice) {
  extremeStress_humans <- 50
  if (m %in% "humans") {extremeStress <- extremeStress_humans} else {
    extremeStress <- bpList[species %in% m, extremeStress]
  }
  for (k in scenarioChoicesEnsemble) {
    #for (k in "historical") {
    for (startYear in startyearChoices) {
      l <- startYear
      yearSpan <- paste0(l, "_", l + yearRange)
      yearnumberRange <- seq(l, (l + yearRange), 1)
      if (k %in% "historical") {yearSpan <- "1991_2010"; l <- 1991}
      
      fileName_in <- paste0("data/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
      r <- rast(fileName_in)
      print(system.time(extremeCt <- app(r, funextTHI)))
      fileName_out <- paste0("data/cmip6/THI/extremeCt.", m, "_", k, "_", yearSpan, ".tif")
      print(paste0("fileName out: ", fileName_out))
      writeRaster(extremeCt, fileName_out, overwrite = TRUE, woptList = woptList )
      title <- paste0(m, ", days above extreme stress value of ", extremeStress, ", ensemble means, ", k, ", ", yearSpan)
      plot(extremeCt, main = paste0(m, ", days above extreme stress value of ", extremeStress, ", ensemble means, ", k, ", ", yearSpan))
    }
  }
}

# now do some graphics
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
coastline <- st_read("data-raw/regionInformation/ne_50m_coastline/ne_50m_coastline.shp")
coastline <- st_transform(coastline, crsRob)

# test values
m = "cattle"
k = "ssp585"
yearSpan_begin <- "1991_2010"
yearSpan_end <- "2081_2100"

for (m in speciesChoice) {
  fileName_century_end <- paste0("data/cmip6/THI/extremeCt.", m, "_", k, "_", yearSpan_end, ".tif")
  fileName_century_begin <- paste0("data/cmip6/THI/extremeCt.", m, "_", "historical", "_", yearSpan_begin, ".tif")
  fileName_r_mask <- paste0("data/animals/raster_ct_", m, ".tif")
  r_mask <- rast(fileName_r_mask)
  r_end <- rast(fileName_century_end)
  r_begin <- rast(fileName_century_begin)
  r_delta <- r_end - r_begin
  
  names(r_end) <- names(r_begin) <- names(r_delta) <- "value"
  if (m %in% "cattle") maskMin <- 2000
#  if (m %in% "humans") maskMin <- 10 commented out to see if that affects the middle east values
    if (m %in% "humans") maskMin <- 1 
  if (m %in% "swine") maskMin <- 10
  if (m %in% "chicken") maskMin <- 2000
  if (m %in% "goat") maskMin <- 1000
  r_mask[r_mask < maskMin] <- NA
  r_mask[r_mask > 0] <- 1
  r_end <- mask(r_end, r_mask)
  r_begin <- mask(r_begin, r_mask)
  r_delta <- mask(r_delta, r_mask)
  # convert to Robinson projection
  
  r_end <- project(r_end, crsRob)
  r_begin <- project(r_begin, crsRob)
  r_delta <- project(r_delta, crsRob)
  
  # convert to data frame to use with ggplot
  r_end_df <- as.data.frame(r_end, xy = TRUE)
  r_begin_df <- as.data.frame(r_begin, xy = TRUE)
  r_delta_df <- as.data.frame(r_delta, xy = TRUE)
  
  titleText_end <- paste0(m, ", days above extreme stress value, ", "SSP585, ", yearSpan_end)
  titleText_begin <- paste0(m, ", days above extreme stress value, ", yearSpan_begin)
  titleText_delta <- paste0(m, ", change in days above extreme stress value, \n early to end century ", "SSP585 ")
  
  legendTitle <- "Days"
  for (i in c("r_begin_df", "r_end_df", "r_delta_df")){
    if (i %in% "r_end_df") {titleText <- titleText_end}
    if (i %in% "r_begin_df") {titleText <- titleText_begin}
    if (i %in% "r_delta_df") {titleText <- titleText_delta}
    gc()
    colorList <- (RColorBrewer::brewer.pal(5, "YlOrRd"))
    maxVal <- 250
    custom_bins <- round(seq.int(from = 0, to = maxVal, length = 6))
    r <- get(i)
    r$value[r$value > maxVal] <- maxVal
    g <- ggplot(data = coastline) +
      labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
      labs(x = "", y = "") +
      
      geom_raster(data = r, aes(x, y, fill = value)) +
      scale_fill_gradientn(colours = colorList, na.value = "white",
                           breaks = custom_bins,labels = custom_bins,
                           limits = c(0, maxVal)) + 
      geom_sf(fill = NA, color = "gray", lwd = 0.3,)
    print(g)
    if (i %in% "r_end_df") {outFilename <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", k, "_", yearSpan_begin, ".png")}
    if (i %in% "r_begin_df") {outFilename <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", k, "_", yearSpan_end, ".png")}
    if (i %in% "r_delta_df") {outFilename <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", k, "_", yearSpan_end, ".png")}
    png(filename = outFilename, width = 6, height = 6, units = "in", res = 300)
    print(g)
    dev.off()}
}

# graph deltas

for (m in speciesChoice) {
  fileName_century_end <- paste0("data/cmip6/THI/extremeCt.", m, "_", k, "_", yearSpan_end, ".tif")
  fileName_century_begin <- paste0("data/cmip6/THI/extremeCt.", m, "_", "historical", "_", yearSpan_begin, ".tif")
  
  r_end <- rast(fileName_century_end)
  r_begin <- rast(fileName_century_begin)
  r_delta <- r_end - r_begin
  r_end <- project(r_end, crsRob)
  r_begin <- project(r_begin, crsRob)
  r_delta <- project(r_delta, crsRob)
  
  r_delta_df <- as.data.frame(r_delta, xy = TRUE)
  names(r_delta_df)[names(r_delta_df) == "lyr.1"] <- "value"
  
  titleText <- paste0(m, ", change in days above extreme stress value, \n early to end century ", "SSP585 ")
  
  legendTitle <- "Change in \nHigh Stress Days"
  print(titleText)
  gc()
  colorList <- (RColorBrewer::brewer.pal(5, "YlOrRd"))
  maxVal <- 250
  custom_bins <- round(seq.int(from = 0, to = maxVal, length = 6))
  r <- r_delta_df
  r$value[r$value > maxVal] <- maxVal
  g <- ggplot(data = coastline) +
    labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
    labs(x = "", y = "") +
    
    geom_raster(data = r, aes(x, y, fill = value)) +
    scale_fill_gradientn(colours = colorList, na.value = "white",
                         breaks = custom_bins,labels = custom_bins,
                         limits = c(0, maxVal)) + 
    geom_sf(fill = NA, color = "gray")
  g
  outFilename <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", k, "_", ".png")
  png(filename = outFilename, width = 6, height = 6, units = "in", res = 300)
  print(g)
  dev.off()
}

# get animal raster masks
animalsOnly <- speciesChoice[!speciesChoice %in% "humans"]
for (m in animalsOnly) {
  if (m %in% "cattle") cutoff = 2000
  fileName_r_mask <- paste0("data/animals/raster_ct_", m, ".tif")
  r_mask <- rast(fileName_r_mask)
  values(r_mask)[values(r_mask) < 2000] <- NA
  r_delta_mask <- mask(r_delta, r_mask)
}
r_delta_mask <- mask(r_delta, pop_mask)
pop_mask
#plot data with masking

for (m in speciesChoice) {
  fileName_century_end <- paste0("data/cmip6/THI/extremeCt.", m, "_", k, "_", yearSpan_end, ".tif")
  fileName_century_begin <- paste0("data/cmip6/THI/extremeCt.", m, "_", "historical", "_", yearSpan_begin, ".tif")
  r_raster <- 
  r_end <- rast(fileName_century_end)
  r_begin <- rast(fileName_century_begin)
  r_delta <- r_end - r_begin
  
  r_delta_df <- as.data.frame(r_delta_mask, xy = TRUE)
  names(r_delta_df)[names(r_delta_df) == "lyr.1"] <- "value"
  
  titleText <- paste0(m, ", change in days above extreme stress value, \n early to end century ", "SSP585 ")
  
  legendTitle <- "Change in \nHigh Stress Days"
  print(titleText)
  gc()
  colorList <- (RColorBrewer::brewer.pal(5, "YlOrRd"))
  maxVal <- 250
  custom_bins <- round(seq.int(from = 0, to = maxVal, length = 6))
  r <- r_delta_df
  r$value[r$value > maxVal] <- maxVal
  g <- ggplot(data = coastline) +
    labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
    labs(x = "", y = "") +
    
    geom_raster(data = r, aes(x, y, fill = value)) +
    scale_fill_gradientn(colours = colorList, na.value = "white",
                         breaks = custom_bins,labels = custom_bins,
                         limits = c(0, maxVal)) + 
    geom_sf(fill = NA, color = "gray")
  g
  outFilename <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", k, ".png")
  png(filename = outFilename, width = 6, height = 6, units = "in", res = 300)
  print(g)
  dev.off()
}

# convert population density raster
pop_mask <- rast("data-raw/gpw_v4_population_density_rev11_2020_30_min.tif")
setMinMax(pop_mask)
fileName_r_mask <- paste0("data/animals/raster_ct_", "humans", ".tif")
writeRaster(pop_mask, fileName_r_mask, overwrite = TRUE, woptList = woptList)

maskValUpper <- 3000
maskValLower <- 10
pop_mask > maskValUpper
pop_mask[pop_mask > maskValUpper] <- NA
pop_mask[pop_mask < maskValLower] <- NA
pop_mask[pop_mask > 0] <- 1

fileName_in <- "data/cmip6/THI/ensemble_thi.humans_ssp585_2081_2100.tif"
test <- rast(fileName_in)
test
test_brick <- raster::brick(fileName_in)
test_brick
test_anim <- raster::animate(test_brick, 1)

{# calculate animal and human climate stress
  source("R/globallyUsed.R")
  library("terra")
  library("crayon")
  # library(data.table)
  
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
    terraOptions(memfrac = 2, progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path, memfrac = .9,
  }else{
    terraOptions(memfrac = .6,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
  }
  
  woptList <- list(gdal=c("COMPRESS=LZW"))
  woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))
  
  locOfFiles <- "data/bigFiles/"
  speciesChoice <- c("humans", "cattle", "goat", "swine", "chicken", "sheep") 
  #speciesChoice <- c("cattle")
  sspChoices <- c("ssp126", "ssp585") 
  #sspChoices <- c("ssp585") 
  modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") 
  #modelChoices <- c(  "IPSL-CM6A-LR") 
  startYearChoices <-  c(2041, 2081) 
  startYearChoices_historical <- c(1991)
  scenarioChoicesEnsemble <- c("historical", sspChoices)
  extremeStress_humans <- 60
  animalCts_global <- as.data.table(read.csv("/Volumes/ExtremeSSD/data/animals/animalCt.csv"))
  
  resultsStorage <- data.table::data.table(species = character(), scenario = character(), startYear = numeric(), model = character(), ct_small= numeric(), ct_large = numeric())
  
  bpList <- data.table::as.data.table(readxl::read_excel("data-raw/animals/AnimalbreakpointslistRaw.xlsx"))
  
  ext_noAntarctica <- ext(-180, 180, -60, 90)
  
  yearRange <- 19
  # colorList <- (RColorBrewer::brewer.pal(5, "RdYlGn"))
  colorList <- (RColorBrewer::brewer.pal(5, "YlOrRd")) # yellow to red
  
  locOfDataFiles <- "data/cmip6/THI/"
  #test values
  i <- "GFDL-ESM4"
  k <- "ssp585"
  l <- 2081
  yearNumber <- 2043
  m <- "humans"
  
  f_getExtremeStressValue <- function(m) {
    if (m == "humans") {extremeStress <- extremeStress_humans
    } else {
      extremeStress <- bpList[species %in% m, extremeStress]
    }
    return(extremeStress)
  }
  
  f_readRast_thi_ensemble <- function(i, m, k) {
    fileName_in <- paste0(locOfDataFiles, "thi.", m, "_", i, "_", k,  "_", yearSpan, ".tif") # note *yearSpan* here
    print(paste0("m: ", m, ", k: ", k, ", model(i): ", i, ", fileName in: ", fileName_in))
    r <- rast(fileName_in)
    indices <- seq(from = 1, to = nlyr(r), 1)
    indices <- paste0("X", as.character(indices))
    names(r) <- indices
    r
  }
  
  f_THI_cattle <- function(rh, tmax) {
    thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))
    thi[tmax < 20] <- 0
    thi <- round(thi, 3)
  }
  
  f_THI_goat <- function(rh, tmax) {
    thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) # should this be the same as cattle? Apparently.
    thi[tmax < 20] <- 0
    thi <- round(thi, 3)
  }
  
  f_THI_sheep <- function(rh, tmax) {
    thi <- tmax - ((0.31 - (0.31 * (rh / 100))) * (tmax - 14.4)) 
    thi[tmax < 20] <- 0
    thi <- round(thi, 3)
  }
  
  f_THI_swine <- function(rh, tmax) {
    thi <- tmax - (0.55 - (0.0055 * rh) * (tmax - 14.5))
    thi[tmax < 20] <- 0
    thi <- round(thi, 3)
  }
  
  f_THI_chicken <- function(tmax, wbulb) {
    thi <- (0.85 * tmax + 0.15 * wbulb) # using broiler formula. Note. no rh needed
    thi[tmax < 20] <- 0
    #  thi[thi > 100] <- 100
    thi <- round(thi, 3)
  }
  
  # formula from Stull, R. (2011). Wet-Bulb Temperature from Relative Humidity and Air Temperature. J. Appl. Meteorol. Climatol. 50, 2267–2269. doi:10.1175/JAMC-D-11-0143.1.
  f_wetbulb <- function(rh, tas) {
    wb <- tas * atan(0.151977* (rh + 8.313659)^(1/2)) + atan(tas + rh) - atan(rh - 1.676331) +
      0.00391838 * (rh)^ (3/2) * atan(0.023101 * rh) - 4.686035
    return(wb)
  }
  
  f_THI_humans <-  function(rh, tas) {
    pwc <- 100/(1 + (((-12.28 * log(rh) + 87.99)/tas))^(-2.21 * log(rh) + 2.63))
    #  pwc[pwc > 100] <- 100 #Not needed because pwc is never above 100
    pwc[tas < 15] <- 100
    pwc
  }
  
  f_resultsStorage <- function(r_out, extremeStress) {
    ct_small <- sum(r_out < extremeStress, na.rm = FALSE) # count of days at each location where r_out is less than extreme stress
    ct_large <- sum(r_out >= extremeStress, na.rm = FALSE) # count of days at each location where r_out is greater than or equal to extreme stress
    ct_small_sum <- global(ct_small, sum, na.rm = TRUE) # total number of days below extreme stress
    ct_large_sum <- global(ct_large, sum, na.rm = TRUE) # total number of days at or above extreme stress
    ratio <- ct_small_sum/ct_large_sum
    results <- c(m, k, l, i, ct_small_sum, ct_large_sum)
  }
  
  f_THI <- function(k, l, yearRange) {
    # k, l, i, yearRange are set outside the function, in THI scenarios and THI historical
    yearSpan <- paste0(l, "_", l + yearRange)
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices <- paste0("X", as.character(indices))
    indices_day <- format(as.Date(indices, format = "X%Y-%m-%d"), format = "%j") # %j is day of the year
    indices_day <- as.numeric(indices_day)
    
    for (i in modelChoices) {
      modelName.lower <- tolower(i)
      fileName_rh_mean <- paste0(locOfFiles, "dailyMn_20yr_", modelName.lower, "_", k, "_hurs_", yearSpan, ".tif")
      fileName_tmax_mean <- paste0(locOfFiles,  "dailyMn_20yr_", modelName.lower, "_", k, "_tasmax_", yearSpan, ".tif")
      fileName_tmin_mean <- paste0(locOfFiles,  "dailyMn_20yr_", modelName.lower, "_", k, "_tasmin_", yearSpan, ".tif")
      fileName_tas_mean <- paste0(locOfFiles,  "dailyMn_20yr_", modelName.lower, "_", k, "_tas_", yearSpan, ".tif")
      
      rh.mean <- rast(fileName_rh_mean)
      cat(red(paste0("rh.mean min: ", round(min(minmax(rh.mean)), 2), ", rh.mean max: ", round(max(minmax(rh.mean)), 2), ", rh file: ", fileName_rh_mean, "\n")))
      tmax.mean <- rast(fileName_tmax_mean)
      cat(red(paste0("tmax.mean min: ", round(min(minmax(tmax.mean)), 2), ", tmax.mean max: ", round(max(minmax(tmax.mean)), 2), ", tmax file: ", fileName_tmax_mean, "\n")))
      tmin.mean <- rast(fileName_tmin_mean)
      cat(red(paste0("tmin.mean min: ", round(min(minmax(tmin.mean)), 2), ", tmin.mean max: ", round(max(minmax(tmin.mean)), 2), ", tmin file: ", fileName_tmin_mean, "\n")))
      tas.mean <- rast(fileName_tas_mean)
      cat(red(paste0("tas.mean min: ", round(min(minmax(tas.mean)), 2), ", tas.mean max: ", round(max(minmax(tas.mean)), 2), ", tmax file: ", fileName_tmax_mean, "\n")))
      
      if ((max(minmax(tas.mean)) | max(minmax(tmin.mean))) > max(minmax(tmax.mean))) stop("something is wrong!!")
      
      comb <- sds(rh.mean, tmax.mean) # the combo used for most animals
      for (m in speciesChoice) {
        extremeStress <- f_getExtremeStressValue(m) 
        
        fileName_out <- paste0(locOfDataFiles, "thi.", m, "_",  i, "_", k, "_", yearSpan, ".tif")
        print(paste0("fileName out: ", fileName_out))
        funName <- paste0("f_THI_", m)
        
        # now do species-specific crunching.
        if (m == "chicken") {
          comb_wb <- sds(rh.mean, tas.mean) # needed to generate wet bulb input data
          system.time(wbulb <- lapp(comb_wb, fun = f_wetbulb))
          comb_chicken <- sds(tas.mean, wbulb)
          print(system.time(r_out <- lapp(comb_chicken, f_THI_chicken, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
        }
        if (m == "humans") {
          tas.mean <- rast(fileName_tas_mean)
          comb_humans <- sds(rh.mean, tas.mean)
          print(system.time(r_out <- lapp(comb_humans, fun = funName, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
          col = rev(colorList)
        } 
        if (m %in% c("sheep", "swine", "cattle", "goat")) {
          col = colorList
          print(system.time(r_out <- lapp(comb, fun = funName, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
        } 
        gc()
        
        resultsStorage <- data.table::rbindlist(list(resultsStorage, f_resultsStorage(r_out, extremeStress)), use.names = FALSE)
        
        # display results from each species
        print(paste0("--------", m, "--------"))
        print(r_out)
        cat(red(paste0("species: ", m, ", ssp: ", k, ", start year: ", l, ", model: ", i, "\n\n"))) #, ", ratio of small to large: ", round(ratio, 2)), "\n\n"))
        cat(paste0(red(m, ", THI min: ", round(min(minmax(r_out)), 2), ",  THImax: ", round(max(minmax(r_out)), 2), ", THI out file: ", fileName_out), "\n\n" ))
        breaks = c(bpList[species %in% m, zeroLevel], bpList[species %in% m, noStress], bpList[species %in% m, moderateStress],bpList[species %in% m, extremeStress], max(minmax(r_out)))
        if (m == "humans") {
          breaks = c(0, 25, 50, 75, 100)
        }
        breaks <- round(breaks, 0)
        
        plot(r_out, 1, main = paste0(m, ", ", i, ", ", k, ", ", l, ", Jan 1, extremeStress: ", extremeStress), ylim = c(-60, 90), range = c(0, 100), col = col, breaks = breaks, axes = FALSE) #breaks = c(0, 40, 60, 80, 100)
      }
    }
  }
  
  
  f_extremeTHI <- function(cellVector, extremeStress, m) {
    #    print(paste0("species: ", m, ", extreme stress level: ", extremeStress))
    if (m == "humans") {
      extremeCt <- sum(cellVector <= extremeStress, na.rm = FALSE)
    } else {extremeCt <- sum(cellVector >= extremeStress, na.rm = FALSE)
    }
    return(extremeCt) 
  }
  
  f_extremeStress_setup <- function(k, l, colorList) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (m in speciesChoice) {
      extremeStress <- f_getExtremeStressValue(m) 
      fileName_in <- paste0(locOfDataFiles, "ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
      r <- rast(fileName_in)
      maxVal <- round(max(minmax(r)), 3)
      minVal <- round(min(minmax(r)), 3)
      print(paste0("species: ", m, ", minVal: ", minVal, ", maxVal: ", maxVal, ", extreme stress: ", extremeStress, ", fileName in: ", fileName_in))
      print(system.time(extremeCt <- app(r, fun = f_extremeTHI, extremeStress, m)))
      fileName_out <- paste0(locOfDataFiles, "extremeCt.", m, "_", k, "_", yearSpan, ".tif")
      print(paste0("fileName in: ", fileName_in))
      print(paste0("fileName out: ", fileName_out))
      writeRaster(extremeCt, fileName_out, overwrite = TRUE, woptList = woptList )
      title <- paste0(m, ", days above extreme stress value of ", extremeStress, ", \nensemble means, ", k, ", ", yearSpan)
      col = colorList
      if (m == "humans") {
        col <- colorList
        title <- paste0(m, ", days where PWC is less than ", extremeStress, " percent, \nensemble means, ", k, ", ", yearSpan)
      }
      breaks = seq(from = 1, to = max(minmax(extremeCt)), length.out = 5)
      breaks = seq(from = 1, to = 366, length.out = 5)
      breaks <- round(breaks, 0)
      breaks <- c(0, breaks) # add to give 0 - 1 a color
      print(paste0("breaks: ", breaks))
      print(r)
      print(extremeCt)
      print(paste0("extreme level: ", extremeStress))
      plot(extremeCt, 1, main = title, ylim = c(-60, 90), range = c(0, 100), col = col, breaks = breaks, axes = FALSE, colNA = "gray")
      r <- NULL # not clear why this would be needed but just in case
      print("-----------------------------------")
    }
  }
  
  f_ensemble_THI <- function(m, k, l, yearSpan) {
    # cattle and goats have the same 
    for (m in speciesChoice) {
      print(paste0("------", "------"))
      extremeStress <- f_getExtremeStressValue(m) 
      print(paste0("species choice m: ", m))
      x <- lapply(modelChoices, f_readRast_thi_ensemble, m = m, k = k)
      r <- rast(x)
      indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
      maxVal <- round(max(minmax(r)), 2)
      minVal <- round(min(minmax(r)), 2)
      #     print(r)
      fileName_out <- paste0(locOfDataFiles, "ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
      cat(paste0(red("species: ", m, ", ensemble ssp: ", k, ", start year: ", l, ", minVal ", minVal,  ", maxVal ", maxVal,"extreme stress: ", extremeStress , ", fileName out: ", fileName_out), "\n\n"))
      print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
      names(r.mean) <- gsub("X", "Day ", names(r.mean))
      resultsStorage <- data.table::rbindlist(list(resultsStorage, f_resultsStorage(r.mean, extremeStress)), use.names = FALSE)
      
      cat(paste0(red("species: ", m, ", ensemble ssp: ", k, ", start year: ", l, "\n"))) #, ", ratio of small to large: ", round(ratio, 2)), "\n"))
      cat((paste0(red("ensemble mean min: ", round(min(minmax(r.mean)), 2), ", ensemble mean max: ", round(max(minmax(r.mean)), 2), ", ensemble mean out file: ", fileName_out), "\n\n")))
    }
  }
  
  f_prepareR <- function() {
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_in <- paste0(locOfDataFiles, "extremeCt.", m, "_", k, "_", yearSpan, ".tif")
    print(paste0("fileName in: ", fileName_in))
    r <- rast(fileName_in)
    names(r) <- "value"
    return(r)
  }
  
  f_thi_graphing <- function(m, r, col, extremeStress, totalNum, totalNum_extreme, maxCount) {
    print("--------------------------")
    names(r) <- "value"
    print(r)
    # plot(r)
    totalNum_extreme <- totalNum_extreme/1000000 # convert to millions
    totalNum <- totalNum/1000000 # convert to millions
    ratio_extreme <- 100 * totalNum_extreme/totalNum # convert to percent
    totalNum_extreme <- round(totalNum_extreme, 0)
    totalNum <- round(totalNum, 0)
    ratio_extreme <- round (ratio_extreme, 1)
    
    print(paste0("species: ", m, ", min r: ", min(minmax(r)), ", max r: ", max(minmax(r))))
    if (is.na( max(minmax(r)))) stop("max is na")
    # browser()
    # # crop to eliminate Antarctica and project to Robinson
    # r_mask <- crop(r, ext_noAntarctica)
    # r_maskRob <- project(r_mask, crsRob)
    r <- project(r, crsRob)
    #  print(paste0("min r proj: ", min(minmax(r)), ", max r proj: ", max(minmax(r)), ", rname: ", paste0("r_", k, "_", l)))
    caption <- paste0("The extreme stress value for ", m, " is ", extremeStress, ". Stress locations are where the species was raised in the early 21st century. \nOf early century species numbers, ", ratio_extreme, "% are in locations with at least ", maxCount, " days with extreme stress during this period.")
    if (m == "humans") {
      caption <- paste0("The extreme stress value for reduction in physical work capacity (PWC) is ", extremeStress, " %.")
    }
    r_df <- as.data.frame(r, xy = TRUE)
    #   r_df$value[r_df$value > maxVal] <- maxVal #set values > maxVal to maxVal
    r_df_mod <- r_df %>%
      mutate(value_2 = cut(value, breaks = custom_bins)) %>%
      group_by(value_2)
    g <- ggplot() +
      geom_tile(data = r_df_mod, aes(x, y, fill = value_2)) +
      #      scale_fill_discrete(colors = colorList, drop = FALSE, na.value = 'grey95') +
      scale_fill_manual(values = col, drop = FALSE, na.value = 'grey95') + # the na.value doesn't work yet. See https://stackoverflow.com/questions/45144630/scale-fill-manual-define-color-for-na-values/45147172 for a possible solution
      labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + 
      theme_bw()  +
      #      theme(plot.title = element_text(size = 12, hjust = 0.5), plot.caption = element_text(hjust = 0, size = 8)) +
      theme(
        legend.text.align = 1,
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 7.0, size = 8)
      ) +
      geom_sf(data = coastline_cropped , 
              color = "black", size = 0.1, stat = "sf", fill = NA,
              position = "identity")
    
    print(g)
    ggsave(filename = fileName_out, plot = g, device = "png", width = 6, height = 6, units = "in", dpi = 300)
    knitr::plot_crop(fileName_out) # gets rid of margins around the plot
    print(paste0("fileName out: ", fileName_out))
  }
}
#THI scenarios -----
# speciesChoice <- c("chicken")
# sspChoices <- "ssp585"
# startYearChoices <- 2081
# modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"


for (k in sspChoices) {
  #    k = "ssp126"
  for (l in startYearChoices) {
    f_THI(k, l, yearRange)
  }
}

#THI historical -----
k <- "historical"
l = 1991
f_THI(k, l, yearRange)

# ensemble means -----
# combine all the spatrasters by model for the time period and then take the mean across that combo
#speciesChoice <- c("chicken")

for (k in sspChoices) {
  #  k <- "ssp585"
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    f_ensemble_THI(m, k, l, yearSpan)
  }
}

# ensemble means, historical -----
#speciesChoice <- c("chicken")
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
f_ensemble_THI(m, k, l, yearSpan)

fileName_resultsStorage <- paste0(locOfDataFiles, "results/resultsStorage_all.csv")

write.csv(resultsStorage, fileName_resultsStorage, row.names = FALSE)

# mpegs, ensemble means -----

for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    #     yearnumberRange <- seq(l, (l + yearRange), 1)
    
    # cattle and goats have the same values, but mpegs are created separately
    for (m in speciesChoice) {
      col = colorList
      if (m == "humans") col = rev(colorList)
      #      browser()      
      extremeStress <- f_getExtremeStressValue(m)
      print(paste0("species choice m: ", m))
      fileName_in <- paste0(locOfDataFiles, "ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
      r <- rast(fileName_in)
      maxVal <- round(max(minmax(r)), 3)
      minVal <- round(min(minmax(r)), 3)
      print(paste0("raster r, ", "species: ", m, ", minVal: ", minVal, ", maxVal: ", maxVal, ", extreme stress: ", extremeStress, ", fileName in: ", fileName_in))      
      # set up names with Jan-1. Need to set up the endDate_year with a leap year such as 2040
      startDate_year <- paste0(2040, "-01-01")
      endDate_year <- paste0(2040, "-12-31")
      indices <- seq(as.Date(startDate_year), as.Date(endDate_year), 1)
      indices_day <- format(indices, "%b %d")
      indices_date <- format(indices, "%j")
      #      names(r) <- gsub("X", "Day.", names(r))
      videoName_out <- paste0("graphics/cmip6/THI/ensemble_thi.", m, "_", k, "_", yearSpan, ".mp4")
      print(paste0("video fileName out: ", videoName_out))
      #     x.cv <- tapp(x, indices_date, fun = cv, na.rm = TRUE)
      system.time(r.mean <- tapp(r, indices_date, fun = mean, na.rm = TRUE)) #, filename = fileName_out, overwrite = TRUE, woptList = woptList))
      print(r.mean)
      names(r.mean) <- gsub("X", "Day ", names(r.mean))
      title_animate <- paste0("THI value, ", m, " ", k,  ", ", gsub("_", "-", yearSpan), ", extreme stress value: ", extremeStress)
      breaks = c(1, bpList[species %in% m, noStress], bpList[species %in% m, moderateStress],bpList[species %in% m, extremeStress], max(minmax(r.mean)))
      if (m == "humans") {
        title_animate <- paste0("PWC value (%), ", k, ", ", gsub("_", "-", yearSpan), ", extreme stress PWC value is less than: ", extremeStress)
        range = c(0, 100)
        breaks = c(1, 40, 60, 80, 100)
      }
      print(paste0("raster r.mean, ", "species: ", m, ", minVal: ", minVal, ", maxVal: ", maxVal, ", extreme stress: ", extremeStress, ", fileName in: ", fileName_in))      
      breaks <- round(breaks, 0)
      print(paste0("breaks: ", breaks))
      names(r.mean) <- paste0(title_animate, ", ", indices_day)
      rangeMaximum <- max(minmax(r.mean))
      animation::saveVideo(animate(r.mean, n=1, ylim = c(-60, 90), range = c(0, rangeMaximum,  main = "test",), pause = .001, sub = title_animate, col = col, breaks = breaks, axes = FALSE), ani.height = 800, ani.width = 1200, video.name = videoName_out)
    }
  }
}

#speciesChoice <- "cattle"
# extreme stress scenarios -------
for (k in sspChoices) {
  for (l in startYearChoices) {
    f_extremeStress_setup(k, l, colorList)
  }
}

# extreme stress, historical -----
k <- "historical"
l <- 1991
f_extremeStress_setup(k, l, colorList)

{#  do graphics -----
  library(ggplot2)
  library(RColorBrewer)
  #library(rworldmap)
  library(maps)
  #remotes::install_github("ropensci/rnaturalearthhires") need to do once to get the library from github
  #library(rnaturalearthhires)
  library(ggspatial)
  library(data.table)
  library(readxl)
  library(sf)
  library(dplyr)
  legendTitle <- "Days"
  # test values
  m = "chicken"
  # coastline <- st_read("data-raw/regionInformation/ne_50m_coastline/ne_50m_coastline.shp")
  # coastline_cropped <- f_crop_custom(coastline)
  # coastline_cropped <- st_transform(coastline_cropped, crsRob)
  
}

f_thi_graphing_prep <- function(l, yearRange, m, k, extremeStress, dt_stressCts) {
  yearSpan <- paste0(l, "_", l + yearRange)
  fileName_in <- paste0(locOfDataFiles, "extremeCt.", m, "_", k, "_", yearSpan, ".tif")
  print(paste0("fileName in: ", fileName_in))
  r <- rast(fileName_in)
  names(r) <- "value"
  titleText <- paste0(m, ", days above extreme stress value, ", k, ", ", gsub("_", "-", yearSpan))
  if (m == "humans") {
    titleText <- paste0(m, ", days with PWC below ", extremeStress, " percent, " , k, ", ", gsub("_", "-", yearSpan))
  }
  fileName_out <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", k, "_", yearSpan, ".png")
  print(paste0("fileName out: ", fileName_out))
  r_mask_scen <- r_mask
  r_mask_scen[r_mask_scen < maskMin] <- NA
  r_mask_scen[r_mask_scen > 0] <- 1
  #   r <- eval(parse(text = paste0("r_", k, "_", m, "_", l)))
  r <- mask(r, r_mask_scen)
  # get counts in extreme stress regions with more than maxCount days
  maxCount <- 30
  r_ext <- r
  r_ext[r < maxCount] <- NA
  r_ext_mask <- mask(r_mask, r_ext)
  totalNum_extreme <- global(r_ext_mask, fun = "sum", na.rm = TRUE)
  totalNum <- 6.512 * 10^9 # world population in 2005 is 6.512 billion
  if (!m == "humans") totalNum <- animalCts_global[species == m, ct]
  dt_stressCts <- rbindlist(list(dt_stressCts, list(m, k, l, as.numeric(totalNum_extreme), totalNum, maxCount, extremeStress)), use.names = FALSE)
  
  #      totalNum <- global(r_mask, fun = "sum", na.rm = TRUE)
  print(paste0("share of ", m, " in regions with extreme stress days greater than ", maxCount, " is ", round(100 * totalNum_extreme/totalNum, 2), " %."))
  #   r <- crop(r, ext_noAntarctica) I think r_mask is already cropped so no need to do this
  return(dt_stressCts)
}

# speciesChoice <- "humans"
#graphics setup, extreme stress-----

# create blank table to hold animal numbers in high stress areas
dt_stressCts <- data.table(species = character(), ssp  = character(), startYear  = character(), stressCts = numeric(), totalCts = numeric(), countDays = numeric(), extremeStress = numeric())
for (m in speciesChoice) {
  print(paste0("------", m, "------"))
  extremeStress <- f_getExtremeStressValue(m) 
  
  # create mask by number of animals in cell of fileName_r_mask
  fileName_r_mask <- paste0("data/animals/raster_ct_", m, ".tif") # already cropped Antarctica off
  r_mask <- rast(fileName_r_mask)
  maskMin <- switch(
    m,
    "cattle" = 5000,
    "humans" = 0,
    "swine" = 1000,
    "chicken" = 10000,
    "goat" = 5000,
    "sheep" = 3000
  )
  
  #  maxVal <- max(minmax(r_ssp585_2081))  # assumes the maximum value in all years is found at the end of the century in the scenario ssp585
  #  custom_bins <- round(seq.int(from = 0, to = maxVal, length = 4))
  # alternate approach to custom_bins, choose relevant number of days since should be the same for all THIs
  custom_bins <- c(1, 25, 50, 100, 366)
  col = colorList
  
  #  scenario graphics -----
  for (k in sspChoices) {
    for (l in startYearChoices) {
      dt_stressCts <- f_thi_graphing_prep (l, yearRange, m, k, extremeStress, dt_stressCts)
      dt_stressCts
      f_thi_graphing(m, r, col, extremeStress, totalNum, totalNum_extreme, maxCount)
      print("-------------------------")
    }
  }
  
  # historical graphing -----
  k <- "historical"
  l <- 1991
  dt_stressCts <- f_thi_graphing_prep (l, yearRange, m, k, extremeStress, dt_stressCts)
  dt_stressCts
  f_thi_graphing(m, r, col, extremeStress, totalNum, totalNum_extreme, maxCount)
  print("-------------------------")
  
  # delta graphics -----
  l = 2081
  yearSpan <- paste0(l, "_", l + yearRange)
  
  for (k in sspChoices) {
    titleText <- paste0(m, ", change in days above extreme stress value, \n early to end century ", k)
    if (m == "humans") {
      titleText <- paste0(m, ", change in days with PWC below ", extremeStress, " percent, \n early to end century ", k)
    }
    fileName_out <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", k, "_", yearSpan, ".png")
    r_ssp <- rast(paste0(locOfDataFiles, "extremeCt.", m, "_", k, "_", yearSpan, ".tif"))
    r_historical <- rast(paste0(locOfDataFiles, "extremeCt.", m, "_", "historical", "_", "1991_2010", ".tif"))
    r <- r_ssp - r_historical
    names(r) <- "value"
    r[r$value < 0] <- 0
    print(r)
    # r <- mask(r, r_maskRob)
    # r_df <- as.data.frame(r, xy = TRUE)
    r_mask_scen <- r_mask
    r_mask_scen[r_mask_scen < maskMin] <- NA
    r_mask_scen[r_mask_scen > 0] <- 1
    #   r <- eval(parse(text = paste0("r_", k, "_", m, "_", l)))
    r <- mask(r, r_mask_scen)
    
    # get counts in extreme stress regions with more than maxCount days
    maxCount <- 30
    r_ext <- r >= maxCount
    r_ext_mask <- mask(r_mask, r_ext)
    totalNum_extreme <- global(r_ext_mask, fun = "sum", na.rm = TRUE)
    totalNum <- global(r_mask, fun = "sum", na.rm = TRUE)
    
    
    r <- crop(r, ext_noAntarctica)
    f_thi_graphing(m, r, col, extremeStress, totalNum, totalNum_extreme, maxCount)
  }
}

write.csv(dt_stressCts, paste0(locOfDataFiles, "stressCtsTable.csv"))

{# do ppt for THI extreme ct ensemble means -----
  library(officer)
  f_extremeCtSpeciesForPptx <- function(m) {
    fileNameStart <- paste0("THIextremeCt.", m, "_")
    fileName_in <- paste0("graphics/cmip6/THI/", fileNameStart, k, "_", yearSpan, ".png")
    print(paste0("fileName in: ", fileName_in))
    extImg_favLocs <- external_img(src = fileName_in, width = defaultWidth, height = defaultHeight)
    my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
    my_pres <- ph_with(x = my_pres, value = extImg_favLocs, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )
    return(my_pres)
  }
  
  f_deltaExtremeCtSpeciesForPptx <- function() {
    print(paste0("fileName in: ", fileName_in))
    extImg_favLocs <- external_img(src = fileName_in, width = defaultWidth, height = defaultHeight)
    my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
    my_pres <- ph_with(x = my_pres, value = extImg_favLocs, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )
    return(my_pres)
  }
  
  thiList <- c("thi.humans", "thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.swine")
  thiListReduced <- thiList[!thiList %in% c("thi.yak", "thi.broiler", "thi.layer")]
  
  titleString <- paste0("Effects to 2100 of Temperature and Humidity on Productivity of Humans and ", length(thiListReduced)-1 , " Animal Species")
  contentString <- paste0("Preliminary Results: Monthly ensemble means by species for three time periods to 2100. Powerpoint produced on ", Sys.Date())
  startYearChoices_ensemble <-  c(1991, 2041, 2081) 
  
  defaultWidth <- 9
  defaultHeight <- 7
  defaultLeft <- .5
  defaultTop <- 1
}
dataText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
dataText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/). "
dataText3 <- "It includes data from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three scenarios (ssp126, ssp370 and ssp585). In this powerpoint, only results using ssp 126 and ssp585 are presented. " 
dataText4 <- "The data from a 20 year period for the individual models are averaged for each month and a coefficient of variation across the 5 models is calculated. "

IntroText0 <- "The productivity of humans doing field work and animals in producing meat and milk is affected by exposure to combined high levels of temperature and humidity. For humans, the physical work capacity index (PWC) shows the percentage by which  human work capacity is reduced. The temperature and humidity index (THI) for animals is a species-specific measure of those effects with thresholds for low, medium and high negative productivity effects. "
IntroText0.5 <- "\nThe following figures present global graphics of locations where extreme stress is experienced and how many days this happens in a representative year."
IntroText1 <- "\nFor the animals graphics, extreme stress areas are where the species was raised in the early 21st century."
IntroText4 <- "\nThe THI values are averages for three 20 year periods (1991-2010, 2041-2060, and 2081-2100)."
IntroText5 <- "\nThis powerpoint presents work in progress and should not be circulated without permission."
fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
fp_2 <- fp_text(bold = FALSE, font.size = 12)
fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)

blIntro <- block_list(
  fpar(
    ftext(IntroText0, fp_2),
    ftext(IntroText0.5, fp_2)),
  fpar(
    ftext(IntroText1, fp_2)),
  fpar(
    ftext(IntroText4, fp_2)),
  fpar(
    ftext(IntroText5, fp_2))
)

blData <- block_list(
  #  fpar(ftext("hello world", fp_1)),
  fpar(
    ftext(dataText1, fp_2),
    ftext(dataText2, fp_2),
    ftext(dataText3, fp_2),
    ftext(dataText4, fp_2) #,
    #    ftext(dataText5, fp_2)
  ))

my_pres <- read_pptx()
my_pres <- add_slide(x = my_pres, layout = 'Title Slide', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = titleString, location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(x = my_pres, value = contentString, location = ph_location_type(type = "subTitle"))

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = blIntro, location = ph_location_type(type = "body") )

# do all slides for each species in thiListReduced
for (m in thiListReduced) {
  m <- gsub("thi.", "", m)
  ensembleTitle <- m
  my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
  my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
  
  # do historical first, then ssps and future periods -----
  k <- "historical"
  l <- 1991
  yearSpan <- paste0(l, "_", l + yearRange)
  my_pres <- f_extremeCtSpeciesForPptx(m)
  
  for (k in sspChoices) {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      my_pres <- f_extremeCtSpeciesForPptx(m)
    }
  }
  
  # delta days, ppt -----
  ensembleTitle <- paste0(m, ", Change between early and end of the 21st century in days with extreme risk")
  if (m == "humans")   ensembleTitle <- paste0(m, ", Change in days with PWC less than 60% between early and end of the 21st century")
  
  my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
  my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
  for (k in sspChoices) {
    fileName_in <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", k, "_", "2081_2100.png")
    my_pres <- f_deltaExtremeCtSpeciesForPptx()
  }
  my_pres <- ph_with(x = my_pres, value = blData, location = ph_location_type(type = "body") )
}

print(my_pres, target = "presentations/cmip6/THI/damageTemp_Ensemble.pptx") %>% browseURL()


# for (j in 1:length(thiListReduced)) {
#   for (k in sspChoices) {
#     yearSpan <- paste0(l, "_", l + yearRange)
#     print(paste0("ssp choice: ", k, ", start year: ", l))
#     speciesName <- gsub("thi.", "", thiListReduced[j])
#     ensembleTitle <- paste("Ensemble Mean for", speciesName)
#     add_slide(my_pres, layout = 'Section Header', master = 'Office Theme')  %>% 
#       ph_with(value = ensembleTitle, location = ph_location_type(type = "body"))
#     
#     fileNameCts <- paste0("graphics/cmip6/THI/THIextremeCt.", speciesName, "_", k, "_", yearSpan, ".png")
#     extImgObs <- external_img(src = fileNameCts, width = 5, height = 8)
#     
#     add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
#       ph_with(value = extImgObs, location = ph_location(left = 2, top = 0, width = 5, height = 8) )
#     
#     fileNameObserved <- paste0("graphics/cmip6/THI/THIextremeCt.", speciesName, "_historical_",  "1991_2010", ".png")
#     
#     extImgObs <- external_img(src = fileNameObserved, width = 5, height = 8)
#     add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
#       ph_with(value = extImgObs, location = ph_location(left = 0, top = 0, width = 5, height = 8) )
#     
#     for (l in startYearChoices_ensemble) {
#       yearSpan <- paste0(l, "_", l + yearRange)
# #      fileNameCV <- paste0("graphics/cmip6/THI/THI_ensembleCV_masked_",   speciesName, "_",  yearSpan, "_", k, ".jpg")
#       fileNameMean <- paste0("graphics/cmip6/THI/THI_ensembleMean_masked_",  speciesName, "_",  yearSpan, "_", k, ".png")
#       
#       extImgMean <- external_img(src = fileNameMean, width = 5, height = 8)
# #      extImgCV <- external_img(src = fileNameCV, width = 5, height = 8)
#       
#       #   add_slide(my_pres, layout = 'Comparison', master = 'Office Theme') %>% 
#       #     ph_with(value = extImgMean, location = ph_location_left(),  use_loc_size = FALSE ) %>%
#       # #  add_slide(my_pres, layout = 'Comparison', master = 'Office Theme') %>% 
#       #     ph_with(value = extImgCV, location = ph_location_right(),  use_loc_size = FALSE )
#       
#       
#       add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
#         ph_with(value = extImgMean, location = ph_location(left = 0, top = 0, width = 5, height = 8) ) %>%
#         ph_with(value = extImgCV, location = ph_location(left = 5, top = 0, width = 5, height = 8) )
#       
#       my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
#       my_pres <-  ph_with(x = my_pres, value = "Data Source", location = ph_location_type(type = "title"))
#       my_pres <- ph_with(x = my_pres, value = blData, location = ph_location_type(type = "body") )
#       
#     #     }
#   }
# }



# # graphics with polygon shading -----
# library(ggpattern)
# for (m in speciesChoice) {
#   if (m == "humans") {extremeStress <- extremeStress_humans
#   } else {
#     extremeStress <- bpList[species %in% m, extremeStress]
#   }
#   fileName_century_begin <- paste0(locOfDataFiles, "extremeCt.", m, "_", "historical", "_", yearSpan_begin, ".tif")
#   r_begin <- rast(fileName_century_begin)
#   names(r_begin) <- "value"
#   fileName_r_mask <- paste0("data/animals/raster_ct_", m, ".tif")
#   r_mask <- rast(fileName_r_mask)
#   
#   maskMin <- switch(
#     m,
#     "cattle" = 5000,
#     "humans" = 0,
#     "swine" = 1000,
#     "chicken" = 10000,
#     "goat" = 5000,
#     "sheep" = 3000
#   )
#   
#   
#   r_mask[r_mask < maskMin] <- NA
#   r_mask[r_mask > 0] <- 1
#   r_mask_pg <- as.polygons(r_mask)
#   test <- as(r_mask_pg, "Spatial") # convert the SpatVector to a spatial SpatialPolygonsDataFrame
#   
#   r_begin <- mask(r_begin, r_mask)
#   r_begin <- crop(r_begin, ext_noAntarctica)
#   r_begin <- project(r_begin, crsRob)
#   r_begin_df <- as.data.frame(r_begin, xy = TRUE)
#   
#   #sspChoices <- c("ssp126", "ssp585")
#   #  for (k in sspChoices) {
#   fileName_century_end_ssp585 <- paste0(locOfDataFiles, "extremeCt.", m, "_", "ssp585", "_", yearSpan_end, ".tif")
#   r_end_ssp585 <- rast(fileName_century_end_ssp585)
#   
#   r_end_ssp585 <- mask(r_end_ssp585, r_mask)
#   r_end_ssp585 <- crop(r_end_ssp585, ext_noAntarctica)
#   r_end_ssp585 <- project(r_end_ssp585, crsRob)
#   
#   fileName_century_end_ssp126 <- paste0(locOfDataFiles, "extremeCt.", m, "_", "ssp126", "_", yearSpan_end, ".tif")
#   r_end_ssp126 <- rast(fileName_century_end_ssp126)
#   r_end_ssp126 <- mask(r_end_ssp126, r_mask)
#   r_end_ssp126 <- crop(r_end_ssp126, ext_noAntarctica)
#   r_end_ssp126 <- project(r_end_ssp126, crsRob)
#   
#   r_delta_ssp585 <- r_end_ssp585 - r_begin
#   names(r_end_ssp585) <-  names(r_delta_ssp585) <- "value"
#   
#   r_delta_ssp126 <- r_end_ssp126 - r_begin
#   names(r_end_ssp126) <-  names(r_delta_ssp126) <- "value"
#   
#   
#   # convert to data frame to use with ggplot
#   r_end_ssp126_df <- as.data.frame(r_end_ssp126, xy = TRUE)
#   r_delta_ssp126_df <- as.data.frame(r_delta_ssp126, xy = TRUE)
#   
#   r_end_ssp585_df <- as.data.frame(r_end_ssp585, xy = TRUE)
#   r_delta_ssp585_df <- as.data.frame(r_delta_ssp585, xy = TRUE)
#   
#   for (i in c("r_begin_df", "r_end_ssp585_df", "r_delta_ssp585_df", "r_end_ssp126_df", "r_delta_ssp126_df")) {
#     if (i %in% "r_begin_df") {
#       titleText <- paste0(m, ", days above extreme stress value, ", gsub("_", "-", yearSpan_begin))
#       if (m == "humans") titleText <- paste0(m, ", days with PWC below ", extremeStress, " percent, ", gsub("_", "-", yearSpan_begin))
#       fileName_out <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", "historical", "_", yearSpan_begin, ".png")
#       r <- r_begin_df}
#     if (i %in% "r_end_ssp585_df") {
#       titleText <- paste0(m, ", days above extreme stress value, ", " ssp585, ", " ", gsub("_", "-", yearSpan_end))
#       if (m == "humans") titleText <- paste0(m, ", days with PWC below ", extremeStress, " percent" , ", ssp585, ", gsub("_", "-", yearSpan_begin))
#       fileName_out <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", "ssp585", "_", yearSpan_end, ".png")
#       r <- r_end_ssp585_df}
#     if (i %in% "r_delta_ssp585_df") {
#       titleText <- paste0(m, ", change in days above extreme stress value, \n early to end century ", "ssp585")
#       if (m == "humans") titleText <- paste0(m, ", change in days with PWC below ", extremeStress, " percent, \n early to end century ", "ssp585")
#       fileName_out <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", "ssp585", "_", yearSpan_end, ".png")
#       r <- r_delta_ssp585_df}
#     if (i %in% "r_end_ssp126_df") {
#       titleText <- paste0(m, ", days above extreme stress value, ", " ssp126", " ", gsub("_", "-", yearSpan_end))
#       if (m == "humans") titleText <- paste0(m, ", days with PWC below ", extremeStress, " percent" , ", ssp126, ", gsub("_", "-", yearSpan_end))
#       fileName_out <- paste0("graphics/cmip6/THI/THIextremeCt.", m, "_", "ssp126", "_", yearSpan_end, ".png")
#       r <- r_end_ssp126_df}
#     if (i %in% "r_delta_ssp126_df") {
#       titleText <- paste0(m, ", change in days above extreme stress value, \n early to end century ", "ssp585")
#       if (m == "humans") titleText <- paste0(m, ", change in days with PWC below ", extremeStress, " percent, \n early to end century ", "ssp126")
#       fileName_out <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", "ssp126", "_", yearSpan_end, ".png")
#       r <- r_delta_ssp126_df
#     }
#     gc()
#     
#     maxVal <- max(minmax(r_end_ssp585)) 
#     #   if (maxVal > 150) maxVal <- 5
#     custom_bins <- round(seq.int(from = 0, to = maxVal, length = 4))
#     r$value[r$value > maxVal] <- maxVal #set values > maxVal to maxVal
#     g <- ggplot(data = coastline) +
#       labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
#       labs(x = "", y = "") +
#       geom_tile(data = r, aes(x, y, fill = value)) +
#       scale_fill_gradientn(colours = colorList, na.value = "white",
#                            breaks = custom_bins,labels = custom_bins,
#                            limits = c(0, maxVal)) + 
#       geom_sf(fill = NA, color = "gray", lwd = 0.2) +
#       theme_bw() +
#       theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
#       theme(legend.text.align = 1) +
#       ggpattern::geom_tile_pattern(data=test, aes(x=long, y=lat), 
#                                    pattern = 'crosshatch', fill   = 'white', colour  = 'black')
#     
#     print(g)
#     ggsave(filename = fileName_out, plot = g, width = 6, height = 6, units = "in", dpi = 300)
#     knitr::plot_crop(fileName_out) # gets rid of margins around the plot
#     print(paste0("fileName out: ", fileName_out))
#     g <- NULL
#     #     print(g)
#     dev.off()
#   }
# }

library(rgl)
plot3d(x = r_end_ssp126_df$x, y = r_end_ssp126_df$y, z = r_end_ssp126_df$value, col = colorList,
       xlab="longitude", ylab="latitude", zlab="Count of extreme stress")

library(plotly)
plot_ly(x = r_end_ssp126_df$x, y = r_end_ssp126_df$y, z = r_end_ssp126_df$value, colors = colorList, type = "scatter3d", mode = "markers",
        xlab="longitude", ylab="latitude", zlab="Count of extreme stress")
# # get animal raster masks
# animalsOnly <- speciesChoice[!speciesChoice %in% "humans"]
# for (m in animalsOnly) {
#   if (m %in% "cattle") cutoff = 2000
#   fileName_r_mask <- paste0("data/animals/raster_ct_", m, ".tif")
#   r_mask <- rast(fileName_r_mask)
#   values(r_mask)[values(r_mask) < 2000] <- NA
#   r_delta_mask <- mask(r_delta, r_mask)
# }
# r_delta_mask <- mask(r_delta, pop_mask)
# pop_mask
# #plot data with masking
# 
# for (m in speciesChoice) {
#   fileName_century_end <- paste0(locOfDataFiles, "extremeCt.", m, "_", k, "_", yearSpan_end, ".tif")
#   fileName_century_begin <- paste0(locOfDataFiles, "extremeCt.", m, "_", "historical", "_", yearSpan_begin, ".tif")
#   r_raster <- 
#     r_end <- rast(fileName_century_end)
#   r_begin <- rast(fileName_century_begin)
#   r_delta <- r_end - r_begin
#   
#   r_delta_df <- as.data.frame(r_delta_mask, xy = TRUE)
#   names(r_delta_df)[names(r_delta_df) == "lyr.1"] <- "value"
#   
#   titleText <- paste0(m, ", change in days above extreme stress value, \n\n early to end century ", "SSP585 ")
#   
#   legendTitle <- "Change in \n\nHigh Stress Days"
#   print(titleText)
#   gc()
#   colorList <- (RColorBrewer::brewer.pal(5, "YlOrRd"))
#   maxVal <- 250
#   custom_bins <- round(seq.int(from = 0, to = maxVal, length = 6))
#   r <- r_delta_df
#   r$value[r$value > maxVal] <- maxVal
#   g <- ggplot(data = coastline) +
#     labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
#     labs(x = "", y = "") +
#     
#     geom_raster(data = r, aes(x, y, fill = value)) +
#     scale_fill_gradientn(colours = colorList, na.value = "white",
#                          breaks = custom_bins,labels = custom_bins,
#                          limits = c(0, maxVal)) + 
#     geom_sf(fill = NA, color = "gray")
#   g
#   fileName_out <- paste0("graphics/cmip6/THI/THIextremeCtDelta.", m, "_", k, ".png")
#   png(filename = fileName_out, width = 6, height = 6, units = "in", res = 300)
#   print(g)
#   dev.off()
# }
# 
# # convert population density raster
# pop_mask <- rast("data-raw/gpw_v4_population_density_rev11_2020_30_min.tif")
# setMinMax(pop_mask)
# fileName_r_mask <- paste0("data/animals/raster_ct_", "humans", ".tif")
# writeRaster(pop_mask, fileName_r_mask, overwrite = TRUE, woptList = woptList)
# 
# maskValUpper <- 3000
# maskValLower <- 10
# pop_mask > maskValUpper
# pop_mask[pop_mask > maskValUpper] <- NA
# pop_mask[pop_mask < maskValLower] <- NA
# pop_mask[pop_mask > 0] <- 1
# 
# fileName_in <- locOfDataFiles, "ensemble_thi.humans_ssp585_2081_2100.tif"
# test <- rast(fileName_in)
# test
# test_brick <- raster::brick(fileName_in)
# test_brick
# test_anim <- raster::animate(test_brick, 1)
# 
# # calculate number of sequential days with stress; minimum of 5
# 
# fun <- function(cellVector) {
#   startend <- c(NA, NA) 
#   # if (is.nan(cellVector[1])) {
#   #   return(startend)
#   # }
#   g <- gregexpr("1{5,}", paste(+(cellVector > 89), collapse = ""))[[1]]
#   print(g)
#   if (!g[[1]] == -1) { # no need to write to growing season if g returns 1
#     startend[1] <- g[[1]]
#     matchLength <- attributes(g)$match.length
#     startend[2] <- startend[1] + matchLength - 1
#   }
#   return(startend) 
# }
# 
# fileName_in <- paste0(locOfDataFiles, "ensemble_thi.", m, "_", k, "_", yearSpan, ".tif")
# r <- rast(fileName_in)
# 
# print(system.time(extremeStress <- app(r, fun)))
# I discoverd that the functions below were not being used, as of Jan 11, 2021. I put them here temporarily 
# f_readRast_thi_cattle <- function(yearNumber) {
#   fileName_in <- paste0(locOfDataFiles, "thi.cattle_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
#   r <- rast(fileName_in)
# }
# 
# f_readRast_thi_sheep <- function(yearNumber) {
#   fileName_in <- paste0(locOfDataFiles, "thi.sheep_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
#   print(fileName_in)
#   r <- rast(fileName_in)
# }
# f_readRast_thi_goat <- function(yearNumber) {
#   fileName_in <- paste0(locOfDataFiles, "thi.goat_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
#   print(fileName_in)
#   r <- rast(fileName_in)
# }
# f_readRast_thi_chicken <- function(yearNumber) {
#   fileName_in <- paste0(locOfDataFiles, "thi.chicken_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
#   print(fileName_in)
#   r <- rast(fileName_in)
# }
# 
# f_readRast_thi_swine <- function(yearNumber) {
#   fileName_in <- paste0(locOfDataFiles, "thi.swine_", i, "_", k,  "_", yearNumber, ".tif") # note yearNumber here
#   print(fileName_in)
#   r <- rast(fileName_in)
# }


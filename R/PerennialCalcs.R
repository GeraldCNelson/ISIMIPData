# code to do various perennial crop calculations
{source("R/globallyUsed.R")
  terraOptions(memfrac = 2, progress = 0, tempdir =  "data/ISIMIP", verbose = FALSE)
  locOfClimFiles <- "data/bigFiles/"
  locOfDataFiles <- "data/cmip6/perennials/"
  sspChoices <- c("ssp126", "ssp585") 
  #sspChoices <- c("ssp585") 
  modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") 
  #modelChoices <- c("MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") 
  startyearChoices <-  c(2041, 2081) 
  #startyearChoices <-  c(2081) 
  startyearChoices_historical <- c(1991)
  scenarioChoicesEnsemble <- c("historical", sspChoices)
  extent_NH <- c( -180, 180, 0, 90)
  extent_SH <-c( -180, 180, -60, 0) #-60 gets rid of Antarctica
  hemisphere <- c("NH", "SH")
  options(warn=0) # convert warnings to errors
  
  yearRange <- 19
  yearRangeSH <- 18 # one less year because of 6 month offset
  fileLocCP <- "data/cmip6/chillPortions/chill_portions/"
  speciesChoice <- c("cherry", "almond", "winegrape", "apple") #, "olive", "berries") 
  
  weatherVarClassification <- read_excel("data-raw/crops/perennials/weatherVarClassification.xlsx", skip = 1)
  names(weatherVarClassification) <- c("range", "range_winterChill_CP", "range_winterChill_species", "range_springFrost_days", "range_springFrost_species", "range_summerHeat_days", "range_summerHeat_species")
  
  # constants -----
  minimumGrwSeasonLength = 100
  tminExtremeVal <- -30 # single day kills the plant
  tminDamageVal <- 0 # frost damage temperature
  tmaxDamageVal <- 35 # heat damage temperature
  unsuitable_springFreezeDays <- 45 # days where temp below tminDamageVal means location is unsuitable
  unsuitable_summerHotDays <- 45 # days where temp above tmaxDamageVal means location is unsuitable
  springStart_NH <- 60 #March 1 in 2019
  springEnd_NH <- 120 #April 30 in 2019
  springStart_SH <- 227 #Aug 15 in 2019
  springEnd_SH <- 288 #Oct 15 in 2019
  heatDamageStart_NH <- 182 #July 1 in 2019
  heatDamageEnd_NH <- 242 #Aug 30 in 2019
  heatDamageStart_SH <- 1 #Jan 1
  heatDamageEnd_SH <- 59 #Feb 28 in 2019
  frostRiskDays <- c(0, 5, 6, 20, 21, 45) # pairs for ranges - low, medium, high
  heatRiskDays <- c(0, 9, 10, 30, 31, 45) # pairs for ranges - low, medium, high
  
  woptList <- list(gdal=c("COMPRESS=LZW"))
  woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))
  
  #test values
  i <- "IPSL-CM6A-LR"
  k <- "ssp585"
  l <- 2041
  yearSpan <- paste0(l, "_", l + yearRange)
  
  
  # functions -----
  
  f_range <- function(x, range) {
    # set locations with values outside the range to NA
    x[x < range[1]] <- NA
    x[x > range[2]] <- NA
    return(x)
  }
  
  f_coldFrostHeatDamage <- function() {
    # does extreme cold locations and frost and heat damage locations
    combined <- sds()
    fileName_tasmin <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr_", k, "_tasmin_", yearSpan, ".tif")
    fileName_tasmax <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr_", k, "_tasmax_", yearSpan, ".tif")
    tmin <- rast(fileName_tasmin)
    tmax <- rast(fileName_tasmax)
    for (m in hemisphere) {
      # do extreme cold locations
      extremeColdCt <- sum(tmin < tminExtremeVal, na.rm = TRUE)
      extremeColdCt[extremeColdCt > 0] <- 1 # one day is all it takes
      extremeColdMask_hem <- crop(extremeColdCt, get(paste0("extent_", m)))
      
      # species-specific chill portion
      fileLocCP <- "data/cmip6/chillPortions/chill_portions/"
      fileNameCP_in <- paste0(fileLocCP, "ensemble_chill_cutoff_", fruitSpecies, "_", k, "_", m, "_", yearSpan, ".tif")
      chillPortionsCutoff <- rast(fileNameCP_in)
      print(paste0("fileNameCP_in: ", fileNameCP_in))
      # if (m == "NH") extremeColdMask <- crop(extremeCold, extent_NH)
      # if (m == "SH") extremeColdMask <- crop(extremeCold, extent_SH)
      # 
      chillPortionsCutoff[chillPortionsCutoff == 0] <- NA    
      # heat and frost damage calculations
      tmin_hem <- crop(tmin, get(paste0("extent_", m)))
      tmax_hem <- crop(tmax, get(paste0("extent_", m)))
      spStart <- paste0("springStart_", m)
      spEnd <- paste0("springEnd_", m)
      spLyrs <- paste0(get(spStart), ":", get(spEnd))
      hdStart <- paste0("heatDamageStart_", m)
      hdEnd <- paste0("heatDamageEnd_", m)
      hdLyrs <- paste0(get(hdStart), ":", get(hdEnd))
      tmin_hem <- subset(tmin_hem, get(spStart):get(spEnd))
      tmax_hem <- subset(tmax_hem, get(hdStart):get(hdEnd))
      # count number of days tmin is below tminDamageVal
      frostCt <- sum(tmin_hem < tminDamageVal, na.rm = TRUE)
      heatCt <- sum(tmax_hem > tmaxDamageVal, na.rm = TRUE)
      for (i in c("good", "ok", "bad")) {
        frDays <- switch(i,
                         "good" = frostRiskDays[1:2],
                         "ok" = frostRiskDays[3:4],
                         "bad" = frostRiskDays[5:6],
        )
        fr <- f_range(frostCt, frDays)
        hdDays <- switch(i,
                         "good" = heatRiskDays[1:2],
                         "ok" = heatRiskDays[3:4],
                         "bad" = heatRiskDays[5:6],
        )
        hr <- f_range(heatCt, hdDays)
        # do unsuitable
        unsuitableFrost <- frostCt
        unsuitableFrost[unsuitableFrost < unsuitable_springFreezeDays] <- 1
        unsuitableFrost[unsuitableFrost >= unsuitable_springFreezeDays] <- 0
        
        unsuitableHeat <- heatCt
        unsuitableHeat[unsuitableHeat < unsuitable_summerHotDays] <- 1
        unsuitableHeat[unsuitableHeat >= unsuitable_summerHotDays] <- 0 # locations where the number of hot days in summer is greater than the suitable cutoff
        
        
        #   assign(paste0("fr_", m, "_", i), fr)
        #   assign(paste0("hr_", m, "_", i), hr)
        # combined <- sds(combined, get(paste0("fr_", m, i)))
        #      holder <- c("extremeColdMask_hem", paste0("fr_",  m, "_", i), paste0("hr_",  m, "_", i))
        holder <- c("extremeColdMask_hem", "unsuitableFrost", "fr", "unsuitableHeat", "hr", "chillPortionsCutoff") # the names and order in which the rasters are stored in the sds
        combined <- sds(lapply(holder,  function(x) eval(parse(text = x))))
        fileName_out <- paste0(locOfDataFiles, "combinedDamages_", fruitSpecies, "_", m, "_", i, "_", yearSpan, ".nc")
        writeCDF(combined, filename = fileName_out,  overwrite = TRUE, missval=-9999, prec="float", compression=5)
      }
      
      return(combined)
    }
  }
  f_combinedDamage <- function(fruitSpecies) {
    for (m in hemisphere) {
      for (i in c("good", "ok", "bad")) {
      fileName_in <- paste0(locOfDataFiles, "combinedDamages_", fruitSpecies, "_", m, "_", "i", "_", yearSpan, ".nc")
      combined <- rast(fileName_in)
      extremeCold <- combined[1]
      unsuitableFrost <- combined[2]
      frostCt <- combined[3]
      unsuitableHeat <- combined[4]
      heatCt <- combined[5]
      
      print(paste0("working on combined damage ", fruitSpecies, " in hemisphere ", m, ", year ", l, ", scenario ", k))
      #unsuitable summer heat
      # fileNameStart <- paste0("summerHeat_hem_")
      # fileName_heat <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", k, "_", yearSpan, ".tif")
      # print(paste0("fileName_heat: ", fileName_heat))
      # heatCt <- rast(fileName_heat)
      # heatCt[heatCt < unsuitable_summerHotDays] <- 1
      # heatCt[heatCt >= unsuitable_summerHotDays] <- 0 # locations where the number of hot days in summer is greater than the suitable cutoff
      # 
      # # unsuitable spring frost
      # fileNameStart <- paste0("springFrost_hem_")
      # fileName_frost <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", k, "_", yearSpan, ".tif")
      # frostCt <- rast(fileName_frost)
      # frostCt[frostCt < unsuitable_springFreezeDays] <- 1
      # frostCt[frostCt >= unsuitable_springFreezeDays] <- 0
      # 
      # species-specific chill portion
      fileLocCP <- "data/cmip6/chillPortions/chill_portions/"
      fileNameCP_in <- paste0(fileLocCP, "ensemble_chill_cutoff_", fruitSpecies, "_", k, "_", m, "_", yearSpan, ".tif")
      chillPortionsCutoff <- rast(fileNameCP_in)
      print(paste0("fileNameCP_in: ", fileNameCP_in))
      if (m == "NH") extremeColdMask <- crop(extremeCold, extent_NH)
      if (m == "SH") extremeColdMask <- crop(extremeCold, extent_SH)
      
      #combine them all
      chillPortionsCutoff[chillPortionsCutoff == 0] <- NA
      r <- chillPortionsCutoff # 1 is where chill portions is adequate 
      
      print(r)
      print(extremeColdMask)
      r <- mask(r, extremeColdMask, maskvalue = 1) # 0 is no extreme cold; 1 is extreme cold
      r <- mask(r, frostCt, maskvalue = 0) # 1 is where frost Ct is below the unsuitable level
      r <- mask(r, heatCt, maskvalue = 0) # 1 is where heat Ct is below the unsuitable level
      fileName_out <- paste0("data/cmip6/perennials/suitable_", fruitSpecies, "_", k, "_", m, "_", yearSpan, ".tif")
      writeRaster(r, fileName_out, overwrite = TRUE, woptList = woptList)
      print(paste0("file name out: ", fileName_out))
      }
    }
  }
  
  f_favorableLoc <- function(fruitSpecies) {
    for (m in hemisphere) {
      fileName_in <- paste0("data/cmip6/perennials/suitable_", fruitSpecies, "_", k, "_", m, "_", yearSpan, ".tif")
      baseCt <- rast(fileName_in)  #suitable/not suitable - 1/NA - because some combo of chill portions, extreme cold, and frost or heat outside the suitable level
      
      #frostRiskDays
      fileNameStart <- paste0("springFrost_hem_")
      fileName_frost <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", k, "_", yearSpan, ".tif")
      frostCt <- rast(fileName_frost) # number of days with temps below zero
      
      #heatRiskDays
      fileNameStart <- paste0("summerHeat_hem_")
      fileName_heat <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", k, "_", yearSpan, ".tif")
      heatCt <- rast(fileName_heat)
      
      for (i in c("good", "ok", "bad")) {
        #      print(paste0("i: ", i, ", hemisphere: ", m, ", fruitSpecies: ", fruitSpecies))
        frDays <- switch(
          i,
          "good" = frostRiskDays[1:2],
          "ok" = frostRiskDays[3:4],
          "bad" = frostRiskDays[5:6],
        )
        fr <- f_range(frostCt, frDays) # f_range sets values at locations outside the range to be NA
        hdDays <- switch(
          i,
          "good" = heatRiskDays[1:2],
          "ok" = heatRiskDays[3:4],
          "bad" = heatRiskDays[5:6],
        )
        hr <- f_range(heatCt, hdDays)
      }
      r <- mask(get(paste0("heatRisk_", i)), baseCt)
      r <- mask(r, get(paste0("frostRisk_", i)))
      fileName_out <- paste0("data/cmip6/perennials/", fruitSpecies, "_frost_", i, "_heat_", i, "_", k, "_", m, "_", yearSpan, ".tif")
      print(paste0("file name out: ", fileName_out))
      writeRaster(r, fileName_out, overwrite = TRUE, woptList = woptList)
    }
  }
}


# use the functions -----
for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (fruitSpecies in speciesChoice) {
      print(system.time(combined <- f_coldFrostHeatDamage())) # combined has three temperature-related spatrasters - hard freeze, frost, and heat. Each combined output file has all locations that are good, ok, and bad for frost and heat damage
    }
  }
}

#  historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (fruitSpecies in speciesChoice) {
  print(system.time(combined <- f_coldFrostHeatDamage())) # combined has 5 temperature-related spatrasters - hard freeze, unsuitable frost days, acceptable frost days, and heat. Each combined output file has all locations that are good, ok, and bad for frost and heat damage
}

# combined damage -----
# code to read in chill portions, cold, freeze and heat stress 1/0 files and produce 1/0 tifs where the crop is potentially growable. The chill portions files are created in the chillPortions.R script

for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (fruitSpecies in speciesChoice) {
      print(paste0("fruitSpecies: ", fruitSpecies, ", ssp choice: ", k, ", start year: ", l))
      f_combinedDamage(fruitSpecies) 
    }
  }
}

# combined damage, historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (fruitSpecies in speciesChoice) {
  print(paste0("fruitSpecies: ", fruitSpecies, ", ssp choice: ", k, ", start year: ", l))
  f_combinedDamage(fruitSpecies) 
} 

#favorable locations - create figures with different combinations of favorable growing conditions -----
fileLocCP <- "data/cmip6/chillPortions/chill_portions/"
speciesChoice <- c("almond", "apple", "cherry", "winegrape") #, "olive", "berries") 
for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (fruitSpecies in speciesChoice) {
      print(paste0("fruitSpecies: ", fruitSpecies, ", ssp choice: ", k, ", start year: ", l))
      f_favorableLoc(fruitSpecies)
    }
  }
}

#favorable locations, historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (fruitSpecies in speciesChoice) {
  print(paste0("fruitSpecies: ", fruitSpecies, ", ssp choice: ", k, ", start year: ", l))
  f_favorableLoc(fruitSpecies)
} 

# graphing -----
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
colorCombo <- "YlOrRd"
colorList <- rev((RColorBrewer::brewer.pal(4, colorCombo)))

coastline <- st_read("data-raw/regionInformation/ne_50m_coastline/ne_50m_coastline.shp")
library(readxl)
fileName_fruitCPs <- paste0("data-raw/crops/", "fruitCPs.xlsx")
CPs <- read_excel(fileName_fruitCPs)

#function to get rid of Antarctica and do either northern or southern hemisphere
f_crop_custom <- function(poly.sf, ext) {
  poly.sp <- as(poly.sf, "Spatial")
  extR <- ext
  poly.sp.crop <- crop(poly.sp, extR)
  st_as_sf(poly.sp.crop)
}
#st_crop(coastline, c(xmin=-180, xmax= 180, ymin = -60, ymax = 90))
# RobinsonProj <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# crsRob <- RobinsonProj

legendTitle <- "Days"

f_perennialClimateThreatsGraphics <- function() {
  # get extreme cold mask
  fileNameStart <- paste0("extremeColdlocs_", gsub("-", "", tminExtremeVal))
  extremeColdMask <- paste0("data/cmip6/perennials/", fileNameStart, "_", k, "_", yearSpan, ".tif")
  extremeColdMask <- rast(extremeColdMask)
  extremeColdMask_NH <- crop(extremeColdMask, extent_NH)
  extremeColdMask_SH <- crop(extremeColdMask, extent_SH)
  
  for (m in hemisphere) {
    # crop out areas where summer heat or spring frost are greater than the unsuitable values, either unsuitable_springFreezeDays or unsuitable_summerHotDays
    coastline_cropped <- f_crop_custom(coastline, get(paste0("extent_", m)))
    coastline_cropped <- st_transform(coastline_cropped, crsRob)
    
    for (n in c("spring", "summer")) {
      if(n %in% "spring") { 
        fileNameStart <- paste0("springFrost_hem_")
        fileName_in <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", k, "_", yearSpan, ".tif")
        print(paste0("fileName_in: ", fileName_in))
        r <- rast(fileName_in)
        # crop out areas where temp is below extreme level
        r <- mask(r, get(paste0("extremeColdMask_", m)), maskvalue = 1)
        
        r[r > unsuitable_springFreezeDays] <- NA
        titleText <- paste0("Spring frost danger, scenario ", k, ", period ", gsub("_", "-", yearSpan))
        # get maximum value for spring, I'm guessing it is in the historical period
        fileName_in_max <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", "historical", "_", "1991_2010", ".tif")
        maxVal <- ceiling(max(minmax(rast(fileName_in_max))))
        coldDamageLimits <- c(0, 5, 20, unsuitable_springFreezeDays)
        bins <- coldDamageLimits
        caption <- paste0("Note: NA is oceans and land areas that are not suitable because of extreme cold \n(less than ", tminExtremeVal, "°C or more than ", unsuitable_springFreezeDays, " frost days in spring).")
      }
      
      if(n %in% "summer")  {
        fileNameStart <- paste0("summerHeat_hem_")
        fileName_in <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", k, "_", yearSpan, ".tif")
        print(paste0("fileName_in: ", fileName_in))
        r <- rast(fileName_in)
        # crop out areas where temp is below extreme level
        r <- mask(r, get(paste0("extremeColdMask_", m)), maskvalue = 1)
        
        r[r > unsuitable_summerHotDays] <- NA # unsuitable_summerHotDays is number of days where tmax is greater than tmaxDamageVal
        titleText <- paste0("Summer heat danger, scenario ", k, ", period ", gsub("_", "-", yearSpan))
        # get maximum value for summer, I'm guessing it is in the ssp585 at end century 
        fileName_in_max <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", "ssp585", "_", "2081_2100", ".tif")
        maxVal <- ceiling(max(minmax(rast(fileName_in_max))))
        heatDamageLimits <- c(0, 10, 30, unsuitable_summerHotDays)
        bins <- heatDamageLimits
        caption <- paste0("Note: NA is oceans and land areas that are not suitable because of high heat \n(more than ", unsuitable_summerHotDays, 
                          " days in the summer with temperatures above ", tmaxDamageVal, "°C) or extreme cold (less than ", tminExtremeVal, "°C.)")
        
      }
      r <- project(r, crsRob)
      r_df <- as.data.frame(r, xy = TRUE)
      names(r_df) <-   c("lon", "lat", "value")
      
      #for testing
      baseCt_df <- project(baseCt, crsRob)
      baseCt_df <- as.data.frame(baseCt_df, xy = TRUE)
      names(baseCt_df) <-   c("lon", "lat", "value")
      
      fileName_out <- paste0("graphics/cmip6/perennials/", fileNameStart, m, "_", k, "_", yearSpan, ".png")
      custom_bins <- bins
      r_df$custom_bins <- cut(r_df$value, breaks = custom_bins) # convert value to factors
      
      g <- ggplot(data = coastline_cropped) +
        labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) +
        geom_tile(data = r_df , aes(x = lon, y = lat, fill = custom_bins))  +
        scale_fill_manual(values = RColorBrewer::brewer.pal(3, colorCombo)) +
        geom_sf(fill = NA, color = "gray", lwd = 0.1) +
        theme_bw() +
        theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
        theme(plot.title = element_text(size = 12, hjust = 0.5), plot.caption = element_text(hjust = 0, size = 8)) +
        theme(legend.text.align = 1)
      g <- g + geom_tile(data = baseCt_df,   aes(x = lon, y = lat, fill = value)) +
        scale_fill_gradient("Distance",
                            low = 'yellow', high = 'blue',
                            na.value = NA)
      
      #        theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
      print(g)
      ggsave(filename = fileName_out, plot = g, width = 8, height = 4, units = "in", dpi = 300)
      knitr::plot_crop(fileName_out) # gets rid of margins around the plot
      print(paste0("out file name: ", fileName_out))
      g <- NULL
    }
  }
}

# frostRiskDays <- c(0,4,5,6,20,21, 45) # pairs for ranges - good, ok, bad
# heatRiskDays <-  c(0,9,10,30,31, 45) # pairs for ranges - good, ok, bad

f_favorableLocsGraphics <- function() {
  for (m in hemisphere) {
    # crop out areas where summer heat or spring frost are greater than the unsuitable values, either unsuitable_springFreezeDays or unsuitable_summerHotDays
    coastline_cropped <- f_crop_custom(coastline, get(paste0("extent_", m)))
    coastline_cropped <- st_transform(coastline_cropped, crsRob)
    CPfruit <- CPs$chillRequirement[CPs$crop==fruitSpecies]
    for (i in c("good", "ok", "bad")) {
      fileName_in <- paste0("data/cmip6/perennials/", fruitSpecies, "_frost_", i, "_heat_", i, "_", k, "_", m, "_", yearSpan, ".tif")
      titleText <- paste0("Growing conditions for ", fruitSpecies," are ", i, "\n" , "scenario ", k, ", period ", gsub("_", "-", yearSpan))
      if (i == "good") {
        caption <- paste0("Note: Good growing conditions for ", fruitSpecies, " include chill portions of at least ", CPfruit, ", less than ", frostRiskDays[3], 
                          " days of spring frost risk and less than ", heatRiskDays[3], " days of summer excessive heat.")
        col = "green"}
      if (i == "ok") {
        caption <- paste0("Note: Ok growing conditions for ", fruitSpecies, " include chill portions of at least ", CPfruit, ", ", frostRiskDays[3], "-",  frostRiskDays[4], 
                          " days of spring frost risk and ",   heatRiskDays[3], "-",  heatRiskDays[4], " days of summer excessive heat.")
        col = "yellow"}
      if (i == "bad") {
        caption <- paste0("Note: Bad growing conditions for ",fruitSpecies, " include chill portions of at least ", CPfruit, ", ", frostRiskDays[5], "-",  frostRiskDays[6], 
                          " days of spring frost risk and ",  heatRiskDays[5], "-",  heatRiskDays[6], " days of summer excessive heat.")
        col = "red"}
      
      r <- rast(fileName_in)
      r <- project(r, crsRob)
      r_df <- as.data.frame(r, xy = TRUE)
      names(r_df) <-   c("lon", "lat", "value")
      
      fileName_out <- paste0("graphics/cmip6/perennials/", fruitSpecies, "_", i, "_locations_", m, "_", k, "_", yearSpan, ".png")
      
      g <- ggplot(data = coastline_cropped) +
        labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) +
        geom_tile(data = r_df, aes(x = lon, y = lat), color = col)  +
        #       scale_fill_manual(values =  RColorBrewer::brewer.pal(3, colorCombo)) +
        geom_sf(fill = NA, color = "gray", lwd = 0.1) +
        theme_bw() +
        theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
        theme(plot.title = element_text(size = 12, hjust = 0.5), plot.caption = element_text(hjust = 0, size = 6)) +
        theme(legend.text.align = 1)
      #        theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
      print(g)
      ggsave(filename = fileName_out, plot = g, width = 8, height = 4, units = "in", dpi = 300)
      knitr::plot_crop(fileName_out) # gets rid of margins around the plot
      print(paste0("file name out: ", fileName_out))
      g <- NULL
    }
  }
}

# threats graphics -----
for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    f_perennialClimateThreatsGraphics()
  }
}

# threats graphics, historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
f_perennialClimateThreatsGraphics()

# favorable locs graphics -----
for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (fruitSpecies in speciesChoice) {
      print(paste0(" favorable locs, fruitSpecies: ", fruitSpecies, ", ssp choice: ", k, ", start year: ", l))
      f_favorableLocsGraphics()
    }
  }
}

# favorable locs  graphics, historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (fruitSpecies in speciesChoice) {
  print(paste0(" favorable locs, fruitSpecies: ", fruitSpecies, ", ssp choice: ", k, ", start year: ", l))
  f_favorableLocsGraphics()
}

# powerpoint, threats -----
library(officer)
library(flextable)
library(magrittr)

defaultWidth <- 10
defaultHeight <- 4
defaultLeft <- 0
defaultTopNH <- 0.5
defaultTopSH <- 4

f_perennialStressPpt <- function(season) {
  if (season == "spring") fileNameStart <- paste0("springFrost_hem_")
  if (season == "summer") fileNameStart <- paste("summerHeat_hem_")
  fileName_NH <- paste0("graphics/cmip6/perennials/", fileNameStart, "NH", "_", k, "_", yearSpan, ".png")
  fileName_SH <- paste0("graphics/cmip6/perennials/", fileNameStart, "SH", "_", k, "_", yearSpan, ".png")
  
  extImg_NH <- external_img(src = fileName_NH, width = defaultWidth, height = defaultHeight)
  extImg_SH <- external_img(src = fileName_SH, width = defaultWidth, height = defaultHeight)
  
  my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
  my_pres <- ph_with(x = my_pres, value = extImg_NH, location = ph_location(left = defaultLeft, top = defaultTopNH, width = defaultWidth, height = defaultHeight - 0.5) )
  my_pres <- ph_with(x = my_pres, value = extImg_SH, location = ph_location(left = defaultLeft, top = defaultTopSH, width = defaultWidth, height = defaultHeight - 0.5) )
  return(my_pres)
}

# presentation text and figures 
titleString <- paste0("Spring Frost and Summer Heat Threat")
contentString <- paste0("Powerpoint produced on ", Sys.Date())

dataText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
dataText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/). "
dataText3 <- "It includes data from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three scenarios (ssp126, ssp370 and ssp585). In this powerpoint, only results using ssp 126 and ssp585 are presented. " 
dataText4 <- "The data from a 20 year period for the individual models are averaged for each month and a coefficient of variation across the 5 models is calculated. "
introText1 <- "There are two types of slides in this ppt – those that show the spring frost risk and the summer heat damage risk. These are defined in the table below. "
introText2 <- "Winter freeze and unsuitable areas, either because of frost and heat, are indicated by land areas that are white."

#dataText5 <- "Crop area is based on the SAGE cropping calendar data set, based in the early 2000s. Areas that are not cropped have an NA value and are displayed in white. Areas in gray have chilling less than the minimum requirements. Areas in yellow have chilling hours between the lower and upper range of the requirements."
dataText <- c(dataText1, dataText2, dataText3, dataText4) #, dataText5)

fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
fp_2 <- fp_text(bold = FALSE, font.size = 12)
fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)

blData <- block_list(
  #  fpar(ftext("hello world", fp_1)),
  fpar(
    ftext(dataText1, fp_2),
    ftext(dataText2, fp_2),
    ftext(dataText3, fp_2),
    ftext(dataText4, fp_2) #,
    #    ftext(dataText5, fp_2)
  ))

blIntro <- block_list(
  fpar(
    ftext(introText1, fp_2),
    ftext(introText2, fp_2)
  ))

my_pres <- read_pptx()
my_pres <- add_slide(x = my_pres, layout = 'Title Slide', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = titleString, location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(x = my_pres, value = contentString, location = ph_location_type(type = "subTitle"))

fileName_RTable <- paste0("graphics/cmip6/perennials/", "RebeccaTable.png")
extImg_Rtable <- external_img(src = fileName_RTable, width = defaultWidth, height = defaultHeight)
my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = extImg_Rtable, location = ph_location(left = defaultLeft, top = 3, width = 8, height = defaultHeight) )
my_pres <- ph_with(x = my_pres, value = blIntro, location = ph_location_type(type = "body") )

for (season in c("spring", "summer")) {
  if (season == "spring") ensembleTitle <- paste("Spring Frost Threat")
  if (season == "summer") ensembleTitle <- paste("Summer Heat Threat")
  my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
  my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
  
  # do historical first, then ssps and future periods
  k <- "historical"
  l <- 1991
  yearSpan <- paste0(l, "_", l + yearRange)
  my_pres <- f_perennialStressPpt(season)
  
  for (k in sspChoices) {
    for (l in startyearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      f_perennialStressPpt(season)
    }
  }
}

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Data Source", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = blData, location = ph_location_type(type = "body") )

print(my_pres, target = "presentations/cmip6/perennials/summerHeatandSpringFrost.pptx") %>% browseURL()


# powerpoint, favorable locations -----
library(officer)
library(flextable)
library(magrittr)

defaultWidth <- 10
defaultHeight <- 5
defaultLeft <- 0
defaultTop <- 1
# defaultTopSH <- 4

f_favorableLocsPpt <- function(fruitSpecies) {
  fileNameStart <- paste0(fruitSpecies, "_", i, "_locations_", m, "_")
  fileName_in <- paste0("graphics/cmip6/perennials/", fileNameStart, k, "_", yearSpan, ".png")
  extImg_favLocs <- external_img(src = fileName_in, width = defaultWidth, height = defaultHeight)
  my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
  my_pres <- ph_with(x = my_pres, value = extImg_favLocs, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )
  return(my_pres)
}

# presentation intro -----
titleString <- paste0("Adequate Chill Portions by Fruit, Time Period, and Scenario")
contentString <- paste0("Powerpoint produced on ", Sys.Date())

dataText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
dataText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/). "
dataText3 <- "It includes data from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three scenarios (ssp126, ssp370 and ssp585). In this powerpoint, only results using ssp 126 and ssp585 are presented. " 
dataText4 <- "The data from a 20 year period for the individual models are averaged for each month and a coefficient of variation across the 5 models is calculated. "
introText1 <- "These slides display locations where the three conditions for perennial crop success - adequate chill portions, and heat and freeze threat conditions are 'good, 'ok', or 'bad'. "
introText2 <- "The table below shows the crops we’re considering and the chill portion requirements used in the following graphs. "

#dataText5 <- "Crop area is based on the SAGE cropping calendar data set, based in the early 2000s. Areas that are not cropped have an NA value and are displayed in white. Areas in gray have chilling less than the minimum requirements. Areas in yellow have chilling hours between the lower and upper range of the requirements."
dataText <- c(dataText1, dataText2, dataText3, dataText4) #, dataText5)

fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
fp_2 <- fp_text(bold = FALSE, font.size = 12)
fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)

blData <- block_list(
  #  fpar(ftext("hello world", fp_1)),
  fpar(
    ftext(dataText1, fp_2),
    ftext(dataText2, fp_2),
    ftext(dataText3, fp_2),
    ftext(dataText4, fp_2) #,
    #    ftext(dataText5, fp_2)
  ))

blIntro <- block_list(
  fpar(
    ftext(introText1, fp_2),
    ftext(introText2, fp_2)
  ))

my_pres <- read_pptx()
my_pres <- add_slide(x = my_pres, layout = 'Title Slide', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = titleString, location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(x = my_pres, value = contentString, location = ph_location_type(type = "subTitle"))

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, head(CPs), location = ph_location(left = 2.5, top = 2.5, width = 4, height = 3) )

my_pres <- ph_with(x = my_pres, value = blIntro, location = ph_location_type(type = "body") )

for (fruitSpecies in speciesChoice) {
  ensembleTitle <- paste("Favorable locations for ", fruitSpecies)
  my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
  my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
  
  for (i in c("good", "ok", "bad")) {
    for (m in hemisphere) {
      
      # do historical first, then ssps and future periods
      k <- "historical"
      l <- 1991
      yearSpan <- paste0(l, "_", l + yearRange)
      my_pres <- f_favorableLocsPpt(fruitSpecies)
      
      for (k in sspChoices) {
        for (l in startyearChoices) {
          yearSpan <- paste0(l, "_", l + yearRange)
          my_pres <- f_favorableLocsPpt(fruitSpecies)
        }
      }
    }
  }
}

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Data Source", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = blData, location = ph_location_type(type = "body") )

print(my_pres, target = "presentations/cmip6/perennials/adequateChillPortions.pptx") %>% browseURL()

# runs testing ------
f_runs <- function(x) {
  runResult <- c(NA, NA) 
  if (is.nan(x[1])) {
    return(runResult)
  }
  seqLengthCode <- paste0("1{", test_length, ",}") #A regular expression  to get the first item of gregexpr. It says look for  test_length times See http://xenon.stanford.edu/~xusch/regexp/
  g <- gregexpr(seqLengthCode, paste(+eval(parse(text = test_logic)), collapse = ""))[[1]] # The + converts TRUE and FALSE to 1 and 0
  if (!(g[1] == -1)) { # no need to write to growing season if g returns -1, return 0,0
    startDays <- unlist(g)
    runLengths <- sum(as.numeric(attributes(g)$match.length))
    runResult <- c(length(startDays), runLengths)
  } else {
    runResult <- c(0, 0) 
  }
  return(runResult)
}

test_logic <- "x > 35"
test_length <- 10

for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_in <- paste0("data/bigFiles/ensemble_dyMean20yr_", k, "_tasmax_global_daily_", yearSpan, ".tif")
    r <- rast(fileName_in)
    print(system.time(r_runs <- app(r, f_runs)))
    fileName_out <- paste0("data/cmip6/tmaxRuns/run_10_lim_gt35_ensemble_tmax_", k, "_", yearSpan, ".tif")
    print(system.time(writeRaster(r_runs, filename = fileName_out,  overwrite = TRUE, format = "GTiff", wopt= woptList))); flush.console()
    
  }
}
yearSpan_end_century <- "2081_2100"
yearSpan_mid_century <- "2041_2060"
temp_ssp585_end <- rast(paste0("data/cmip6/tmaxRuns/run_10_lim_gt35_ensemble_tmax_", k, "_", yearSpan_end_century, ".tif"))
temp_ssp585_mid <- rast(paste0("data/cmip6/tmaxRuns/run_10_lim_gt35_ensemble_tmax_", k, "_", yearSpan_mid_century, ".tif"))
plot(temp_ssp585_end$lyr.1, main = "Number of tmax runs, minimum length of 10 days, tmax > 35\nSSP585, end century")
plot(temp_ssp585_end$lyr.2, main = "Tmax number of days in runs, minimum length of 10 days, tmax > 35\nSSP585, end century")
plot(temp_ssp585_mid$lyr.1, main = "Number of tmax runs, minimum length of 10 days, tmax > 35, SSP585\nmid century")
plot(temp_ssp585_mid$lyr.2, main = "Tmax number of days in runs, minimum length of 10 days, tmax > 35\nSSP585, mid century")


# f_heatDamage <- function() {
#   # does extreme cold locations and frost and heat damage locations
#   combined <- sds()
#   
#   fileName_tasmax <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr_", k, "_tasmax_", yearSpan, ".tif")
#   tmax <- rast(fileName_tasmax)
#   for (m in hemisphere) {
#     tmin_hem <- crop(tmin, get(paste0("extent_", m)))
#     tmax_hem <- crop(tmax, get(paste0("extent_", m)))
#     spStart <- paste0("springStart_", m)
#     spEnd <- paste0("springEnd_", m)
#     spLyrs <- paste0(get(spStart), ":", get(spEnd))
#     hdStart <- paste0("heatDamageStart_", m)
#     hdEnd <- paste0("heatDamageEnd_", m)
#     hdLyrs <- paste0(get(hdStart), ":", get(hdEnd))
#     tmin_hem <- subset(tmin_hem, get(spStart):get(spEnd))
#     tmax_hem <- subset(tmax_hem, get(hdStart):get(hdEnd))
#     # count number of days tmin is below tminDamageVal
#     frostCt <- sum(tmin_hem < tminDamageVal, na.rm = TRUE)
#     heatCt <- sum(tmax_hem > tmaxDamageVal, na.rm = TRUE)
#     # fileNameStart_frost <- paste0("frostCt_hem_")
#     # fileName_out_frostDam <- paste0("data/cmip6/perennials/", fileNameStart_frost, m, "_", k, "_", yearSpan, ".tif")
#     # print(system.time(writeRaster(frostCt, fileName_out_frostDam, overwrite = TRUE, woptList = woptList)))
#     # fileNameStart_heat <- paste0("heatCt_hem_")
#     # fileName_out_heatDam <- paste0("data/cmip6/perennials/", fileNameStart_heat, m, "_", k, "_", yearSpan, ".tif")
#     # print(system.time(writeRaster(frostCt, fileName_out_heatDam, overwrite = TRUE, woptList = woptList)))
#     # 
#     for (i in c("good", "ok", "bad")) {
#       frDays <- switch(i,
#                        "good" = frostRiskDays[1:2],
#                        "ok" = frostRiskDays[3:4],
#                        "bad" = frostRiskDays[5:6],
#       )
#       fr <- f_range(frostCt, frDays)
#       hdDays <- switch(i,
#                        "good" = heatRiskDays[1:2],
#                        "ok" = heatRiskDays[3:4],
#                        "bad" = heatRiskDays[5:6],
#       )
#       hr <- f_range(heatCt, hdDays)
#     }
#     # combined <- sds(combined, get(paste0("fr_", m, i)))
#     # combined <- sds(combined, get(paste0("hr_", m, i)))
#     combined <- sds(lapply(combined, function(i) eval(parse(text = paste0("fr_", m, i)))))
#     combined <- sds(lapply(combined, function(i) eval(parse(text = paste0("hr_", m, i)))))
#     
#   }
#   return(combined)
# }

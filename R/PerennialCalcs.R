# code to do various perennial crop calculations
{source("R/globallyUsed.R")
  terraOptions(memfrac = 2, progress = 0, tempdir =  "data/ISIMIP", verbose = FALSE)
  locOfClimFiles <- "data/bigFiles/"
  locOfDataFiles <- "data/cmip6/perennials/"
  locOfCPFiles <- "data/cmip6/chillPortions/chill_portions/"
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
  locOfCPFiles <- "data/cmip6/chillPortions/chill_portions/"
  speciesChoice <- c("cherry", "almond", "winegrape", "apple") #, "olive", "berries") 
  CPs <- read_excel(paste0("data-raw/crops/", "fruitCPs.xlsx"))
  
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
  
  f_coldFrostHeatDamage <- function(k, l, fruitSpecies) {
    # does extreme cold locations and frost and heat damage locations
    combined <- sds()
    fileName_tasmin <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr_", k, "_tasmin_", yearSpan, ".tif")
    fileName_tasmax <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr_", k, "_tasmax_", yearSpan, ".tif")
    tmin <- rast(fileName_tasmin)
    tmax <- rast(fileName_tasmax)
    for (m in hemisphere) {
      # do extreme cold locations - a bad day is 1
      extremeColdCt <- sum(tmin < tminExtremeVal, na.rm = TRUE)
      extremeColdCt[extremeColdCt > 0] <- 1 # one day is all it takes
      extremeColdMask_hem <- crop(extremeColdCt, get(paste0("extent_", m)))
      
      # species-specific chill portion
      fileNameCP_in <- paste0(locOfCPFiles, "ensemble_chill_cutoff_", fruitSpecies, "_", k, "_", m, "_", yearSpan, ".tif")
      chillPortionsCutoff <- rast(fileNameCP_in)
      print(paste0("fileNameCP_in: ", fileNameCP_in))
      # if (m == "NH") extremeColdMask <- crop(extremeCold, extent_NH)
      # if (m == "SH") extremeColdMask <- crop(extremeCold, extent_SH)
      # 
      #chillPortionsCutoff[chillPortionsCutoff == 0] <- NA    
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
      # count number of days tmin is below tminDamageVal (freezing)
      frostCt <- sum(tmin_hem < tminDamageVal, na.rm = TRUE) # each cell with a value is where days with frost are less than the number of days where permanent damage occurs
      heatCt <- sum(tmax_hem > tmaxDamageVal, na.rm = TRUE) # each cell with a value is where days with high heat are less than the number of days were heat damage occurs
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
        unsuitableFrost[unsuitableFrost >= unsuitable_springFreezeDays] <- 0 # locations where the number of frost days in spring is greater than the suitable cutoff, 0 is unsuitable, 1 is suitable
        
        unsuitableHeat <- heatCt
        unsuitableHeat[unsuitableHeat < unsuitable_summerHotDays] <- 1
        unsuitableHeat[unsuitableHeat >= unsuitable_summerHotDays] <- 0 # locations where the number of hot days in summer is greater than the suitable cutoff, 0 is unsuitable, 1 is suitable
        
        
        #   assign(paste0("fr_", m, "_", i), fr)
        #   assign(paste0("hr_", m, "_", i), hr)
        # combined <- sds(combined, get(paste0("fr_", m, i)))
        #      holder <- c("extremeColdMask_hem", paste0("fr_",  m, "_", i), paste0("hr_",  m, "_", i))
        holder <- c("extremeColdMask_hem", "unsuitableFrost", "fr", "unsuitableHeat", "hr", "chillPortionsCutoff") # the names and order in which the rasters are stored in the sds
        combined <- sds(lapply(holder,  function(x) eval(parse(text = x))))
        print(combined)
        varnames <- c("extremeCold", "unsuitableFrost", "frostCt", "unsuitableHeat", "heatCt", "chillPortions")
        fileName_out <- paste0(locOfDataFiles, "combinedDamages_", fruitSpecies, "_", m, "_", k, "_", i, "_", yearSpan, ".nc")
        writeCDF(combined, filename = fileName_out,  overwrite = TRUE, missval=-9999, prec="float", compression=5)
        print(paste0("file name out: ", fileName_out))
      }
    }
  }
  f_combinedDamage <- function(k, l, fruitSpecies) {
    for (m in hemisphere) {
      for (i in c("good", "ok", "bad")) {
        fileName_in <- paste0(locOfDataFiles, "combinedDamages_", fruitSpecies, "_", m, "_", k, "_", i, "_", yearSpan, ".nc")
        #     combined <- rast(fileName_in)
        extremeColdCt <- rast(fileName_in, 1) # 1 is bad, 0 is ok
        setMinMax(extremeColdCt)
        extremeColdCt[extremeColdCt == 1] <- NA
        unsuitableFrost <- rast(fileName_in, 2) # 0 is bad, 1 is ok
        setMinMax(unsuitableFrost)
        frostCt <- rast(fileName_in, 3) # number of frost days below the damage threshold of zero; locations outside the range are NA; outside land is NaN
        setMinMax(frostCt)
        unsuitableHeat <-rast(fileName_in, 4) #0 is bad, 1 is ok
        setMinMax(unsuitableHeat)
        heatCt <- rast(fileName_in, 5) # number of high heat days below the damage threshold; locations outside the range are NA; outside land is NaN
        setMinMax(heatCt)
        chillPortionsCutoff <- rast(fileName_in, 6) # 1 is good, 0 is bad
        setMinMax(chillPortionsCutoff)
        
        print(paste0("working on combined damage ", fruitSpecies, " in hemisphere ", m, ", year ", l, ", scenario ", k))
        
        # if (m == "NH") extremeColdMask <- crop(extremeColdCt, extent_NH) # these come in already by hemisphere
        # if (m == "SH") extremeColdMask <- crop(extremeColdCt, extent_SH)
        
        #combine them all
        # start with locations where chillPortions are adequate
        # chillPortionsCutoff[chillPortionsCutoff == 0] <- NA
        r <- chillPortionsCutoff # 1 is where chill portions is adequate 
        print(r)
        plot(r, main = paste0("locations where chill portions are adequate for ", fruitSpecies))
        print(extremeColdCt)
        plot(extremeColdCt, main = paste0("extreme Cold Mask (locs with value of 1 have temp less than ", tminExtremeVal, " for at least one day)"))
        r <- mask(r, extremeColdCt) # 0 is no extreme cold; 1 is extreme cold, this sets masked areas to NA
        plot(r, main = "chillPortionsCutoff masked with extremeCold Mask")
        r <- mask(r, unsuitableFrost, maskvalue = 0)
        plot(r, main = "chillPortionsCutoff masked with \nextremeCold and unsuitable frost masks")
        frostCt[frostCt > 1] <- 1
        r <- mask(r, frostCt, maskvalue = 1) # 1 is where frost Ct is below the unsuitable level
        plot(r, main = "chillPortions adequate masked with extremeCold and \nfrost Ct is in the suitable range")
        heatCt[heatCt > 1] <- 1
        r <- mask(r, heatCt, maskvalue = 1) # 1 is where heat Ct is below the unsuitable level
        plot(r, main = "chillPortionsCutoff masked with extremeCold Mask and \nfrost Ct and heat ct are in the suitable ranges")
        r[r == 0] <- NA
        fileName_out <- paste0("data/cmip6/perennials/suitable_", fruitSpecies, "_", m, "_", k, "_", i, "_", yearSpan, ".tif")
        writeRaster(r, fileName_out, overwrite = TRUE, woptList = woptList)
        print(paste0("file name out: ", fileName_out))
        
        # r <- mask(r, frostCt, maskvalue = 1) # 1 is where frost Ct is below the unsuitable level
        # col <- defaultColor
        # titleText = "Locations where chill portions are adequate and \nthe number of spring frost days is in the suitable range"
        # fileName_out <- paste0("graphics/cmip6/perennials/demoSlide_springFrostGood", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
        # f_graphics_demoSlides(r, titleText, caption, fileName_out, col)
        # 
        # r <- mask(r, heatCt, maskvalue = 1) # 1 is where heat Ct is above the unsuitable level
        # 
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
  
  f_suitableLocsGraphics <- function(k, yearSpan, fruitSpecies) {
    legendTitle <- "Suitable"
    
    #   for (m in hemisphere) {
    # crop out areas where summer heat or spring frost are greater than the unsuitable values, either unsuitable_springFreezeDays or unsuitable_summerHotDays
    CPfruit <- CPs$chillRequirement[CPs$crop==fruitSpecies]
    for (i in c("good", "ok", "bad")) {
      titleText <- paste0("Growing conditions for ", fruitSpecies," are suitable;  quality:  ", i, "\n" , "scenario: ", k, ", period: ", gsub("_", "-", yearSpan))
      if (i == "good") {
        caption <- paste0("Note: Good growing conditions for ", fruitSpecies, " include chill portions of at least ", CPfruit, ", less than ", frostRiskDays[3], 
                          " days of spring frost risk and \nless than ", heatRiskDays[3], " days of summer excessive heat. Gray hatching indicates early 21st century area.")
        col = "green"}
      if (i == "ok") {
        caption <- paste0("Note: Ok growing conditions for ", fruitSpecies, " include chill portions of at least ", CPfruit, ", ", frostRiskDays[3], "-",  frostRiskDays[4], 
                          " days of spring frost risk and \n",   heatRiskDays[3], "-",  heatRiskDays[4], " days of summer excessive heat. Gray hatching indicates early 21st century area.")
        col = "yellow"}
      if (i == "bad") {
        caption <- paste0("Note: Bad growing conditions for ",fruitSpecies, " include chill portions of at least ", CPfruit, ", ", frostRiskDays[5], "-",  frostRiskDays[6], 
                          " days of spring frost risk and \n",  heatRiskDays[5], "-",  heatRiskDays[6], " days of summer excessive heat. Gray hatching indicates early 21st century area.")
        col = "red"}
      
      fileName_in_NH <- paste0("data/cmip6/perennials/suitable_", fruitSpecies, "_", "NH", "_", k, "_", i, "_", yearSpan, ".tif")
      fileName_in_SH <- paste0("data/cmip6/perennials/suitable_", fruitSpecies, "_", "SH", "_", k, "_", i, "_", yearSpan, ".tif")

      r_nh <- rast(fileName_in_NH)
      r_sh <- rast(fileName_in_SH)
      
      # now get harvested area map
      fruit_in <- fruitSpecies
      if (fruitSpecies == "winegrape") fruit_in <- "grape"
      rInArea <- rast(paste0(tifFileLoc, fruit_in,"/",  fruit_in, "_HarvestedAreaHectares.tif"))
      harvestArea <- aggregate(rInArea, fact = 6, fun = "sum") # convert 5 arc minutes to 1/2 degrees
      harvestArea[harvestArea == 0] <- NA
      harvestArea[harvestArea > 0] <- 1
      
      r <- merge(r_nh, r_sh)
      
      r <- project(r, crsRob)
      harvestArea <- project(harvestArea, crsRob)
      r_df <- as.data.frame(r, xy = TRUE)
      names(r_df) <-   c("x", "y", "value")
      harvestArea_df <- as.data.frame(harvestArea, xy = TRUE)
      names(harvestArea_df) <-   c("x", "y", "value")
      fileName_out <- paste0("graphics/cmip6/perennials/", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
      
      # g <- ggplot(data = coastline_cropped) +
      #    geom_sf(fill = NA, color = "gray", lwd = 0.1) +
      #     theme(plot.title = element_text(size = 12, hjust = 0.5), plot.caption = element_text(hjust = 0, size = 6)) +
      # 
      
      # do without legend
      g <- ggplot() +
        geom_tile(data = r_df, aes(x, y), fill = col) +
        #              scale_fill_discrete(colors = col, drop = FALSE, na.value = 'grey95') +
        #scale_fill_manual(values = col, drop = FALSE, na.value = 'grey95') + # the na.value doesn't work yet. See https://stackoverflow.com/questions/45144630/scale-fill-manual-define-color-for-na-values/45147172 for a possible solution
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
                position = "identity") +
        geom_tile(data = harvestArea_df, aes(x, y, fill = value), alpha = .2, show.legend = FALSE) # +
     #   scale_fill_gradient(low = "gray", high = "gray")
    #    scale_fill_grey()
      
      print(g)
      ggsave(filename = fileName_out, plot = g, width = 8, height = 4, units = "in", dpi = 300)
      knitr::plot_crop(fileName_out) # gets rid of margins around the plot
      print(paste0("file name out: ", fileName_out))
      g <- NULL
    }
  }
  #}
  f_suitableLocsPpt <- function(k, l, yearSpan, i) {
    fileName_in <- paste0("graphics/cmip6/perennials/", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
    extImg <- external_img(src = fileName_in, width = defaultWidth, height = defaultHeight)
    my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
    my_pres <- ph_with(x = my_pres, value = extImg, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5))
    return(my_pres)
  }
}

# use the functions -----
for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (fruitSpecies in speciesChoice) {
      print(system.time(combined <- f_coldFrostHeatDamage(k, l, fruitSpecies))) # combined has three temperature-related spatrasters - hard freeze, frost, and heat. Each combined output file has all locations that are good, ok, and bad for frost and heat damage
    }
  }
}

#  historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (fruitSpecies in speciesChoice) {
  print(system.time(combined <- f_coldFrostHeatDamage(k, l, fruitSpecies))) # combined has 5 temperature-related spatrasters - hard freeze, unsuitable frost days, acceptable frost days, and heat. Each combined output file has all locations that are good, ok, and bad for frost and heat damage
}

# combined damage -----
# code to read in chill portions, cold, freeze and heat stress 1/0 files and produce 1/0 tifs where the crop is potentially growable. The chill portions files are created in the chillPortions.R script

for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (fruitSpecies in speciesChoice) {
      print(paste0("fruitSpecies: ", fruitSpecies, ", ssp choice: ", k, ", start year: ", l))
      f_combinedDamage(k, l, fruitSpecies) 
    }
  }
}

# combined damage, historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (fruitSpecies in speciesChoice) {
  print(paste0("fruitSpecies: ", fruitSpecies, ", ssp choice: ", k, ", start year: ", l))
  f_combinedDamage(k, l, fruitSpecies) 
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



# frostRiskDays <- c(0,4,5,6,20,21, 45) # pairs for ranges - good, ok, bad
# heatRiskDays <-  c(0,9,10,30,31, 45) # pairs for ranges - good, ok, bad

# # threats graphics -----
# for (k in sspChoices) {
#   for (l in startyearChoices) {
#     yearSpan <- paste0(l, "_", l + yearRange)
#     f_perennialClimateThreatsGraphics()
#   }
# }
# 
# # threats graphics, historical -----
# k <- "historical"
# l <- 1991
# yearSpan <- paste0(l, "_", l + yearRange)
# f_perennialClimateThreatsGraphics()

#test data ------
k = "ssp126"
l = 2041
i = "good"
fruitSpecies = "cherry"
m = "NH"

#suitable locations, scenario graphics -----
for (k in sspChoices) {
  for (l in startyearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (fruitSpecies in speciesChoice) {
      print(paste0(" favorable locs, fruitSpecies: ", fruitSpecies, ", ssp choice: ", k, ", start year: ", l))
      f_suitableLocsGraphics(k, yearSpan, fruitSpecies)
    }
  }
}

#suitable locations, historical graphics -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (fruitSpecies in speciesChoice) {
  print(paste0(" favorable locs, fruitSpecies: ", fruitSpecies, ", ssp choice: ", k, ", start year: ", l))
  f_suitableLocsGraphics(k, yearSpan, fruitSpecies)
}

# demo slides ------
f_graphics_demoSlides <- function(r, titleText, caption, fileName_out, col) {
  r <- project(r, crsRob, method = "near")
  r_df <- as.data.frame(r, xy = TRUE)
  names(r_df) <-   c("x", "y", "value")
  r_df$value <- as.factor(r_df$value)
  
  print(paste0("file name out: ", fileName_out))
  g <- ggplot() +
    geom_tile(data = r_df, aes(x, y,  fill = value), show.legend = FALSE) +
    #              scale_fill_discrete(colors = col, drop = FALSE, na.value = 'grey95') +
    scale_fill_manual(values = col) +
    # the na.value doesn't work yet. See https://stackoverflow.com/questions/45144630/scale-fill-manual-define-color-for-na-values/45147172 for a possible solution
    labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + 
    theme_bw()  +
    #      theme(plot.title = element_text(size = 12, hjust = 0.5), plot.caption = element_text(hjust = 0, size = 8)) +
    theme(
      legend.position = "none",
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
  ggsave(filename = fileName_out, plot = g, width = 8, height = 4, units = "in", dpi = 300)
  knitr::plot_crop(fileName_out) # gets rid of margins around the plot
}

k = "ssp126"
l = 2041
yearSpan <- paste0(l, "_", l + yearRange)
fruitSpecies <- "cherry"
i = "good"
legendTitle <- "Suitable"
defaultColor <- c("green", "red")
CPfruit <- CPs$chillRequirement[CPs$crop==fruitSpecies]
if (i == "good") {
  caption <- paste0("Note: ", i, " growing conditions for ", fruitSpecies, " include chill portions of at least ", CPfruit, ", ", frostRiskDays[3], "-",  frostRiskDays[4], 
                    " days of spring frost risk and \n",   heatRiskDays[3], "-",  heatRiskDays[4], " days of summer excessive heat.")
}

fileName_in_NH <- paste0(locOfDataFiles, "combinedDamages_", fruitSpecies, "_", "NH", "_", k, "_", i, "_", yearSpan, ".nc")
fileName_in_SH <- paste0(locOfDataFiles, "combinedDamages_", fruitSpecies, "_", "SH", "_", k, "_", i, "_", yearSpan, ".nc")
extremeColdCt_NH <- rast(fileName_in_NH, 1) # 1 is bad, 0 is ok
unsuitableFrost_NH <- rast(fileName_in_NH, 2) #0 is bad, 1 is ok
frostCt_NH <- rast(fileName_in_NH, 3) # number of frost days below the damage threshold
unsuitableHeat_NH <-rast(fileName_in_NH, 4) #0 is bad, 1 is ok
heatCt_NH <- rast(fileName_in_NH, 5) # number of high heat days below the damage threshold
chillPortionsCutoff_NH <- rast(fileName_in_NH, 6) # 1 is good, 0 is bad

extremeColdCt_SH <- rast(fileName_in_SH, 1) # 1 is bad, 0 is ok
unsuitableFrost_SH <- rast(fileName_in_SH, 2) #0 is bad, 1 is ok
frostCt_SH <- rast(fileName_in_SH, 3) # number of frost days below the damage threshold
unsuitableHeat_SH <-rast(fileName_in_SH, 4) #0 is bad, 1 is ok
heatCt_SH <- rast(fileName_in_SH, 5) # number of high heat days below the damage threshold
chillPortionsCutoff_SH <- rast(fileName_in_SH, 6) # 1 is good, 0 is bad

extremeColdCt <- merge(extremeColdCt_NH,  extremeColdCt_SH)
extremeColdCt[extremeColdCt == 1] <- NA
unsuitableFrost <- merge(unsuitableFrost_NH, unsuitableFrost_SH)
frostCt <- merge(frostCt_NH, frostCt_SH)
frostCt[frostCt > 1] <- 1
unsuitableHeat <- merge(unsuitableHeat_NH, unsuitableHeat_SH)
heatCt <- merge(heatCt_NH, heatCt_SH)
heatCt[heatCt > 1] <- 1
chillPortionsCutoff <- merge(chillPortionsCutoff_NH, chillPortionsCutoff_SH)

r <- extremeColdCt
col <- defaultColor
titleText <- paste0("Extreme Cold Mask (minimum temperature is less than ", tminExtremeVal, " for at least one day). \nGreen areas don't have this cold.")
fileName_out <- paste0("graphics/cmip6/perennials/demoSlide_extremeColdMask", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
f_graphics_demoSlides(r, titleText, caption, fileName_out, col)

r <- chillPortionsCutoff # 1 is where chill portions is adequate 
col <- rev(defaultColor)
titleText <- paste0("Locations where chill portions are adequate for ", fruitSpecies, ", scenario: ", k, ", year span: ", yearSpan)
fileName_out <- paste0("graphics/cmip6/perennials/demoSlide_chillPortionsCutoff", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
f_graphics_demoSlides(r, titleText, caption, fileName_out, col)

{# these next lines of code must stay in this order.
  r <- mask(chillPortionsCutoff, extremeColdCt, maskvalue = 1) # 0 is no extreme cold; 1 is extreme cold, this sets masked areas to NA
  col <- rev(defaultColor)
  titleText <- "Locations where the chill portions are not affected by extreme cold"
  fileName_out <- paste0("graphics/cmip6/perennials/demoSlide_chillPortionsGood", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
  f_graphics_demoSlides(r, titleText, caption, fileName_out, col)
  
  r <- mask(r, frostCt, maskvalue = 1) # 1 is where frost Ct is below the unsuitable level
  col <- rev(defaultColor)
  titleText = "Locations where chill portions are adequate and \nthe number of spring frost days is in the suitable range"
  fileName_out <- paste0("graphics/cmip6/perennials/demoSlide_springFrostGood", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
  f_graphics_demoSlides(r, titleText, caption, fileName_out, col)
  
  r <- mask(r, heatCt, maskvalue = 1) # 1 is where heat Ct is above the unsuitable level
  r[r == 0] <- NA
  col <- defaultColor
  titleText = "Locations were chill portions are adequate, the numbers of spring frost \nand summer hot days are in suitable ranges."
  fileName_out <- paste0("graphics/cmip6/perennials/demoSlide_summerHeatBad", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
  f_graphics_demoSlides(r, titleText, caption, fileName_out, col)}

r <- heatCt
col <- defaultColor
titleText = "Locations in red are where the number of summer hot days are not suitable."
fileName_out <- paste0("graphics/cmip6/perennials/demoSlide_summerHeatGood", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
f_graphics_demoSlides(r, titleText, caption, fileName_out, col)

# powerpoint, suitable regions -----
{library(officer)
  library(flextable)
  library(magrittr)
  
  # for two stacked graphs such and NH and SH
  # defaultWidth <- 10
  # defaultHeight <- 4
  # defaultLeft <- 0
  # defaultTopNH <- 0.5
  # defaultTopSH <- 4
  
  # for one global graph
  defaultWidth <- 9
  defaultHeight <- 7
  defaultLeft <- .5
  defaultTop <- 1
  
  
  # f_perennialStressPpt <- function(season) {
  #   if (season == "spring") fileNameStart <- paste0("springFrost_hem_")
  #   if (season == "summer") fileNameStart <- paste("summerHeat_hem_")
  #   fileName_NH <- paste0("graphics/cmip6/perennials/", fileNameStart, "NH", "_", k, "_", yearSpan, ".png")
  #   fileName_SH <- paste0("graphics/cmip6/perennials/", fileNameStart, "SH", "_", k, "_", yearSpan, ".png")
  #   
  #   extImg_NH <- external_img(src = fileName_NH, width = defaultWidth, height = defaultHeight)
  #   extImg_SH <- external_img(src = fileName_SH, width = defaultWidth, height = defaultHeight)
  #   
  #   my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
  #   my_pres <- ph_with(x = my_pres, value = extImg_NH, location = ph_location(left = defaultLeft, top = defaultTopNH, width = defaultWidth, height = defaultHeight - 0.5) )
  #   my_pres <- ph_with(x = my_pres, value = extImg_SH, location = ph_location(left = defaultLeft, top = defaultTopSH, width = defaultWidth, height = defaultHeight - 0.5) )
  #   return(my_pres)
  # }
  
  # presentation text and figures 
  titleString <- paste0("Suitable growing regions for perennial crops")
  contentString <- paste0("Powerpoint produced on ", Sys.Date())
  
  dataText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
  dataText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/). "
  dataText3 <- "It includes data from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three scenarios (ssp126, ssp370 and ssp585). In this powerpoint, only results using ssp 126 and ssp585 are presented. " 
  dataText4 <- "The data from a 20 year period for the individual models are averaged for each month and a coefficient of variation across the 5 models is calculated. "
  #dataText5 <- "Crop area is based on the SAGE cropping calendar data set, based in the early 2000s. Areas that are not cropped have an NA value and are displayed in white. Areas in gray have chilling less than the minimum requirements. Areas in yellow have chilling hours between the lower and upper range of the requirements."
  dataText <- c(dataText1, dataText2, dataText3, dataText4) #, dataText5)
  
  introText1 <- "Temperature perennial fruits face four weather-based challenges - temperature extremes of cold and heat can kill the plant, a period of time during the winter season where temperatures are below freezing (a chilling period) is required, and periods during the growing season where high temperatures can cause damage in critical growth periods. These challenges, and specific values, are described in more detail in the table below."
  introText2 <- "The initial slides in this ppt show how these weather-based challenges combine to identify locations across the globe today.  The remaining slides are specific to individual perennial fruits and show in the future under different climate scenarios where the combined conditions result in suitability for each species with good, ok, and bad likelihood of success."
  
  fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
  fp_2 <- fp_text(bold = FALSE, font.size = 12)
  fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)
  
  blIntro <- block_list(
    fpar(
      ftext(introText1, fp_2)),
    fpar(),
    fpar(
      ftext(introText2, fp_2)
    ))
  
  blData <- block_list(
    #  fpar(ftext("hello world", fp_1)),
    fpar(
      ftext(dataText1, fp_2),
      ftext(dataText2, fp_2),
      ftext(dataText3, fp_2),
      ftext(dataText4, fp_2) #,
      #    ftext(dataText5, fp_2)
    ))
}

my_pres <- read_pptx()
my_pres <- add_slide(x = my_pres, layout = 'Title Slide', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = titleString, location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(x = my_pres, value = contentString, location = ph_location_type(type = "subTitle"))

fileName_RTable <- paste0("graphics/cmip6/perennials/", "RebeccaTable.png")
extImg_Rtable <- external_img(src = fileName_RTable, width = defaultWidth, height = defaultHeight)
my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = extImg_Rtable, location = ph_location(left = defaultLeft, top = defaultHeight/2, width = 8, height = 3) )
my_pres <- ph_with(x = my_pres, value = blIntro, location = ph_location_type(type = "body") )

# add graphics for the demo slides
extremeColdMask <- external_img(src =  "graphics/cmip6/perennials/demoSlide_extremeColdMaskcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
chillPortionsCutoff <- external_img(src =  "graphics/cmip6/perennials/demoSlide_chillPortionsCutoffcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
chillPortions <- external_img(src =  "graphics/cmip6/perennials/demoSlide_chillPortionsGoodcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
springFrost <- external_img(src =  "graphics/cmip6/perennials/demoSlide_springFrostGoodcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
summerHeatBad <- external_img(src =  "graphics/cmip6/perennials/demoSlide_summerHeatBadcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
summerHeatGood <- external_img(src =  "graphics/cmip6/perennials/demoSlide_summerHeatGoodcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)

# demo slides in ppt -----
my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = "Demonstration Slides", location = ph_location_type(type = "title"))

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = extremeColdMask, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = chillPortionsCutoff, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = chillPortions, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = springFrost, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = summerHeatGood, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = summerHeatBad, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

ensembleTitle <- paste("Suitable locations for perennial fruits")
ensembleBody <- "Results for each quality level are presented sequentially; ie 'good locations for all species, followed by 'ok' locations for all species, and then 'bad' locations for all species."
my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = ensembleBody, location = ph_location_type(type = "body"))


# do historical first, then ssps and future periods
for (i in c("good", "ok", "bad")) {
  
  for (fruitSpecies in speciesChoice) {
    ensembleTitle <- paste("Suitable locations for ", fruitSpecies, ", Locations quality: ", i)
    my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
    my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
    
    k <- "historical"
    l <- 1991
    yearSpan <- paste0(l, "_", l + yearRange)
    my_pres <- f_suitableLocsPpt(k, l, yearSpan, i)
    
    for (k in sspChoices) {
      for (l in startyearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        my_pres <- f_suitableLocsPpt(k, l, yearSpan, i)
      }
    }
  }
}

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Data Sources", location = ph_location_type(type = "title"))
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
  ensembleTitle <- paste("Suitable locations for ", fruitSpecies)
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

# f_perennialClimateThreatsGraphics <- function() {
#   # get extreme cold mask
#   fileNameStart <- paste0("extremeColdlocs_", gsub("-", "", tminExtremeVal))
#   extremeColdMask <- paste0("data/cmip6/perennials/", fileNameStart, "_", k, "_", yearSpan, ".tif")
#   extremeColdMask <- rast(extremeColdMask)
#   extremeColdMask_NH <- crop(extremeColdMask, extent_NH)
#   extremeColdMask_SH <- crop(extremeColdMask, extent_SH)
#   
#   for (m in hemisphere) {
#     # crop out areas where summer heat or spring frost are greater than the unsuitable values, either unsuitable_springFreezeDays or unsuitable_summerHotDays
#     coastline_cropped <- f_crop_custom(coastline, get(paste0("extent_", m)))
#     coastline_cropped <- st_transform(coastline_cropped, crsRob)
#     
#     for (n in c("spring", "summer")) {
#       if(n %in% "spring") { 
#         fileNameStart <- paste0("springFrost_hem_")
#         fileName_in <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", k, "_", yearSpan, ".tif")
#         print(paste0("fileName_in: ", fileName_in))
#         r <- rast(fileName_in)
#         # crop out areas where temp is below extreme level
#         r <- mask(r, get(paste0("extremeColdMask_", m)), maskvalue = 1)
#         
#         r[r > unsuitable_springFreezeDays] <- NA
#         titleText <- paste0("Spring frost danger, scenario ", k, ", period ", gsub("_", "-", yearSpan))
#         # get maximum value for spring, I'm guessing it is in the historical period
#         fileName_in_max <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", "historical", "_", "1991_2010", ".tif")
#         maxVal <- ceiling(max(minmax(rast(fileName_in_max))))
#         coldDamageLimits <- c(0, 5, 20, unsuitable_springFreezeDays)
#         bins <- coldDamageLimits
#         caption <- paste0("Note: NA is oceans and land areas that are not suitable because of extreme cold \n(less than ", tminExtremeVal, "°C or more than ", unsuitable_springFreezeDays, " frost days in spring).")
#       }
#       
#       if(n %in% "summer")  {
#         fileNameStart <- paste0("summerHeat_hem_")
#         fileName_in <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", k, "_", yearSpan, ".tif")
#         print(paste0("fileName_in: ", fileName_in))
#         r <- rast(fileName_in)
#         # crop out areas where temp is below extreme level
#         r <- mask(r, get(paste0("extremeColdMask_", m)), maskvalue = 1)
#         
#         r[r > unsuitable_summerHotDays] <- NA # unsuitable_summerHotDays is number of days where tmax is greater than tmaxDamageVal
#         titleText <- paste0("Summer heat danger, scenario ", k, ", period ", gsub("_", "-", yearSpan))
#         # get maximum value for summer, I'm guessing it is in the ssp585 at end century 
#         fileName_in_max <- paste0("data/cmip6/perennials/", fileNameStart, m, "_", "ssp585", "_", "2081_2100", ".tif")
#         maxVal <- ceiling(max(minmax(rast(fileName_in_max))))
#         heatDamageLimits <- c(0, 10, 30, unsuitable_summerHotDays)
#         bins <- heatDamageLimits
#         caption <- paste0("Note: NA is oceans and land areas that are not suitable because of high heat \n(more than ", unsuitable_summerHotDays, 
#                           " days in the summer with temperatures above ", tmaxDamageVal, "°C) or extreme cold (less than ", tminExtremeVal, "°C.)")
#         
#       }
#       r <- project(r, crsRob)
#       r_df <- as.data.frame(r, xy = TRUE)
#       names(r_df) <-   c("lon", "lat", "value")
#       
#       #for testing
#       baseCt_df <- project(baseCt, crsRob)
#       baseCt_df <- as.data.frame(baseCt_df, xy = TRUE)
#       names(baseCt_df) <-   c("lon", "lat", "value")
#       
#       fileName_out <- paste0("graphics/cmip6/perennials/", fileNameStart, m, "_", k, "_", yearSpan, ".png")
#       custom_bins <- bins
#       r_df$custom_bins <- cut(r_df$value, breaks = custom_bins) # convert value to factors
#       
#       g <- ggplot(data = coastline_cropped) +
#         labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) +
#         geom_tile(data = r_df , aes(x = lon, y = lat, fill = custom_bins))  +
#         scale_fill_manual(values = RColorBrewer::brewer.pal(3, colorCombo)) +
#         geom_sf(fill = NA, color = "gray", lwd = 0.1) +
#         theme_bw() +
#         theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
#         theme(plot.title = element_text(size = 12, hjust = 0.5), plot.caption = element_text(hjust = 0, size = 8)) +
#         theme(legend.text.align = 1)
#       g <- g + geom_tile(data = baseCt_df,   aes(x = lon, y = lat, fill = value)) +
#         scale_fill_gradient("Distance",
#                             low = 'yellow', high = 'blue',
#                             na.value = NA)
#       
#       #        theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
#       print(g)
#       ggsave(filename = fileName_out, plot = g, width = 8, height = 4, units = "in", dpi = 300)
#       knitr::plot_crop(fileName_out) # gets rid of margins around the plot
#       print(paste0("out file name: ", fileName_out))
#       g <- NULL
#     }
#   }
# }
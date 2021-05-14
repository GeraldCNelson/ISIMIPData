# code to do various perennial crop calculations
{
  #source("R/globallyUsed.R")
  require(terra)
  library(ggplot2)
  source("R/ISIMIPconstants.R")
  source("R/ISIMIPspatialConstants.R")
  source("R/perennialsPrep.R") # creates the data tables majorCropValues_main, majorCropValues_lo and majorCropValues_hi
  options(warn = 1)
  # file locations -----
  areaYieldtifFileLoc <- "data-raw/crops/HarvestedAreaYield175Crops_Geotiff/GeoTiff/"
  # constants, general
  
  # choose whether to do the base cps, or the lo or hi cp requirements varieties -----
  varChoices <- c("varieties_lo", "varieties_main", "varieties_hi")
  suitabilityLevels <- c("good", "acceptable", "bad")
  yearRangeSH <- 18 # one less year because of 6 month offset
  speciesList <- c("almond", "apple", "cherry", "winegrape", "olive") #, "blueberries") 
  
  # constants, perennials -----
  minimumGrwSeasonLength = 100
  # All day numbers are Julian calendar days
  # The flowering window is NH = 60:120 (1Mar:30Apr); 227:288 (SH = 15Aug:15Oct)
  floweringLength <- 60
  heatDamageLength <- 60
  chillPortionWindow <- 214
  floweringStart_NH <- 60 # March 1 in 2019
  floweringStart_SH <- 227 # Aug 15 in 2019
  heatDamageStart_NH <- 182 # July 1
  heatDamageStart_SH <- 1 # Jan 1
  chillPortionStart_NH <- 272
  chillPortionStart_SH <- 92
  extremeColdCutoff <- -30
  # suitability day counts - first two numbers are good window, second two numbers are acceptable window; third two numbers are bad window; fourth 2 numbers are unsuitable range
  frostRiskDays <- c(0, 5, 6, 20, 21, 45, 46, floweringLength) 
  heatRiskDays <- c(0, 9, 10, 30, 31, 45, 46, heatDamageLength) 
  
  #test values, perennials -----
  varChoice <- "varieties_main"
  var_suffix <- gsub("varieties", "", varChoice)
  cropVals <- get(paste0("majorCropValues", var_suffix))
  speciesChoices <- unique(cropVals$cropName)
  speciesChoice <- "olive_main"
  suitabilityLevel <- "good"
  climVarChoice <- "tasmin"
  
  # functions -----
  f_range <- function(x, rangeVals) {
    # set locations with values outside the range to 999 as an indicator where land is for later adjustment
    # x[x < rangeVals[1]] <- NA
    # x[x > rangeVals[2]] <- NA
    x[x < rangeVals[1]] <- 999
    x[x > rangeVals[length(rangeVals)]] <- 999 # length needed because rangeVals can sometimes have more than 2 entries.
    return(x)
  }
  
  f_convertToSHdays <- function(yearNum, calIn) {
    if (lubridate::leap_year(yearNum)) {dayCt <- 366}else{dayCt <- 365}
    NHYear <- rep(1:dayCt, 1)
    SHyrStart <- rep(182:dayCt,1) # july 1
    SHyrEnd <- rep(1:181,1)
    SHYear <- c(SHyrStart, SHyrEnd)
    daysLookup <- data.frame(cbind(NHYear, SHYear))
    uniqueVals <- unique(calIn)
    for (val in 1:length(uniqueVals)) {
      calIn[calIn == uniqueVals[val]] <- daysLookup[uniqueVals[val], "SHYear"]
    }
    return(calIn)
  }
  
  f_readRast_count <- function(modelChoice, climVarChoice, layersToKeep, probVal, f_ct, k, l) {
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_in_hem <- paste0("data/bigFiles/", modelChoice, "_", k, "_", climVarChoice, "_", hem, "_global_daily_", yearSpan, ".tif") 
    print(paste0("climate fileName_in_hem in: ", fileName_in_hem))
    r <- rast(fileName_in_hem)
    #    print(r)
    # convert day numbers to calendar days to subset
    datesToKeep <- c()
    indices <- c()
    for (yearNum in l:(l + yearRange)) {
      temp <- as.Date(layersToKeep, origin = paste0(yearNum, "-01-01"))
      indices_yearNum <- rep(yearNum - l + 1, length(layersToKeep))
      indices <-c(indices, indices_yearNum)
      temp <- paste0("X", temp)
      datesToKeep <- c(datesToKeep, temp)
    }
    #    print(paste0("indices_yearNum: ", indices))
    r <- subset(r, datesToKeep)
    # indices <- format(as.Date(names(r), format = "X%Y-%m-%d"), format = "%y") # %y is year 2 digit number
    # indices <- as.numeric(indices)
    # indices <- indices - l + 2000 + 1
    # print(paste0("indices_v2: ", indices))
    # 
    print(paste0("length r: ", length(names(r)), ", length indices: ", length(indices)))
    print(system.time(tempCt <- tapp(r, indices, f_ct))) #indices values are from 1 to the number of years in the subsetted file, usually 20. layersToKeep are the layer numbers to keep in each year. The f_ct is the sum function created in the calling script
    print(tempCt)
    return(tempCt)
    #    return(r)
  }
  
  f_readRast_extreme <- function(modelChoice, varName, funDir) {
    fileName_in_hem <- paste0("data/bigFiles/", modelChoice, "_", k, "_", varName, "_", hem, "_global_daily_", yearSpan, ".tif") 
    print(paste0("climate fileName_in_hem in: ", fileName_in_hem))
    r <- rast(fileName_in_hem)
    print(r)
    indices <- format(as.Date(names(r), format = "X%Y-%m-%d"), format = "%y") # %y is year 2 digit number
    indices <- as.numeric(indices)
    indices <- indices - l + 2000 + 1
    print(system.time(extremeTemp <- tapp(r, indices, funDir))) #indices are from 1 to the number of years in the input file. 365 1s, then 365 2s, etc. The funDir is the minimum or maximum value of the temp var. extremeTemp is the highest or lowest temp value in each year in each cell.
    print(extremeTemp)
    return(extremeTemp)
  }
  
  f_extremeCold <- function(k, l, speciesChoice, hem) {
    yearSpan <- paste0(l, "_", l + yearRange)
    tempVar <- "tasmin"
    funDir <- "min"
    probVal <- 0.90
    system.time(x <- lapply(modelChoices_lower, f_readRast_extreme, tempVar, funDir)) # read in tasmin for the relevant period and all ESMs
    r <- rast(x)
    r
    extremeColdCutoff <- majorCropValues_main[cropName == speciesChoice, low_temp_threshold] # the extreme cold cutoff is common to a species so majorCropValues_main works regardless of what the cp requirements are
    # now do ensemble mean and cutoff
    print(paste0("Working on extreme cold for speciesChoice: ", speciesChoice, ", working on ssp: ", k, ", start year ", l, ", hemisphere ", hem))
    fileName_out <- paste0(locOfDataFiles_perennials, "extremeCold_cutoff_", speciesChoice, "_", k, "_", hem, "_", yearSpan, ".tif")
    print(system.time(extremeCold_quant <- quantile(r, probs = probVal, na.rm = TRUE)))
    # extremeCold_quant[extremeCold_quant < extremeColdCutoff] <- 0 # not suitable
    # extremeCold_quant[extremeCold_quant >= extremeColdCutoff] <- 1 # suitable
    rangeVals <- c(extremeColdCutoff, 50)
    extremeCold_quant <- f_range(extremeCold_quant, rangeVals) 
    extremeCold_quant[extremeCold_quant >= extremeColdCutoff & extremeCold_quant < 999] <- 1 # use of 999 here and in f_range is a kludge to set unsuitable land areas to zero.
    extremeCold_quant[extremeCold_quant == 999] <- 0
    
    print(system.time(writeRaster(extremeCold_quant, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
    print(paste0("fileName_out extreme cold: ", fileName_out))
    plot(extremeCold_quant, main = speciesChoice)
    return(extremeCold_quant)
  }
  
  # chillPortions calcs done in chillPortions.R -----
  
  # flowering and heat damage -----
  f_frostHeatDamage <- function(k, l, speciesChoice, hem, varChoice, suitabilityLevel) {
    yearSpan <- paste0(l, "_", l + yearRange)
    # frost and heat damage day windows -----
    spLyrStart <- get(paste0("floweringStart_", hem))
    spLyrEnd <- spLyrStart + floweringLength
    spLyrs <- spLyrStart:spLyrEnd
    
    hdLyrStart <- get(paste0("floweringStart_", hem))
    hdLyrEnd <- hdLyrStart + floweringLength
    hdLyrs <- hdLyrStart:hdLyrEnd
    
    for (season in c("flowering", "summer")) {
      if (season == "flowering") {
        frDays <- switch(suitabilityLevel,
                         "good" = frostRiskDays[1:2],
                         "acceptable" = frostRiskDays[3:4],
                         "bad" = frostRiskDays[5:6],
                         "unsuitable" = frostRiskDays[7:8]
        )
        climVarChoice <- "tasmin"
        flowerDamage_threshold <- cropVals[cropName == speciesChoice, chill_threshold]
        layersToKeep <- spLyrs
        probVal <- 0.90
        f_ct <- function(x) (sum(x < flowerDamage_threshold)) # created here; used in f_readRast_count
        system.time(x <- lapply(modelChoices_lower, f_readRast_count, climVarChoice, layersToKeep, probVal, f_ct, k, l))
        r <- rast(x)
        r
        fr <- f_range(r, frDays) #f_range sets area outside the suitable range to NA
        system.time(fr <- quantile(fr, probs = probVal, na.rm = TRUE)) # note: if all layers have the same value quantile returns that value
        #       system.time(fr <- round(fr, 0)) # make sure it is an integer
        
        fr[fr >= 0 & fr < 999] <- 1 # use of 999 here and in f_range is a kludge to set unsuitable land areas to zero.
        fr[fr == 999] <- 0
        titleText_fr <- paste0("1 indicates locs where flowering suit. is ", suitabilityLevel, " for ", speciesChoice, " during scenario ", k, ", ", yearSpan, ". \nUnsuitable frost risk is more than ", frDays[2], " frost days during flowering window.")
        plot(fr, main = titleText_fr)
        fileName_fr_out <- paste0(locOfDataFiles_perennials, "floweringDamage_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
        writeRaster(fr, fileName_fr_out, overwrite = TRUE, wopt = woptList)
        print(paste0(" flower damage fileName out: ", fileName_fr_out))
      }
      if (season == "summer") {
        hdDays <- switch(suitabilityLevel,
                         "good" = heatRiskDays[1:2],
                         "acceptable" = heatRiskDays[3:4],
                         "bad" = heatRiskDays[5:6],
                         "unsuitable" = heatRiskDays[7:8]
        )
        climVarChoice <- "tasmax"
        summer_heat_threshold <- cropVals[cropName == speciesChoice, summer_heat_threshold]
        layersToKeep <- hdLyrs
        probVal <- 0.90
        f_ct <- function(x) (sum(x > summer_heat_threshold))
        system.time(x <- lapply(modelChoices_lower, f_readRast_count, climVarChoice, layersToKeep, probVal, f_ct, k, l))
        r <- rast(x)
        r
        hd <- f_range(r, hdDays)
        system.time(hd <- quantile(hd, probs = probVal, na.rm = TRUE)) # note: if all layers have the same value quantile returns that value
        hd[hd >= 0 & hd < 999] <- 1
        hd[hd == 999] <- 0
        titleText_hd <- paste0("1 indicates locs where summer heat suit. is ",suitabilityLevel, " for ", speciesChoice, " during scenario ", k, ", ", yearSpan, ". \nUnsuitable heat risk is more than ", hdDays[2], " days of temperature above ", summer_heat_threshold, "C during summer window.")
        
        plot(hd, main = titleText_hd)
        fileName_hd_out <- paste0(locOfDataFiles_perennials, "heatDamage_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
        writeRaster(hd, fileName_hd_out, overwrite = TRUE, wopt = woptList)
        print(paste0(" heat damage fileName out: ", fileName_hd_out))
      }
      print("--------------------------------------------")
    }
  }
  
  f_combinedDamage <- function(k, l, speciesChoice, suitabilityLevel) {
    # combine suitability metrics from chill portions, extreme cold, flowering frost, and summer heat; locations with value 1 is suitable
    yearSpan <- paste0(l, "_", l + yearRange)
    for (hem in hemispheres) {
      # read in all the rasters needed
      fileName_extremeColdCt_in <- paste0(locOfDataFiles_perennials, "extremeCold_cutoff_", speciesChoice, "_", k, "_", hem, "_", yearSpan, ".tif")
      extremeColdCt <- rast(fileName_extremeColdCt_in) # 1 is good; 0 is bad
      fileName_fr_in <- paste0(locOfDataFiles_perennials, "floweringDamage_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif") 
      frostCt <- rast(fileName_fr_in) # number of frost days below the damage threshold of zero; locations outside the range are NA; outside land is NaN
      fileName_hd_in <- paste0(locOfDataFiles_perennials, "heatDamage_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif") 
      heatCt <- rast(fileName_hd_in) # number of high heat days below the damage threshold; locations outside the range are NA; outside land is NaN
      fileName_cp_in <- paste0("data/cmip6/chillPortions/chill_portions/", "ensemble_chill_cutoff_", speciesChoice, "_", k, "_", hem, "_", yearSpan, ".tif")
      chillPortionsCutoff <- rast(fileName_cp_in) # 1 is good, 0 is bad
      
      # convert day counts to 1 (suitable, 0 - not suitable)
      heatCt[heatCt >= 0] <- 1 # convert the number of days that meet the suitability criterion to 1
      frostCt[frostCt >= 0] <- 1 # convert the number of days that meet the suitability criterion to 1
      
      print(paste0("working on combined damage ", speciesChoice, " in hemisphere ", hem, ", year ", l, ", scenario ", k))
      
      r_combined <- c(extremeColdCt, frostCt, heatCt, chillPortionsCutoff)
      names(r_combined) <- c("extremeColdSuit", "floweringSuit", "heatSuit", "chillPortionsSuit")
      r_suitable <- app(r_combined, prod)
      names(r_suitable) <- "combinedSuit"
      r_all <- c(r_suitable, r_combined)
      r_suit_hem <- paste0("r_suitable_", hem)
      r_suit_all_hem <- paste0("r_suitable_all_", hem)
      assign(r_suit_hem, r_suitable)
      assign(r_suit_all_hem, r_all)
    }
    r_suitable_globe <- merge(r_suitable_NH, r_suitable_SH)
    r_suitable_all_globe <- merge(r_suitable_all_NH, r_suitable_all_SH)
    r_suitable_all_globe_df <- as.data.frame(r_suitable_all_globe, xy = TRUE, na.rm = FALSE)
    fileName_suitable_all_out <- paste0(locOfDataFiles_perennials, "suitable_all ", speciesChoice, "_",  k, "_", suitabilityLevel, "_", yearSpan, ".tif")
    print(system.time(writeRaster(r_suitable_all_globe, filename = fileName_suitable_all_out,  overwrite = TRUE,  wopt= woptList))); flush.console()
    fileName_suitable_all_df_out <- paste0(locOfDataFiles_perennials, "suitable_all_", speciesChoice, "_",  k, "_", suitabilityLevel, "_", yearSpan, ".csv")
    write.csv(r_suitable_all_globe_df, fileName_suitable_all_df_out)
    titleText <- paste0("Locations where suitability is ", suitabilityLevel, " for ", speciesChoice, ", scenario: ", k, ", period: ", yearSpan)
    pal <- colorRampPalette(c("white", "green"))
    
    plot(r_suitable_globe, main = titleText, legend = FALSE, xlab = FALSE, axes=FALSE, col = pal(2))
    plot(coastline_cropped, add = TRUE)
  }
  
  
  f_suitableLocsPpt <- function(k, l, yearSpan, suitabilityLevel) {
    #
    fileName_in <- paste0(lofOfGraphicsFiles, "perennials/", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
    extImg <- external_img(src = fileName_in, width = defaultWidth, height = defaultHeight)
    my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
    my_pres <- ph_with(x = my_pres, value = extImg, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5))
    return(my_pres)
  }
  
  f_suitableLocsGraphics <- function(k, yearSpan, speciesChoice, suitabilityLevel) {
    legendTitle <- suitabilityLevel
    speciesName <- gsub(var_suffix, "", speciesChoice) # needed for the harvested area data
    # the harvested area data is just for grapes so need to get rid of wine in the names
    if (speciesChoice == paste0("winegrape", var_suffix)) speciesName <- "grape"
    
    #   for (hem in hemispheres) {
    # crop out areas where summer heat or spring frost are greater than the unsuitable values, either unsuitable_springFreezeDays or unsuitable_summerHotDays
    CPfruit <- majorCropValues_main[cropName == speciesChoice, chill_hours]
    for (suitabilityLevel in c("good", "acceptable", "bad")) {
      titleText <- paste0("Growing conditions for ", speciesChoice," are suitable;  quality:  ", i, "\n" , "scenario: ", k, ", period: ", gsub("_", "-", yearSpan))
      if (i == "good") {
        caption <- paste0("Note: Good growing conditions for ", speciesChoice, " include chill portions of at least ", CPfruit, ", less than ", frostRiskDays[3], 
                          " days of spring frost risk and \nless than ", heatRiskDays[3], " days of summer excessive heat. Gray hatching indicates early 21st century area.")
        col = "green"}
      if (i == "acceptable") {
        caption <- paste0("Note: acceptable growing conditions for ", speciesChoice, " include chill portions of at least ", CPfruit, ", ", frostRiskDays[3], "-",  frostRiskDays[4], 
                          " days of spring frost risk and \n",   heatRiskDays[3], "-",  heatRiskDays[4], " days of summer excessive heat. Gray hatching indicates early 21st century area.")
        col = "yellow"}
      if (i == "bad") {
        caption <- paste0("Note: Bad growing conditions for ",speciesChoice, " include chill portions of at least ", CPfruit, ", ", frostRiskDays[5], "-",  frostRiskDays[6], 
                          " days of spring frost risk and \n",  heatRiskDays[5], "-",  heatRiskDays[6], " days of summer excessive heat. Gray hatching indicates early 21st century area.")
        col = "red"}
      
      fileName_in_NH <- paste0("data/cmip6/annuals/suitable_", speciesChoice, "_", "NH", "_", k, "_", i, "_", yearSpan, ".tif")
      fileName_in_SH <- paste0("data/cmip6/annuals/suitable_", speciesChoice, "_", "SH", "_", k, "_", i, "_", yearSpan, ".tif")
      
      r_nh <- rast(fileName_in_NH)
      r_sh <- rast(fileName_in_SH)
      
      # now get harvested area map
      rInArea <- rast(paste0(tifFileLoc, fruit_in,"/",  fruit_in, "_HarvestedAreaHectares.tif"))
      harvestArea <- aggregate(rInArea, fact = 6, fun = "sum") # convert 5 arc minutes to 1/2 degrees
      #     harvestArea[harvestArea == 0] <- NA
      harvestArea[harvestArea > 0] <- 1
      
      r <- merge(r_nh, r_sh)
      
      r <- project(r, crsRob)
      harvestArea <- project(harvestArea, crsRob)
      r_df <- as.data.frame(r, xy = TRUE)
      names(r_df) <-   c("x", "y", "value")
      harvestArea_df <- as.data.frame(harvestArea, xy = TRUE)
      names(harvestArea_df) <-   c("x", "y", "value")
      fileName_out <- paste0(lofOfGraphicsFiles, "annuals/", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
      
      # do without legend
      g <- ggplot() +
        geom_tile(data = r_df, aes(x, y), fill = col) +
        labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + 
        theme_bw()  +
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
        geom_tile(data = harvestArea_df, aes(x, y, fill = value), alpha = .2, show.legend = FALSE) 
      
      print(g)
      ggsave(filename = fileName_out, plot = g, width = 8, height = 4, units = "in", dpi = 300)
      knitr::plot_crop(fileName_out) # gets rid of margins around the plot
      print(paste0("file name out: ", fileName_out))
      g <- NULL
    }
  }
  
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
}

# extreme cold calcs, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (hem in hemispheres) {
      for (speciesChoice in speciesChoices) {
        f_extremeCold(k, l, speciesChoice, hem)
      }
    }
  }
}

# extreme cold calcs, historical -----
varChoice <- "varieties_main"
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (hem in hemispheres) {
  for (speciesChoice in speciesChoices){
    f_extremeCold(k, l, speciesChoice, hem)
  }
}

# flowering and heat damage, scenarios -----
varChoice <- "varieties_main"
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (hem in hemispheres) {
      for (speciesChoice in speciesChoices) {
        f_frostHeatDamage(k, l, speciesChoice, hem, varChoice, suitabilityLevel)
      }
    }
  }
}

# flowering and heat damage, historical -----
varChoice <- "varieties_main"
k <- "historical"
l <- 1991
for (hem in hemispheres) {
  for (speciesChoice in speciesChoices) {
    f_frostHeatDamage(k, l, speciesChoice, hem, varChoice, suitabilityLevel)
  }
}

# combined damage, scenarios -----
# code to read in chill portions, cold, freeze and heat stress 1/0 files and produce 1/0 tifs where the crop is potentially growable. The chill portions files are created in the chillPortions.R script

for (k in sspChoices) {
  for (l in startYearChoices) {
    for (speciesChoice in speciesChoices) {
      print(paste0("speciesChoice: ", speciesChoice, ", ssp choice: ", k, ", start year: ", l))
      f_combinedDamage(k, l, speciesChoice, suitabilityLevel) 
    }
  }
}

# combined damage, historical -----
k <- "historical"
l <- 1991
for (speciesChoice in speciesChoices) {
  print(paste0("speciesChoice: ", speciesChoice, ", ssp choice: ", k, ", start year: ", l))
  f_combinedDamage(k, l, speciesChoice, suitabilityLevel) 
} 

# area calculations -----
{
  dt_area <- data.table(species = character(), hemisphere = character(), ssp = character(), yearSpan = character(), quality = character(), area_suitable = numeric(), rasterName = character())
  suitabilityLevels <- "good"
  for (k in sspChoices) {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      for (speciesChoice in speciesChoices) {
        for (hem in hemispheres) {
          for (suitabilityLevel in suitabilityLevels) {
            fileName_in <- paste0(locOfDataFiles_perennials, "suitable_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
            rastName <- paste0("suitable_", speciesChoice, "_", hem, "_", k, "_", suitabilityLevel, "_", yearSpan)
            r <- rast(fileName_in)
            r[r == 0] <- NA # only want to get area of cells that have a value of 1; ie, suitable
            r_area <- expanse(r, unit = "km")
            dt_area <- rbind(dt_area, list(speciesChoice, hem, k, yearSpan, suitabilityLevel, r_area, rastName))
          }
        }
      }
    }
  }
  
  k <- "historical"
  l <- 1991
  yearSpan <- paste0(l, "_", l + yearRange)
  for (speciesChoice in speciesChoices) {
    for (hem in hemispheres) {
      for (suitabilityLevel in suitabilityLevels) {
        fileName_in <- paste0(locOfDataFiles_perennials, "suitable_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
        rastName <- paste0("suitable_", speciesChoice, "_", hem, "_", k, "_", suitabilityLevel, "_", yearSpan)
        r <- rast(fileName_in)
        r[r == 0] <- NA # only want to get area of cells that have a value of 1; ie, suitable
        r_area <- expanse(r, unit = "km")
        dt_area <- rbind(dt_area, list(speciesChoice, hem, k, yearSpan, suitabilityLevel, r_area, rastName))
      }
    }
  }
  fileName_out <- paste0(locOfDataFiles_perennials, "areaCalcs", var_suffix, ".csv")
  write.csv(dt_area, file = fileName_out, row.names = FALSE)
  print(paste0("fileName out: ", fileName_out))
}

# area deltas, need to run code above first -----
dt_area_delta <- data.table(species = character(), hemisphere = character(), ssp_base = character(), ssp = character(), yearSpan = character(), quality = character(), area_base = numeric(), area_delta = numeric(), delta_share = numeric())
for (speciesChoice in speciesChoices) {
  for (hem in hemispheres) {
    for (suitabilityLevel in suitabilityLevels) {
      r_historical <- rast(paste0(locOfDataFiles_perennials, "suitable_", speciesChoice, "_", "historical", "_", suitabilityLevel, "_", hem, "_", "1991_2010.tif"))
      area_base <- expanse(r_historical, unit = "km")
      for (l in startYearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        for (k in sspChoices) {
          rastName <- paste0(locOfDataFiles_perennials, "suitable_", speciesChoice, "_",  k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
          r_delta <- rast(rastName) - r_historical
          r_delta_area <- expanse(r_delta, unit = "km")
          delta_ratio <- r_delta_area/area_base
          dt_area_delta <- rbind(dt_area_delta, list(speciesChoice, hem, "historical", k, yearSpan, suitabilityLevel, area_base, r_delta_area, delta_ratio))
        }
      }
    }
  }
}

fileName_out <- paste0(locOfDataFiles_perennials, "areaCalcs_delta", var_suffix, ".csv")
write.csv(dt_area_delta, file = fileName_out, row.names = FALSE)
print(paste0("fileName out: ", fileName_out))

# harvest and suitability area calcs ------
# area common to early and end century -----
dt_area_common <- data.table(species = character(), hemisphere = character(), ssp = character(), area_common = numeric())
for (speciesChoice in speciesChoices) {
  speciesName <- gsub(var_suffix, "", speciesChoice) # needed for the harvested area data
  # the harvested area data is just for grapes so need to get rid of wine in the names
  if (speciesChoice == paste0("winegrape", var_suffix)) speciesName <- "grape"
  fileName_in <- paste0(areaYieldtifFileLoc, speciesName,"/",  speciesName, "_HarvestedAreaHectares.tif")
  rInArea <- rast(fileName_in)
  harvestArea_earlyCent <- aggregate(rInArea, fact = 6, fun = "sum") # convert 5 arc minutes to 1/2 degrees
  maskMin <- switch(
    speciesName,
    "cherry" = 50,
    "almond" = 50,
    "grape" = 50,
    "apple" = 50,
    "olive" = 50
  )
  #  harvestArea_earlyCent[harvestArea_earlyCent < maskMin] <- NA # set minimum area to be greater than 50 hectares per grid cell
  harvestArea_earlyCent[harvestArea_earlyCent < maskMin] <- 0 # set minimum area to be greater than 50 hectares per grid cell
  harvestArea_earlyCent[harvestArea_earlyCent > 0] <- 1
  harvestArea_earlyCent <- crop(harvestArea_earlyCent, extent_noAntarctica)
  harvestArea_earlyCent_NH <- crop(harvestArea_earlyCent, extent_NH)
  harvestArea_earlyCent_SH <- crop(harvestArea_earlyCent, extent_SH)
  # dt_area_common <- rbind(dt_area_common, list(speciesChoice, "NH", "historical", expanse(harvestArea_earlyCent_NH, unit = "km")))
  # dt_area_common <- rbind(dt_area_common, list(speciesChoice, "SH", "historical", expanse(harvestArea_earlyCent_SH, unit = "km")))
  
  for (k in sspChoices) {
    #suitable areas -----
    suitabilityLevel <- "good"
    # suitable area end century
    yearSpan <- "2081_2100"
    fileName_in_SH <- paste0(locOfDataFiles_perennials, "suitable_", speciesChoice, "_", k, "_", suitabilityLevel, "_", "SH", "_", yearSpan, ".tif")
    fileName_in_NH <- paste0(locOfDataFiles_perennials, "suitable_", speciesChoice, "_", k, "_", suitabilityLevel, "_", "NH", "_", yearSpan, ".tif")
    suitableArea_endCent_SH <- rast(fileName_in_SH)
    suitableArea_endCent_NH <- rast(fileName_in_NH)
    
    suitableArea_endCent <- merge(suitableArea_endCent_SH, suitableArea_endCent_NH)
    commonArea <- harvestArea_earlyCent + suitableArea_endCent # areas where both early and end century rasters = 1
    commonArea_NH <- suitableArea_endCent_NH + harvestArea_earlyCent_NH
    commonArea_SH <- suitableArea_endCent_SH + harvestArea_earlyCent_SH
    dt_area_common <- rbind(dt_area_common, list(speciesChoice, "NH", k, expanse(commonArea_NH, unit = "km")))
    dt_area_common <- rbind(dt_area_common, list(speciesChoice, "SH", k, expanse(commonArea_SH, unit = "km")))
  }# end of ssp loop
  
} 
# fileName_out <- paste0(locOfDataFiles_perennials, "areaCalcs_common", "_", k, var_suffix, ".csv")
# write.csv(dt_area_common, file = fileName_out, row.names = FALSE)
# print(paste0("fileName out: ", fileName_out))

# create table of area changes ------
dt_area <- as.data.table(read.csv(file = paste0(locOfDataFiles_perennials, "areaCalcs", var_suffix, ".csv")))
# fileName_in <- paste0(locOfDataFiles_perennials, "areaCalcs_common", "_", k, var_suffix, ".csv")
# dt_area_common <- as.data.table(read.csv(fileName_in))

# delete rows that are for suitability bad or acceptable and mid century - keeping just early and end century
dt_area <- dt_area[!quality %in% c("bad", "acceptable") & !yearSpan %in% "2041_2060",]
dt_area[, c("quality", "rasterName", "yearSpan") := NULL]
dt_area_wide <- dcast(dt_area, species + hemisphere ~ ssp, value.var = "area_suitable")
dt_area_common_wide <- dcast(dt_area_common, species + hemisphere ~ ssp, value.var = "area_common")
setnames(dt_area_common_wide, old = c("ssp126", "ssp585"), c("ssp126_common", "ssp585_common"))
# dt_area_common[, ssp := NULL]
combined <- merge(dt_area_wide, dt_area_common_wide)
# convert units
colsToConvert <- c("historical", "ssp126", "ssp585", "ssp126_common", "ssp585_common")
combined[, (colsToConvert) := lapply(.SD, '/', 1000), .SDcols = (colsToConvert)]
combined[, ratioEnd2Early_585 := -1 + ssp585/historical]
combined[, ratioEnd2Early_126 := -1 + ssp126/historical]
combined[, ratioCommon2Early_585 := ssp585_common/historical]
combined[, ratioCommon2Early_126 := ssp126_common/historical]
combined[, RatioLossOfEarly_585 := (historical - ssp585_common)/historical]
combined[, RatioLossOfEarly_126 := (historical - ssp126_common)/historical]
write.csv(combined_wide, file = paste0(locOfDataFiles_perennials, "sumTable", var_suffix, ".csv", row.names = FALSE))

#library(gtsummary)
k = "ssp126"
l = 2041
i = "good"
speciesChoice = "cherry"
m = "NH"

#suitable locations graphics, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (speciesChoice in speciesChoices) {
      print(paste0(" favorable locs, speciesChoice: ", speciesChoice, ", ssp choice: ", k, ", start year: ", l))
      f_suitableLocsGraphics(k, yearSpan, speciesChoice)
    }
  }
}

#suitable locations graphics, historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (speciesChoice in speciesChoices) {
  print(paste0(" favorable locs, speciesChoice: ", speciesChoice, ", ssp choice: ", k, ", start year: ", l))
  f_suitableLocsGraphics(k, yearSpan, speciesChoice)
}

# demo slides ------
# stuff below here needs work XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
k = "ssp126"
l = 2041
yearSpan <- paste0(l, "_", l + yearRange)
speciesChoice <- "cherry_main"
suitabilityLevel = "good"
legendTitle <- "Suitable"
defaultColor <- c("green", "red")
CPfruit <- cropVals[cropName == speciesChoice, chill_hours]
if (i == "good") {
  caption <- paste0("Note: ", suitabilityLevel, " growing conditions for ", speciesChoice, " include chill portions of at least ", CPfruit, ", ", frostRiskDays[3], "-",  frostRiskDays[4], 
                    " days of flowering frost risk and \n",   heatRiskDays[3], "-",  heatRiskDays[4], " days of summer excessive heat.")
}

fileName_in_NH <- paste0(locOfDataFiles_perennials, "combinedDamages_", speciesChoice, "_", "NH", "_", k, "_", suitabilityLevel, "_", yearSpan, ".nc")
fileName_in_SH <- paste0(locOfDataFiles_perennials, "combinedDamages_", speciesChoice, "_", "SH", "_", k, "_", suitabilityLevel, "_", yearSpan, ".nc")
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
#extremeColdCt[extremeColdCt == 1] <- NA
extremeColdCt[extremeColdCt == 1] <- 0 # hmmmm
unsuitableFrost <- merge(unsuitableFrost_NH, unsuitableFrost_SH)
frostCt <- merge(frostCt_NH, frostCt_SH)
frostCt[frostCt > 1] <- 1
unsuitableHeat <- merge(unsuitableHeat_NH, unsuitableHeat_SH)
heatCt <- merge(heatCt_NH, heatCt_SH)
heatCt[heatCt > 1] <- 1
chillPortionsCutoff <- merge(chillPortionsCutoff_NH, chillPortionsCutoff_SH)

r <- extremeColdCt
col <- defaultColor
tminExtremeVal <- cropVals[cropName == speciesChoice, low_temp_threshold]
titleText <- paste0("Extreme Cold Mask (minimum temperature is less than ", tminExtremeVal, " for at least one day). \nGreen areas don't have this cold.")
fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_extremeColdMask", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
f_graphics_demoSlides(r, titleText, caption, fileName_out, col)

r <- chillPortionsCutoff # 1 is where chill portions is adequate 
col <- rev(defaultColor)
titleText <- paste0("Locations where chill portions are adequate for ", speciesChoice, ", scenario: ", k, ", year span: ", yearSpan)
fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_chillPortionsCutoff", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
f_graphics_demoSlides(r, titleText, caption, fileName_out, col)

{# these next lines of code must stay in this order.
  r <- mask(chillPortionsCutoff, extremeColdCt, maskvalue = 1) # 0 is no extreme cold; 1 is extreme cold, this sets masked areas to NA
  col <- rev(defaultColor)
  titleText <- "Locations where the chill portions are not affected by extreme cold"
  fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_chillPortionsGood", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
  f_graphics_demoSlides(r, titleText, caption, fileName_out, col)
  
  r <- mask(r, frostCt, maskvalue = 1) # 1 is where frost Ct is below the unsuitable level
  col <- rev(defaultColor)
  titleText = "Locations where chill portions are adequate and \nthe number of flowering frost days is in the suitable range"
  fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_floweringFrostGood", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
  f_graphics_demoSlides(r, titleText, caption, fileName_out, col)
  
  r <- mask(r, heatCt, maskvalue = 1) # 1 is where heat Ct is above the unsuitable level
  r[r == 0] <- NA
  col <- defaultColor
  titleText = "Locations were chill portions are adequate, the numbers of flowering frost \nand summer hot days are in suitable ranges."
  fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_summerHeatBad", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
  f_graphics_demoSlides(r, titleText, caption, fileName_out, col)}

r <- heatCt
col <- defaultColor
titleText = "Locations in red are where the number of summer hot days are not suitable."
fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_summerHeatGood", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
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
  #   if (season == "flowering") fileNameStart <- paste0("floweringFrost_hem_")
  #   if (season == "summer") fileNameStart <- paste("summerHeat_hem_")
  #   fileName_NH <- paste0(lofOfGraphicsFiles, "perennials/", fileNameStart, "NH", "_", k, "_", yearSpan, ".png")
  #   fileName_SH <- paste0(lofOfGraphicsFiles, "perennials/", fileNameStart, "SH", "_", k, "_", yearSpan, ".png")
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
  dataText4 <- "The data from a 20 year period for the individual models are averaged. "
  #dataText5 <- "Crop area is based on the SAGE cropping calendar data set, based in the early 2000s. Areas that are not cropped have an NA value and are displayed in white. Areas in gray have chilling less than the minimum requirements. Areas in yellow have chilling hours between the lower and upper range of the requirements."
  dataText <- c(dataText1, dataText2, dataText3, dataText4) #, dataText5)
  
  introText1 <- "Temperate perennial fruits face four weather-based challenges - temperature extremes of cold and heat can kill the plant, a period of time during the winter season where temperatures are below freezing (a chilling period) is required, and periods during the growing season where high temperatures can cause damage in critical growth periods. These challenges, and specific values, are described in more detail in the table below."
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

fileName_RTable <- paste0(lofOfGraphicsFiles, "perennials/", "RebeccaTable.png")
extImg_Rtable <- external_img(src = fileName_RTable, width = defaultWidth, height = defaultHeight)
my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = extImg_Rtable, location = ph_location(left = defaultLeft, top = defaultHeight/2, width = 8, height = 3) )
my_pres <- ph_with(x = my_pres, value = blIntro, location = ph_location_type(type = "body") )

# add graphics for the demo slides
extremeColdMask <- external_img(src =  lofOfGraphicsFiles, "perennials/demoSlide_extremeColdMaskcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
chillPortionsCutoff <- external_img(src =  lofOfGraphicsFiles, "perennials/demoSlide_chillPortionsCutoffcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
chillPortions <- external_img(src =  lofOfGraphicsFiles, "perennials/demoSlide_chillPortionsGoodcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
floweringFrost <- external_img(src =  lofOfGraphicsFiles, "perennials/demoSlide_floweringFrostGoodcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
summerHeatBad <- external_img(src =  lofOfGraphicsFiles, "perennials/demoSlide_summerHeatBadcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
summerHeatGood <- external_img(src =  lofOfGraphicsFiles, "perennials/demoSlide_summerHeatGoodcherry_good_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)

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
my_pres <- ph_with(x = my_pres, value = floweringFrost, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

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
for (suitabilityLevel in suitabilityLevels) {
  
  for (speciesChoice in speciesChoices) {
    ensembleTitle <- paste("Suitable locations for ", speciesChoice, ", Locations quality: ", suitabilityLevel)
    my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
    my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
    
    k <- "historical"
    l <- 1991
    yearSpan <- paste0(l, "_", l + yearRange)
    my_pres <- f_suitableLocsPpt(k, l, yearSpan, suitabilityLevel)
    
    for (k in sspChoices) {
      for (l in startYearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        my_pres <- f_suitableLocsPpt(k, l, yearSpan, suitabilityLevel)
      }
    }
  }
}

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Data Sources", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = blData, location = ph_location_type(type = "body") )

print(my_pres, target = "presentations/cmip6/perennials/summerHeatandfloweringFrost.pptx") %>% browseURL()


# # powerpoint, favorable locations -----
# library(officer)
# library(flextable)
# library(magrittr)
# 
# defaultWidth <- 10
# defaultHeight <- 5
# defaultLeft <- 0
# defaultTop <- 1
# # defaultTopSH <- 4
# 
# f_favorableLocsPpt <- function(fruitSpecies) {
#   fileNameStart <- paste0(fruitSpecies, "_", suitabilityLevel, "_locations_", hem, "_")
#   fileName_in <- paste0(lofOfGraphicsFiles, "perennials/", fileNameStart, k, "_", yearSpan, ".png")
#   extImg_favLocs <- external_img(src = fileName_in, width = defaultWidth, height = defaultHeight)
#   my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
#   my_pres <- ph_with(x = my_pres, value = extImg_favLocs, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )
#   return(my_pres)
# }
# 
# # presentation intro -----
# titleString <- paste0("Adequate Chill Portions by Fruit, Time Period, and Scenario")
# contentString <- paste0("Powerpoint produced on ", Sys.Date())
# 
# dataText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
# dataText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/). "
# dataText3 <- "It includes data from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three scenarios (ssp126, ssp370 and ssp585). In this powerpoint, only results using ssp 126 and ssp585 are presented. " 
# dataText4 <- "The data from a 20 year period for the individual models are averaged for each month and a coefficient of variation across the 5 models is calculated. "
# introText1 <- "These slides display locations where the three conditions for perennial crop success - adequate chill portions, and heat and freeze threat conditions are 'good, 'ok', or 'bad'. "
# introText2 <- "The table below shows the crops we’re considering and the chill portion requirements used in the following graphs. "
# 
# #dataText5 <- "Crop area is based on the SAGE cropping calendar data set, based in the early 2000s. Areas that are not cropped have an NA value and are displayed in white. Areas in gray have chilling less than the minimum requirements. Areas in yellow have chilling hours between the lower and upper range of the requirements."
# dataText <- c(dataText1, dataText2, dataText3, dataText4) #, dataText5)
# 
# fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
# fp_2 <- fp_text(bold = FALSE, font.size = 12)
# fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)
# 
# blData <- block_list(
#   #  fpar(ftext("hello world", fp_1)),
#   fpar(
#     ftext(dataText1, fp_2),
#     ftext(dataText2, fp_2),
#     ftext(dataText3, fp_2),
#     ftext(dataText4, fp_2) #,
#     #    ftext(dataText5, fp_2)
#   ))
# 
# blIntro <- block_list(
#   fpar(
#     ftext(introText1, fp_2),
#     ftext(introText2, fp_2)
#   ))
# 
# my_pres <- read_pptx()
# my_pres <- add_slide(x = my_pres, layout = 'Title Slide', master = 'Office Theme')
# my_pres <- ph_with(x = my_pres, value = titleString, location = ph_location_type(type = "ctrTitle"))
# my_pres <- ph_with(x = my_pres, value = contentString, location = ph_location_type(type = "subTitle"))
# 
# my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
# my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
# my_pres <- ph_with(my_pres, head(CPs), location = ph_location(left = 2.5, top = 2.5, width = 4, height = 3) )
# 
# my_pres <- ph_with(x = my_pres, value = blIntro, location = ph_location_type(type = "body") )
# 
# for (speciesChoice in speciesChoices) {
#   ensembleTitle <- paste("Suitable locations for ", speciesChoice)
#   my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
#   my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
#   
#   for (i in c("good", "acceptable", "bad")) {
#     for (hem in hemispheres) {
#       
#       # do historical first, then ssps and future periods
#       k <- "historical"
#       l <- 1991
#       yearSpan <- paste0(l, "_", l + yearRange)
#       my_pres <- f_favorableLocsPpt(speciesChoice)
#       
#       for (k in sspChoices) {
#         for (l in startYearChoices) {
#           yearSpan <- paste0(l, "_", l + yearRange)
#           my_pres <- f_favorableLocsPpt(speciesChoice)
#         }
#       }
#     }
#   }
# }
# 
# my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
# my_pres <-  ph_with(x = my_pres, value = "Data Source", location = ph_location_type(type = "title"))
# my_pres <- ph_with(x = my_pres, value = blData, location = ph_location_type(type = "body") )
# 
# print(my_pres, target = "presentations/cmip6/perennials/adequateChillPortions.pptx") %>% browseURL()

# # runs testing ------
# f_runs <- function(x) {
#   runResult <- c(NA, NA) 
#   if (is.nan(x[1])) {
#     return(runResult)
#   }
#   seqLengthCode <- paste0("1{", test_length, ",}") #A regular expression  to get the first item of gregexpr. It says look for  test_length times See http://xenon.stanford.edu/~xusch/regexp/
#   g <- gregexpr(seqLengthCode, paste(+eval(parse(text = test_logic)), collapse = ""))[[1]] # The + converts TRUE and FALSE to 1 and 0
#   if (!(g[1] == -1)) { # no need to return if g returns -1, return 0,0 instead
#     startDays <- unlist(g)
#     runLengths <- sum(as.numeric(attributes(g)$match.length))
#     runResult <- c(length(startDays), runLengths)
#   } else {
#     runResult <- c(0, 0) 
#   }
#   return(runResult)
# }
# 
# test_logic <- "x > 35"
# test_length <- 10
# 
# for (k in sspChoices) {
#   for (l in startYearChoices) {
#     yearSpan <- paste0(l, "_", l + yearRange)
#     fileName_in <- paste0("data/bigFiles/ensemble_dyMean20yr_", k, "_tasmax_global_daily_", yearSpan, ".tif")
#     r <- rast(fileName_in)
#     print(system.time(r_runs <- app(r, f_runs)))
#     fileName_out <- paste0("data/cmip6/tmaxRuns/run_10_lim_gt35_ensemble_tmax_", k, "_", yearSpan, ".tif")
#     print(system.time(writeRaster(r_runs, filename = fileName_out,  overwrite = TRUE,  wopt= woptList))); flush.console()
#     
#   }
# }
# yearSpan_end_century <- "2081_2100"
# yearSpan_mid_century <- "2041_2060"
# temp_ssp585_end <- rast(paste0("data/cmip6/tmaxRuns/run_10_lim_gt35_ensemble_tmax_", k, "_", yearSpan_end_century, ".tif"))
# temp_ssp585_mid <- rast(paste0("data/cmip6/tmaxRuns/run_10_lim_gt35_ensemble_tmax_", k, "_", yearSpan_mid_century, ".tif"))
# plot(temp_ssp585_end$lyr.1, main = "Number of tmax runs, minimum length of 10 days, tmax > 35\nSSP585, end century")
# plot(temp_ssp585_end$lyr.2, main = "Tmax number of days in runs, minimum length of 10 days, tmax > 35\nSSP585, end century")
# plot(temp_ssp585_mid$lyr.1, main = "Number of tmax runs, minimum length of 10 days, tmax > 35, SSP585\nmid century")
# plot(temp_ssp585_mid$lyr.2, main = "Tmax number of days in runs, minimum length of 10 days, tmax > 35\nSSP585, mid century")


# f_heatDamage <- function() {
#   # does extreme cold locations and frost and heat damage locations
#   combined <- sds()
#   
#   fileName_tasmax <- paste0(locOfClimFiles, "ensembleMn_dailyMn_20Yr_", k, "_tasmax_", yearSpan, ".tif")
#   tmax <- rast(fileName_tasmax)
#   for (hem in hemispheres) {
#     tmin_hem <- crop(tmin, get(paste0("extent_", hem)))
#     tmax_hem <- crop(tmax, get(paste0("extent_", hem)))
#     spStart <- paste0("floweringStart_", m)
#     spEnd <- paste0("floweringEnd_", m)
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
#     # fileName_out_frostDam <- paste0(locOfDataFiles_perennials, fileNameStart_frost, hem, "_", k, "_", yearSpan, ".tif")
#     # print(system.time(writeRaster(frostCt, fileName_out_frostDam, overwrite = TRUE, wopt = woptList)))
#     # fileNameStart_heat <- paste0("heatCt_hem_")
#     # fileName_out_heatDam <- paste0(locOfDataFiles_perennials, fileNameStart_heat, hem, "_", k, "_", yearSpan, ".tif")
#     # print(system.time(writeRaster(frostCt, fileName_out_heatDam, overwrite = TRUE, wopt = woptList)))
#     # 
#     for (i in c("good", "acceptable", "bad")) {
#       frDays <- switch(i,
#                        "good" = frostRiskDays[1:2],
#                        "acceptable" = frostRiskDays[3:4],
#                        "bad" = frostRiskDays[5:6],
#       )
#       fr <- f_range(frostCt, frDays)
#       hdDays <- switch(i,
#                        "good" = heatRiskDays[1:2],
#                        "acceptable" = heatRiskDays[3:4],
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
#   extremeColdMask <- paste0(locOfDataFiles_perennials, fileNameStart, "_", k, "_", yearSpan, ".tif")
#   extremeColdMask <- rast(extremeColdMask)
#   extremeColdMask_NH <- crop(extremeColdMask, extent_NH)
#   extremeColdMask_SH <- crop(extremeColdMask, extent_SH)
#   
#   for (hem in hemispheres) {
#     # crop out areas where summer heat or flowering frost are greater than the unsuitable values, either unsuitable_floweringFreezeDays or unsuitable_summerHotDays
#     coastline_cropped <- f_crop_custom(coastline, get(paste0("extent_", hem)))
#     coastline_cropped <- st_transform(coastline_cropped, crsRob)
#     
#     for (n in c("flowering", "summer")) {
#       if(n %in% "flowering") { 
#         fileNameStart <- paste0("floweringFrost_hem_")
#         fileName_in <- paste0(locOfDataFiles_perennials, fileNameStart, hem, "_", k, "_", yearSpan, ".tif")
#         print(paste0("fileName_in: ", fileName_in))
#         r <- rast(fileName_in)
#         # crop out areas where temp is below extreme level
#         r <- mask(r, get(paste0("extremeColdMask_", m)), maskvalue = 1)
#         
#         r[r > unsuitable_floweringFreezeDays] <- NA
#         titleText <- paste0("flowering frost danger, scenario ", k, ", period ", gsub("_", "-", yearSpan))
#         # get maximum value for flowering, I'm guessing it is in the historical period
#         fileName_in_max <- paste0(locOfDataFiles_perennials, fileNameStart, hem, "_", "historical", "_", "1991_2010", ".tif")
#         maxVal <- ceiling(max(minmax(rast(fileName_in_max))))
#         coldDamageLimits <- c(0, 5, 20, unsuitable_floweringFreezeDays)
#         bins <- coldDamageLimits
#         caption <- paste0("Note: NA is oceans and land areas that are not suitable because of extreme cold \n(less than ", tminExtremeVal, "°C or more than ", unsuitable_floweringFreezeDays, " frost days in flowering).")
#       }
#       
#       if(n %in% "summer")  {
#         fileNameStart <- paste0("summerHeat_hem_")
#         fileName_in <- paste0(locOfDataFiles_perennials, fileNameStart, hem, "_", k, "_", yearSpan, ".tif")
#         print(paste0("fileName_in: ", fileName_in))
#         r <- rast(fileName_in)
#         # crop out areas where temp is below extreme level
#         r <- mask(r, get(paste0("extremeColdMask_", m)), maskvalue = 1)
#         
#         r[r > unsuitable_summerHotDays] <- NA # unsuitable_summerHotDays is number of days where tmax is greater than tmaxDamageVal
#         titleText <- paste0("Summer heat danger, scenario ", k, ", period ", gsub("_", "-", yearSpan))
#         # get maximum value for summer, I'm guessing it is in the ssp585 at end century 
#         fileName_in_max <- paste0(locOfDataFiles_perennials, fileNameStart, hem, "_", "ssp585", "_", "2081_2100", ".tif")
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
#       fileName_out <- paste0(lofOfGraphicsFiles, "perennials/", fileNameStart, hem, "_", k, "_", yearSpan, ".png")
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


# saving just in case
# f_extreme_cold <- function(k, l){
#   for (hem in hemispheres) {
#     yearSpan <- paste0(l, "_", l + yearRange)
#     if (hem == "NH") startDate <- paste0(l, "-01-01"); endDate <- paste0(l+yearRange, "-12-31"); endYear <- l + yearRange
#     if (hem == "SH") startDate <- paste0(l, "-07-01"); endDate <- paste0(l+yearRange - 1, "-06-30"); endYear <- l + yearRange - 1
#     holder_tmin <- c() # to hold the individual model and year results
#     for (modelChoice in modelChoices_lower) {
#       print(paste0("model: ", modelChoice, ", ssp: ", k, ", hem: ", hem, ", yearSpan: ", yearSpan))
#       fileName_in_hem_tmin <- paste0("data/bigFiles/", modelChoice, "_", k, "_", "tasmin", "_", hem, "_global_daily_", yearSpan, ".tif") # needed for heat stress
#       rastIn_tasmin_hem <- rast(fileName_in_hem_tmin)
#       indices <- format(as.Date(names(rastIn_tasmin_hem), format = "X%Y-%m-%d"), format = "%y") # %y is year 2 digit number
#       indices_num <- as.numeric(indices)
#       indices_num <- indices_num - (l - 2000 - 1)
#       
#       print(system.time(year_tmin1 <- tapp(rastIn_tasmin_hem, indices_num, fun = f_extremecld, extremeColdCutoff))) #, filename = fileName_tmin_out, overwrite = TRUE, wopt = woptList)))
#       
#       
#       #       tmaxSum <- tapp(tmax, indices, fun = function(x, ...){(mean(x >= tmaxLimit))})  # divide by 10 to get the annual average number of days above tmaxLimit
#       # year_tmax needs to be done by counting the days above the extreme value
#       #      print(system.time(year_tmax <- tapp(rastIn_tasmax_hem, indices_num, max)))
#       holder_tmin <- c(holder_tmin, year_tmin)
#       #        holder_tmax <- c(holder_tmax, year_tmax)
#     }
#     r_holder_tmin <- rast(holder_tmin) # combine all the single year rasters into 1
#     r_holder_tmin_mean <- app(r_holder_tmin, mean) # take the mean of all the layers
#     system.time(chillPortion <- app(r, fun = quantile, probs=0.1, na.rm = TRUE)) # means not adequate chill portions probs percent of the time
#     
#   }
# }

# 
#   legendTitle <- "Suitable"
#   
#   #   for (hem in hemispheres) {
#   # crop out areas where summer heat or flowering frost are greater than the unsuitable values, either unsuitable_floweringFreezeDays or unsuitable_summerHotDays
#   #    CPfruit <- CPs$chillRequirement[CPs$crop==fruitSpecies] - old version
#   CPfruit <- cropVals[cropName == fruitSpecies, CR_cultivar_mean]
#   for (i in c("good", "acceptable", "bad")) {
#     fruitSpeciesTitleText <- fruitSpecies
#     if (fruitSpecies %in% paste0("winegrape", var_suffix)) fruitSpeciesTitleText <- "wine grape"
#     titleText <- paste0("Growing conditions for ", fruitSpeciesTitleText," where suitability was classified as ", i, "\n" , "scenario: ", k, ", period: ", gsub("_", "-", yearSpan))
#     if (i == "good") {
#       caption <- paste0("Note: Good growing conditions for ", fruitSpeciesTitleText, " include chill portions of at least ", CPfruit, ", less than ", frostRiskDays[3], 
#                         " days of flowering frost risk and \nless than ", heatRiskDays[3], " days of summer excessive heat. Gray hatching indicates early 21st century area.")
#       colVal <- "green"}
#     if (i == "acceptable") {
#       caption <- paste0("Note: Ok growing conditions for ", fruitSpeciesTitleText, " include chill portions of at least ", CPfruit, ", ", frostRiskDays[3], "-",  frostRiskDays[4], 
#                         " days of flowering frost risk and \n",   heatRiskDays[3], "-",  heatRiskDays[4], " days of summer excessive heat. Gray hatching indicates early 21st century area.")
#       colVal <- "yellow"}
#     if (i == "bad") {
#       caption <- paste0("Note: Bad growing conditions for ",fruitSpeciesTitleText, " include chill portions of at least ", CPfruit, ", ", frostRiskDays[5], "-",  frostRiskDays[6], 
#                         " days of flowering frost risk and \n",  heatRiskDays[5], "-",  heatRiskDays[6], " days of summer excessive heat. Gray shading indicates early 21st century area.")
#       colVal <- "red"}
#     
#     fileName_in_NH <- paste0(locOfDataFiles_perennials, "suitable_", fruitSpecies, "_", k, "_", i, "_", "NH", "_", yearSpan, ".tif")
#     fileName_in_SH <- paste0(locOfDataFiles_perennials, "suitable_", fruitSpecies, "_", k, "_", i, "_", "SH", "_", yearSpan, ".tif")
#     
#     r_nh <- rast(fileName_in_NH)
#     r_sh <- rast(fileName_in_SH)
#     
#     # now get harvested area map
#     fruit_in <- fruitSpecies
#     # if (fruitSpecies == paste0("winegrape", var_suffix)) fruit_in <- "grape"
#     # rInArea <- rast(paste0(areaYieldtifFileLoc, fruit_in,"/",  fruit_in, "_HarvestedAreaHectares.tif"))
#     # harvestArea <- aggregate(rInArea, fact = 6, fun = "sum") # convert 5 arc minutes to 1/2 degrees
#     # #    harvestArea[harvestArea == 0] <- NA
#     # harvestArea[harvestArea < 3] <- NA # set minimum area to be greater than 3 hectares per grid cell
#     # harvestArea[harvestArea > 0] <- 1
#     speciesName <- gsub(var_suffix, "", fruitSpecies) # needed for the harvested area data
#     # the harvested area data is just for grapes so need to get rid of wine in the names
#     if (fruitSpecies == paste0("winegrape", var_suffix)) speciesName <- "grape"
#     fileName_in <- paste0(areaYieldtifFileLoc, speciesName,"/",  speciesName, "_HarvestedAreaHectares.tif")
#     #      if (speciesName == paste0("winegrape", var_suffix)) fileName_in <- paste0(areaYieldtifFileLoc, "grape", "/",  "grape", "_HarvestedAreaHectares.tif")
#     
#     rInArea <- rast(fileName_in)
#     harvestArea <- aggregate(rInArea, fact = 6, fun = "sum") # convert 5 arc minutes to 1/2 degrees
#     maskMin <- switch(
#       speciesName,
#       "cherry" = 50,
#       "almond" = 50,
#       "grape" = 50,
#       "apple" = 50,
#       "olive" = 50
#     )
#     
#     harvestArea[harvestArea < maskMin] <- NA # set minimum area to be greater than 3 hectares per grid cell
#     harvestArea[harvestArea > 0] <- 1
#     harvestArea <- crop(harvestArea, c( -180, 180, -60, 90))
#     
#     r <- merge(r_nh, r_sh)
#     
#     r <- project(r, crsRob)
#     harvestArea <- project(harvestArea, crsRob)
#     r_df <- as.data.frame(r, xy = TRUE)
#     names(r_df) <-   c("x", "y", "value")
#     harvestArea_df <- as.data.frame(harvestArea, xy = TRUE)
#     names(harvestArea_df) <-   c("x", "y", "value")
#     fileName_out <- paste0(lofOfGraphicsFiles, "perennials/", fruitSpecies, "_", i, "_", k, "_", yearSpan, ".png")
#     
#     # g <- ggplot(data = coastline_cropped) +
#     #    geom_sf(fill = NA, color = "gray", lwd = 0.1) +
#     #     theme(plot.title = element_text(size = 12, hjust = 0.5), plot.caption = element_text(hjust = 0, size = 6)) +
#     # 
#     
#     # do without legend
#     colValBase <- "grey69"
#     g <- ggplot() +
#       geom_tile(data = r_df, aes(x, y), fill = colVal) +
#       #              scale_fill_discrete(colors = col, drop = FALSE, na.value = 'grey95') +
#       #scale_fill_manual(values = col, drop = FALSE, na.value = 'grey95') + # the na.value doesn't work yet. See https://stackoverflow.com/questions/45144630/scale-fill-manual-define-color-for-na-values/45147172 for a possible solution
#       labs(title = titleText,  x = "", y = "", caption = caption) + #fill = legendTitle,
#       theme_bw()  +
#       #      theme(plot.title = element_text(size = 12, hjust = 0.5), plot.caption = element_text(hjust = 0, size = 8)) +
#       theme(
#         legend.text.align = 1,
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(size = 12, hjust = 0.5),
#         plot.caption = element_text(hjust = 0, vjust = 7.0, size = 8)
#       ) +
#       geom_sf(data = coastline_cropped , 
#               color = "black", size = 0.1, stat = "sf", fill = NA,
#               position = "identity") +
#       geom_tile(data = harvestArea_df, aes(x, y), fill = colValBase, alpha = 0.6, show.legend = FALSE) # +
#     #   scale_fill_gradient(low = "gray", high = "gray")
#     #    scale_fill_grey()
#     
#     print(g)
#     ggsave(filename = fileName_out, plot = g, width = 8, height = 4, units = "in", dpi = 300)
#     knitr::plot_crop(fileName_out) # gets rid of margins around the plot
#     print(paste0("file name out: ", fileName_out))
#     g <- NULL
#   }

# f_extremeValues <- function(k, l, varChoice) { #varChoice is varieties_lo, varieties_hi, varieties_main
#   for (hem in hemispheres) {
#     yearSpan <- paste0(l, "_", l + yearRange)
#     if (hem == "NH") startDate <- paste0(l, "-01-01"); endDate <- paste0(l+yearRange, "-12-31"); endYear <- l + yearRange
#     if (hem == "SH") startDate <- paste0(l, "-07-01"); endDate <- paste0(l+yearRange - 1, "-06-30"); endYear <- l + yearRange - 1
#     holder_tmax <- c() # to hold the individual model and year results
#     for (modelChoice in modelChoices_lower) {
#       print(paste0("model: ", modelChoice, ", ssp: ", k, ", hem: ", hem, ", yearSpan: ", yearSpan))
#       fileName_in_hem_tmax <- paste0("data/bigFiles/", modelChoice, "_", k, "_", "tasmax", "_", hem, "_global_daily_", yearSpan, ".tif") # needed for heat stress
#       rastIn_tasmax_hem <- rast(fileName_in_hem_tmax)
#       indices <- format(as.Date(names(rastIn_tasmax_hem), format = "X%Y-%m-%d"), format = "%y") # %y is year 2 digit number
#       indices_num <- as.numeric(indices)
#       # year_tmax needs to be done by counting the days above the extreme value
#       #      print(system.time(year_tmax <- tapp(rastIn_tasmax_hem, indices_num, max)))
#       holder_tmin <- c(holder_tmin, year_tmin)
#       #        holder_tmax <- c(holder_tmax, year_tmax)
#     }
#     r_holder_tmax <- rast(holder_tmax) # combine all the single year rasters into 1
#     r_holder_tmax_mean <- app(r_holder_tmax, mean) # take the mean of all the layers
#   }
# }
# 

# this function seems to be no longer needed but keeping it around just in case
# f_favorableLocs <- function(k, l, speciesChoice) { 
#   for (hem in hemispheres) {
#     fileName_in_baseCt <- paste0(locOfDataFiles_perennials, "suitable_", speciesChoice, "_", k, "_", hem, "_", yearSpan, ".tif")
#     baseCt <- rast(fileName_in_baseCt)  #suitable/not suitable - 1/NA - because some combo of chill portions, extreme cold, and frost or heat outside the suitable level
#     
#     #frostRiskDays
#     fileName_fr_in <- paste0(locOfDataFiles_perennials, "floweringDamage_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
#     fr_mean <- rast(fileName_fr_in) # number of days with temps below zero
#     fr_mean[fr_mean > 0] <- 1
#     
#     #heatRiskDays
#     fileName_hd_in <- paste0(locOfDataFiles_perennials, "heatDamage_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
#     hd_mean <- rast(fileName_hd_in)
#     hd_mean[hd_mean > 0] <- 1
#     
#     for (suitabilityLevel in suitabilityLevels) {
#       frDays <- switch(
#         suitabilityLevel,
#         "good" = frostRiskDays[1:2],
#         "acceptable" = frostRiskDays[3:4],
#         "bad" = frostRiskDays[5:6],
#       )
#       fr <- f_range(frostCt, frDays) # f_range sets values at locations outside the range to be NA
#       hdDays <- switch(
#         suitabilityLevel,
#         "good" = heatRiskDays[1:2],
#         "acceptable" = heatRiskDays[3:4],
#         "bad" = heatRiskDays[5:6],
#       )
#       hr <- f_range(heatCt, hdDays)
#     }
#     r <- mask(get(paste0("heatRisk_", suitabilityLevel)), baseCt)
#     r <- mask(r, get(paste0("frostRisk_", suitabilityLevel)))
#     fileName_out <- paste0(locOfDataFiles_perennials, speciesChoice, "_frost_", suitabilityLevel, "_heat_", suitabilityLevel, "_", k, "_", hem, "_", yearSpan, ".tif")
#     print(paste0("file name out: ", fileName_out))
#     writeRaster(r, fileName_out, overwrite = TRUE, wopt = woptList)
#   }
# }



# code to do various perennial crop calculations
{
  #source("R/globallyUsed.R")
  require(terra)
  library(ggplot2)
  source("R/ISIMIPconstants.R")
  source("R/ISIMIPspatialConstants.R")
  source("R/perennialsPrep.R") # creates the data tables majorCropValues_main, majorCropValues_lo and majorCropValues_hi
  # library(Rcpp)
  # sourceCpp("R/cpp/gdd.cpp")
  
  options(warn = 1)
  # file locations -----
  #  locOfClimFiles <- "/Volumes/ExtremeSSD2/ISIMIP/cmip6/"
  locOfClimFiles <- "climdata/"
  locOfgddsFiles <- "data/cmip6/growingDegreeDays/"
  # constants, general
  
  # choose whether to do the base cps, or the lo or hi cp requirements varieties -----
  varietiesChoices <- c("varieties_lo", "varieties_main", "varieties_hi")
  suitabilityLevels <- c("good", "acceptable", "bad")
  yearRangeSH <- 18 # one less year because of 6 month offset
  
  varietiesChoice <- "varieties_main"
  varietiesChoiceSuffix <- gsub("varieties", "", varietiesChoice) # whether to use lo, main, or hi cp values
  
  # constants, perennials -----
  minimumGrwSeasonLength = 100
  # All day numbers are Julian calendar days
  # The spring frost window is NH = 60:120 (1Mar:30Apr); 227:288 (SH = 15Aug:15Oct)
  springFrostLength <- 60
  heatDamageLength <- 60
  chillPortionWindow <- 214
  springStart_NH <- 60 # March 1 in 2019
  springStart_SH <- 227 # Aug 15 in 2019
  heatDamageStart_NH <- 182 # July 1
  heatDamageStart_SH <- 1 # Jan 1
  chillPortionStart_NH <- 272
  chillPortionStart_SH <- 92
  extremeColdCutoff <- -30
  # suitability day counts - first two numbers are good window, second two numbers are acceptable window; third two numbers are bad window; fourth 2 numbers are unsuitable range
  frostRiskDays <- c(0, 6, 7, 20, 21, 45, 46, springFrostLength) 
  heatRiskDays <- c(0, 12, 13, 30, 31, 45, 46, heatDamageLength) 
  
  cropVals <- eval(parse(text = (paste0(" majorCropValues", varietiesChoiceSuffix))))
  
  # variables that define the runs parameters. This code is used to createt the runs values in runsHemisphere.R Copying it here is a kludge. Need to think of a better way to deal with the need for this in f_gddSums
  {
    climVal <- -2 # changed from 0 because subject experts say plants can tolerate this
    test_logic <- paste0("x > ", climVal)
    logicDirection <- ">"
    if (logicDirection == ">") ldtext <-"gt"
    if (logicDirection == "<") ldtext <-"lt"
    runlengthChoices <- c(100) # at least 100 days of tmin > 0
    climateVariable <- "tasmin"
    runsParms <- c(climVal, test_logic, logicDirection, ldtext, runlengthChoices, climateVariable)
  }
  #test values, perennials -----
  speciesChoices <- unique(cropVals$cropName)
  speciesChoice <- "cherry_main"
  suitabilityLevel <- "good"
  climateVarChoice <- "tasmin"
  hem <- "NH"
  
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
  
  f_readRast_count <- function(modelChoice, climateVarChoice, threshold, layersToKeep, probVal, k, l, hem) {
    print(climateVarChoice)
    print(threshold)
    modelChoice_lower <- tolower(modelChoice)
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_hem_in <- paste0(locOfClimFiles, modelChoice_lower, "_", climateVarChoice, "_", k, "_", hem, "_", yearSpan, ".tif")
    print(paste0("climate fileName_hem_in: ", fileName_hem_in))
    r <- rast(fileName_hem_in)
    #    print(r)
    # convert day numbers to calendar days to subset
    # indices values are from 1 to the number of years in the subsetted file, usually 20.
    # layersToKeep are the layer numbers to keep in each year.
    datesToKeep <- c()
    indices <- c()
    for (yearNum in l:(l + yearRange)) {
      temp <- as.Date(layersToKeep, origin = paste0(yearNum, "-01-01"))
      indices_yearNum <- rep(yearNum - l + 1, length(layersToKeep))
      indices <-c(indices, indices_yearNum)
      temp <- paste0("X", temp)
      datesToKeep <- c(datesToKeep, temp)
    }
    r <- subset(r, datesToKeep)
    # for spring frost damage
    if (climateVarChoice == "tasmin") f_ct <- function(x) (sum(x < threshold)) 
    if (climateVarChoice == "tasmax") f_ct <- function(x) (sum(x > threshold)) 
    # print(paste0("length r: ", length(names(r)), ", length indices: ", length(indices)))
    
    print(system.time(tempCt <- tapp(r, indices, f_ct)))
    print(tempCt)
    return(tempCt)
  }
  
  f_readRast_extreme <- function(modelChoice_lower, climateVarChoice, funDir, hem) {
    #   modelChoice_lower <- tolower(modelChoice)
    fileName_hem_in <- paste0(locOfClimFiles, modelChoice_lower, "_", climateVarChoice, "_", k, "_", hem, "_", yearSpan, ".tif")
    # fileName_hem_in <- paste0(locOfClimFiles, "mean_daily/", "mean_daily", "_", climateVarChoice, "_", modelChoice_lower, "_", k, "_", hem, "_", yearSpan, ".tif") 
    print(paste0("climate fileName_hem_in in: ", fileName_hem_in))
    r <- rast(fileName_hem_in)
    print(r)
    indices <- format(as.Date(names(r), format = "X%Y-%m-%d"), format = "%Y") # %Y is year as 4 digit number
    indices <- as.numeric(indices)
    indices <- indices - l  + 1
    print(system.time(extremeTemp <- tapp(r, indices, funDir))) #indices are from 1 to the number of years in the input file. 365 1s, then 365 2s, etc. The funDir is the minimum or maximum value of the temp var. extremeTemp is the highest or lowest temp value in each year in each cell.
    print(extremeTemp)
    return(extremeTemp)
  }
  
  f_extremeCold <- function(k, l, speciesChoice, hem, varietiesChoiceSuffix) {
    yearSpan <- paste0(l, "_", l + yearRange)
    climateVarChoice <- "tasmin"
    funDir <- "min"
    probVal <- 0.80
    speciesName <- gsub(varietiesChoiceSuffix, "", speciesChoice) # needed for the harvested area data
    system.time(x <- lapply(modelChoices_lower, f_readRast_extreme, climateVarChoice, funDir, hem)) # read in tasmin for the relevant period and all ESMs
    r <- rast(x)
    r
    extremeColdCutoff <- cropVals[cropName == speciesChoice, low_temp_threshold] 
    
    # now do ensemble mean and cutoff
    print(paste0("Working on extreme cold for speciesChoice: ", speciesChoice, ", working on ssp: ", k, ", start year ", l, ", hemisphere ", hem))
    fileName_lo_out <- paste0(locOfDataFiles_perennials, "extremeCold_cutoff_", speciesName, "_lo", "_", k, "_", hem, "_", yearSpan, ".tif")
    fileName_main_out <- paste0(locOfDataFiles_perennials, "extremeCold_cutoff_", speciesName, "_main", "_", k, "_", hem, "_", yearSpan, ".tif")
    fileName_high_out <- paste0(locOfDataFiles_perennials, "extremeCold_cutoff_", speciesName, "_hi", "_", k, "_", hem, "_", yearSpan, ".tif")
    print(system.time(extremeCold_quant <- quantile(r, probs = probVal, na.rm = TRUE)))
    extremeCold_quant[extremeCold_quant < extremeColdCutoff] <- 0 #  extreme cold limited
    extremeCold_quant[extremeCold_quant >= extremeColdCutoff] <- 1 # not extreme cold limited
    # rangeVals <- c(extremeColdCutoff, 50)
    # extremeCold_quant <- f_range(extremeCold_quant, rangeVals) 
    # extremeCold_quant[extremeCold_quant >= extremeColdCutoff & extremeCold_quant < 999] <- 1 # use of 999 here and in f_range is a kludge to set non-temp-limited land areas to zero.
    # extremeCold_quant[extremeCold_quant == 999] <- 0
    # 
    print(system.time(writeRaster(extremeCold_quant, filename = fileName_lo_out, overwrite = TRUE, wopt = woptList)))
    print(paste0("fileName_out extreme cold: ", fileName_lo_out))
    file.copy(from = fileName_lo_out, to = fileName_main_out, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    file.copy(from = fileName_lo_out, to = fileName_high_out, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    
    plot(extremeCold_quant, main = speciesChoice)
    return(extremeCold_quant)
  }
  
  f_gddsSuitability <- function(k, l, speciesChoice, hem) {
    yearSpan <- paste0(l, "_", l + yearRange)
    # get gdds ensemble mean
    fileName_in <- paste0(locOfgddsFiles, "ensemble_gddSum_mean", "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
    gdds <- rast(fileName_in)                     
    # get gdd requirements
    gddsRequired <- cropVals[cropName == speciesChoice, gdd]
    gddsSuitable <- gdds
    gddsSuitable[gddsSuitable <  gddsRequired] <- 0 
    gddsSuitable[gddsSuitable >=  gddsRequired] <- 1 
    fileName_out <- paste0(locOfgddsFiles, "gdds_not_limiting", "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
    print(system.time(writeRaster(gddsSuitable, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
    print(paste0("fileName_out suitable gdds: ", fileName_out))
    
    plot(gddsSuitable, main = paste0("Adequate GDDs for ", speciesChoice, ", minimum required ", gddsRequired, ", hemisphere: ", hem, ", scenario: ", k, ", period: ", yearSpan))
    return(gddsSuitable)
  }
  
  # chillPortions calcs done in chillPortions.R -----
  
  f_frostDamage <- function(k, l, speciesChoice, hem, varietiesChoice, suitabilityLevel) {
    yearSpan <- paste0(l, "_", l + yearRange)
    
    # frost damage day windows ---
    spLyrStart <- get(paste0("springStart_", hem))
    spLyrEnd <- spLyrStart + springFrostLength
    spLyrs <- spLyrStart:spLyrEnd
    
    # this really doesn't need to have a range. If the second value says what the cutoff is; lower values are fine
    frDays <- switch(suitabilityLevel,
                     "good" = frostRiskDays[1:2],
                     "acceptable" = frostRiskDays[3:4],
                     "bad" = frostRiskDays[5:6],
                     "unsuitable" = frostRiskDays[7:8]
    )
    
    climateVarChoice <- "tasmin"
    threshold <- cropVals[cropName == speciesChoice, frost_threshold]
    layersToKeep <- spLyrs
    probVal <- 0.90
    system.time(x <- lapply(modelChoices_lower, f_readRast_count, climateVarChoice, threshold, layersToKeep, probVal, k, l, hem))
    r <- rast(x)
    r[r <= frDays[2]] <- 1
    r[r > frDays[2]] <- 0
    #       fr <- f_range(r, frDays)
    system.time(fr <- quantile(r, probs = probVal, na.rm = TRUE)) # note: if all layers have the same value quantile returns that value
    fr[fr > 0] <- 1
    
    titleText_fr <- paste0("Green indicates locations where frost days are not limiting for ", strsplit(speciesChoice, "_")[[1]][1], " during the ", k, " scenario", ", ", gsub("_", "-", yearSpan), ". \nUnsuitable frost risk is more than ", frDays[2], " frost days (-2°C) during the spring frost window.")
    plot(fr, main = titleText_fr)
    fileName_fr_out <- paste0(locOfDataFiles_perennials, "frostDamage_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
    writeRaster(fr, fileName_fr_out, overwrite = TRUE, wopt = woptList)
    print(paste0(" frost damage fileName out: ", fileName_fr_out))
    print("--------------------------------------------")
  }
  
  f_heatDamage <- function(k, l, speciesChoice, hem, varietiesChoice, suitabilityLevel) {
    yearSpan <- paste0(l, "_", l + yearRange)
    hdLyrStart <- get(paste0("heatDamageStart_", hem))
    hdLyrEnd <- hdLyrStart + heatDamageLength
    hdLyrs <- hdLyrStart:hdLyrEnd
    hdDays <- switch(suitabilityLevel,
                     "good" = heatRiskDays[1:2],
                     "acceptable" = heatRiskDays[3:4],
                     "bad" = heatRiskDays[5:6],
                     "unsuitable" = heatRiskDays[7:8]
    )
    climateVarChoice <- "tasmax"
    threshold <- cropVals[cropName == speciesChoice, summer_heat_threshold]
    layersToKeep <- hdLyrs
    probVal <- 0.90
    system.time(x <- lapply(modelChoices_lower, f_readRast_count, climateVarChoice, threshold, layersToKeep, probVal, k, l, hem))
    r <- rast(x)
    r[r <= hdDays[2]] <- 1
    r[r > hdDays[2]] <- 0
    #        hd <- f_range(r, hdDays)
    system.time(hd <- quantile(r, probs = probVal, na.rm = TRUE)) # note: if all layers have the same value quantile returns that value
    hd[hd > 0] <- 1
    #  r[r > 0] <- 1
    titleText_hd <- paste0("1 indicates locations where extreme summer heat is not limiting for ", strsplit(speciesChoice, "_")[[1]][1], " during the ", k, " scenario", ", ", gsub("_", "-", yearSpan), ". \nUnsuitable heat is more than ", hdDays[2], " days above ", threshold, "°C during the summer window.")
    
    plot(hd, main = titleText_hd)
    fileName_hd_out <- paste0(locOfDataFiles_perennials, "heatDamage_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
    writeRaster(hd, fileName_hd_out, overwrite = TRUE, wopt = woptList)
    print(paste0(" heat damage fileName out: ", fileName_hd_out))
    print("--------------------------------------------")
  }
  
  f_combinedDamage <- function(k, l, speciesChoice, suitabilityLevel) {
    # combine suitability metrics from chill portions, extreme cold, spring frost, and summer heat; locations with value 1 is suitable
    yearSpan <- paste0(l, "_", l + yearRange)
    for (hem in hemispheres) {
      # read in all the rasters needed
      fileName_extremeColdCt_in <- paste0(locOfDataFiles_perennials, "extremeCold_cutoff_", speciesChoice, "_", k, "_", hem, "_", yearSpan, ".tif")
      extremeColdCt <- rast(fileName_extremeColdCt_in) 
      fileName_fr_in <- paste0(locOfDataFiles_perennials, "frostDamage_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif") 
      frostCt <- rast(fileName_fr_in) 
      fileName_hd_in <- paste0(locOfDataFiles_perennials, "heatDamage_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif") 
      heatCt <- rast(fileName_hd_in) 
      fileName_cp_in <- paste0("data/cmip6/chillPortions/chill_portions/", "ensemble_chill_cutoff_", speciesChoice, "_", k, "_", hem, "_", yearSpan, ".tif")
      chillPortionsCutoff <- rast(fileName_cp_in) 
      fileName_gdds_in <- paste0(locOfgddsFiles, "gdds_not_limiting", "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
      gdds_suitable <- rast(fileName_gdds_in)
      
      print(paste0("working on combined damage ", speciesChoice, " in hemisphere ", hem, ", year ", l, ", scenario ", k))
      
      r_combined <- c(extremeColdCt, frostCt, heatCt, chillPortionsCutoff, gdds_suitable)
      names(r_combined) <- c("extremeColdSuit", "springFrostSuit", "heatSuit", "chillPortionsSuit", "gddsSuit")
      r_suitable <- app(r_combined, prod)
      names(r_suitable) <- "combinedSuit"
      r_all <- c(r_suitable, r_combined)
      r_suit_hem <- paste0("r_nonlimiting_", hem)
      r_suit_all_hem <- paste0("r_nonlimiting_all_", hem)
      assign(r_suit_hem, r_suitable)
      assign(r_suit_all_hem, r_all)
      # write out hemisphere-specific suitable all files
      fileName_nonlimiting_all_hem_out <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_",  k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
      print(system.time(writeRaster(r_all, filename = fileName_nonlimiting_all_hem_out,  overwrite = TRUE,  wopt= woptList))); flush.console()
    }
    r_nonlimiting_globe <- merge(r_nonlimiting_NH, r_nonlimiting_SH) # just the combined nonlimiting value
    r_nonlimiting_all_globe <- merge(r_nonlimiting_all_NH, r_nonlimiting_all_SH)
    r_nonlimiting_all_globe_df <- as.data.frame(r_nonlimiting_all_globe, xy = TRUE, na.rm = FALSE)
    fileName_nonlimiting_all_out <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_",  k, "_", suitabilityLevel, "_", yearSpan, ".tif")
    print(system.time(writeRaster(r_nonlimiting_all_globe, filename = fileName_nonlimiting_all_out,  overwrite = TRUE,  wopt= woptList))); flush.console()
    fileName_nonlimiting_all_df_out <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_",  k, "_", suitabilityLevel, "_", yearSpan, ".csv")
    write.csv(r_nonlimiting_all_globe_df, fileName_nonlimiting_all_df_out)
    titleText <- paste0("Locations where suitability is ", suitabilityLevel, " for ", strsplit(speciesChoice, "_")[[1]][1], ", during the ", k, " scenario", ", period ", gsub("_", "-", yearSpan))
    pal <- colorRampPalette(c("red", "green"))
    
    plot(r_nonlimiting_globe, main = titleText, legend = FALSE, xlab = FALSE, axes=FALSE, col = pal(2))
    plot(coastline_cropped, add = TRUE)
  }
  
  f_suitableLocsPpt <- function(k, l, yearSpan, suitabilityLevel, speciesChoice) {
    yearSpan <- paste0(l, "_", l + yearRange)
    
    fileName_in <- paste0(lofOfGraphicsFiles, "perennials/", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
    extImg <- external_img(src = fileName_in, width = defaultWidth, height = defaultHeight)
    my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
    my_pres <- ph_with(x = my_pres, value = extImg, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5))
    return(my_pres)
  }
  
  f_suitableLocsGraphics <- function(k, l, speciesChoice, suitabilityLevel) {
    yearSpan <- paste0(l, "_", l + yearRange)
    legendTitle <- suitabilityLevel
    speciesName <- gsub(varietiesChoiceSuffix, "", speciesChoice) # needed for the harvested area data
    # the harvested area data is just for grapes so need to get rid of wine in the names
    if (speciesChoice == paste0("winegrape", varietiesChoiceSuffix)) speciesName <- "grape"
    
    # crop out areas where summer heat or spring frost are greater than the unsuitable values, either unsuitable_springFreezeDays or unsuitable_summerHotDays
    CPfruit <- cropVals[cropName == speciesChoice, chill_portions]
    summerHeat <- cropVals[cropName == speciesChoice, summer_heat_threshold]
    cultivar <-  cropVals[cropName == speciesChoice, cultivar]
    gddsFruit <- cropVals[cropName == speciesChoice, gdd]
    
    titleText <- paste0("Growing conditions for ", speciesName,", cultivar ", cultivar, ", are ", suitabilityLevel, "\n" , "during the ", k, " scenario", ", period ", gsub("_", "-", yearSpan))
    if (suitabilityLevel == "good") {
      # caption <- paste0("Note: Good growing conditions for ", speciesName, ", cultivar ", cultivar, " include chill portions of at least ", CPfruit, ", less than ", frostRiskDays[3], " days of spring frost risk and \nless than ", heatRiskDays[3], " days of summer heat greater than ", summerHeat )#, "°C. Gray chading indicates early 21st century area.")
      captionString <- "Note: Locations (green) not limited by temperature for %s, cultivar %s, include at least %s chill portions, fewer than %s days of spring frost risk, \na minimum of %s growing degree days and %s days of summer heat greater than %s°C. Gray shading indicates early 21st century area \nfor all %s varieties according to data from http://www.earthstat.org. Pink shading indicates early century non-limited areas."
      caption <- sprintf(paste(captionString, collapse = " ") , speciesName, cultivar, CPfruit, frostRiskDays[2], gddsFruit, heatRiskDays[2], summerHeat, speciesName)
      suitcol = "green"}
    
    if (suitabilityLevel == "acceptable") {
      captionString <-   "Note: Acceptable growing conditions for %s, cultivar %s, include at least %s chill portions, %s - %s  days of spring frost risk, \na minimum of %s growing degree days and \n%s - %s days of summer heat greater than %s°C. Gray shading indicates early 21st century area for all %s varieties according to data from \nhttp://www.earthstat.org. Pink shading indicates suitable areas in the beginning of the century."
      caption <- sprintf(paste(captionString, collapse = " ") , speciesName, cultivar, CPfruit, frostRiskDays[3], frostRiskDays[4], gddsFruit, heatRiskDays[3], heatRiskDays[4], summerHeat, speciesName)
      suitcol = "yellow"}
    
    if (suitabilityLevel == "bad") {
      captionString <-   "Note: Bad growing conditions for %s, cultivar %s, include at least %s chill portions, %s - %s  days of spring frost risk, \na minimum of %s growing degree days and \n%s - %s days of summer heat greater than %s°C. Gray shading indicates early 21st century area for all %s varieties according to data from \nhttp://www.earthstat.org. Pink shading indicates suitable areas in the beginning of the century."
      caption <- sprintf(paste(captionString, collapse = " ") , speciesName, cultivar, CPfruit, frostRiskDays[5], frostRiskDays[6], gddsFruit, heatRiskDays[5], heatRiskDays[6], summerHeat, speciesName)
      suitcol = "red"}
    
    # this file has 6 layers. The first one is the combined suitable locations. The list of layer names - "combinedSuit"      "extremeColdSuit"   "springFrostSuit"     "heatSuit"          "chillPortionsSuit  gddsSuit"
    fileName_in <- paste0("data/cmip6/perennials/nonlimiting_all_", speciesChoice, "_", k, "_", suitabilityLevel, "_", yearSpan, ".tif")
    r_combined <- rast(fileName_in)
    r <- r_combined[[1]]
    # if not historical, get historical suitability for use as background shading
    #    if (!k == "historical") {
    fileName_hist_suit_in <- paste0("data/cmip6/perennials/nonlimiting_all_", speciesChoice, "_", "historical", "_", suitabilityLevel, "_", "1991_2010", ".tif")
    r_combined_hist <- rast(fileName_hist_suit_in)
    r_hist <- r_combined_hist[[1]]
    suitableArea_historical <- project(r_hist, crsRob)
    suitableArea_historical_df <- as.data.frame(suitableArea_historical, xy = TRUE)
    names(suitableArea_historical_df) <- c("x", "y", "value")
    suitableArea_historical_df <- round(suitableArea_historical_df, 0)
    suitableArea_historical_df[suitableArea_historical_df == 0] <- NA
    suitableArea_historical_df$value = as.factor(suitableArea_historical_df$value)
    #    }
    # now get harvested area map
    rInArea <- rast(paste0(locOfHarvestDataFiles, speciesName,"/",  speciesName, "_HarvestedAreaHectares.tif"))
    harvestArea_earlyCent <- aggregate(rInArea, fact = 6, fun = "sum") # convert 5 arc minutes to 1/2 degrees
    maskMin <- switch(
      speciesName,
      "almond" = 100,
      "apple" = 100,
      "cherry" = 100,
      "grape" = 100,
      "olive" = 100
    )
    harvestArea_earlyCent[harvestArea_earlyCent < maskMin] <- 0 # set minimum area to be greater than 50 hectares per grid cell
    harvestArea_earlyCent[harvestArea_earlyCent > 0] <- 1
    r <- project(r, crsRob)
    harvestArea_earlyCent <- project(harvestArea_earlyCent, crsRob)
    r_df <- as.data.frame(r, xy = TRUE)
    names(r_df) <-   c("x", "y", "value")
    r_df <- round(r_df, 0)
    r_df$value = as.factor(r_df$value)
    harvestArea_df <- as.data.frame(harvestArea_earlyCent, xy = TRUE)
    names(harvestArea_df) <-   c("x", "y", "value_harvest")
    harvestArea_df <- round(harvestArea_df, 0)
    harvestArea_df[harvestArea_df == 0] <- NA
    harvestArea_df$value_harvest = as.factor(harvestArea_df$value_harvest)
    fileName_out <- paste0(lofOfGraphicsFiles, "perennials/", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
    
    # do without legend
    g <- ggplot() +
      geom_tile(data = r_df, aes(x, y, fill = value), show.legend = FALSE) +
      #      geom_raster(data = r_df, aes(x, y, fill = value)) +
      scale_fill_manual(values = c("white", "green", "grey")) +
      labs(title = titleText, fill = legendTitle, x = "", y = "", caption = caption) + 
      
      geom_sf(data = coastline_cropped_Rob_sf,  color="black", size = 0.2) +
      #     coord_sf(xlim=c(3,35), ylim=c(52,72)) + 
      theme_bw() +
      theme(
        legend.text.align = 1,
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 7.0, size = 7)
      ) +
      #      geom_tile(data = harvestArea_df, aes(x, y), fill = "gray", alpha = .2, show.legend = FALSE)
      geom_tile(data = dplyr::filter(harvestArea_df, !is.na(value_harvest)), 
                aes(x = x, y = y), fill = "grey60", alpha = .2, show.legend = FALSE) +
      geom_tile(data = dplyr::filter(suitableArea_historical_df, !is.na(value)), 
                aes(x = x, y = y), fill = "chocolate1", alpha = .2, show.legend = FALSE) +
      NULL
    
    print(g)
    ggsave(filename = fileName_out, plot = g, width = 8, height = 4, units = "in", dpi = 300)
    knitr::plot_crop(fileName_out) # gets rid of margins around the plot
    print(paste0("file name out: ", fileName_out))
    g <- NULL
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
        plot.caption = element_text(hjust = 0, vjust = 7.0, size = 7)
      ) +
      geom_sf(data = coastline_cropped_Rob_sf , 
              color = "black", size = 0.1, stat = "sf", fill = NA,
              position = "identity") +
      NULL
    print(g)
    ggsave(filename = fileName_out, plot = g, width = 8, height = 4, units = "in", dpi = 300)
    knitr::plot_crop(fileName_out) # gets rid of margins around the plot
  }
  
  f_computeGDDs <- function(k, l, modelChoice, cropVals) {
    print(paste0("start year: ", l, ", ssp: ", k,  " model: ", modelChoice, ", start year: ", l, ", hemisphere: ", hem))
    modelChoice_lower <- tolower(modelChoice)
    yearSpan <- paste0(l, "_", l + yearRange)
    fileName_tas_in <- paste0(locOfClimFiles, modelChoice_lower, "_", "tas", "_", k, "_", yearSpan, ".tif")
    tas <- rast(fileName_tas_in)
    print(tas)
    
    # split tas up into individual years and run gdd on those
    # with July 6, 2021 adjustments gddtb and GDD_opt are identical for two groups - almond, apple and cherry in one and olive and winegrape in the other
    # first do gdds for one from each group - choose almond and winegrape
    for (speciesChoice in c("almond_main", "winegrape_main")) {
      #    for (speciesChoice in speciesChoices[!speciesChoices %in% "cherry_main"]) {
      fileName_out <- paste0(locOfgddsFiles, modelChoice_lower, "_", "gdd", "_", speciesChoice, "_", k, "_", yearSpan, ".tif")
      if (!fileName_out %in% gddFilesCompleted) {
        #      print(paste0("Working on: ", fileName_out))
        topt_min <- cropVals[cropName == speciesChoice, gddtb]
        topt_max <- cropVals[cropName == speciesChoice, GDD_opt]
        print(paste0("crop: ", speciesChoice, " topt_min: ", topt_min, " topt_max: ", topt_max, " fileName_out: ", fileName_out))
        print(system.time(gdd <- app(tas, fun = f_gdd, topt_min = topt_min, topt_max = topt_min, cores = 1, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
        print(paste0("gdd file out name: ", fileName_out))
        gdd <- NULL
        gc()
      }else{
        print(paste("This file has already been created: ", fileName_out))
      }
      # copy gdd files to other crops with same gdd ranges
      gddFilesCompleted <- list.files(locOfgddsFiles,  full.names = TRUE)
      gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]
      gddFilesCompleted <- gsub("//", "/", gddFilesCompleted)
      gddFilesCompleted <- gddFilesCompleted[!grepl("gddSum_mean_", gddFilesCompleted, fixed = TRUE)]
      gddFilesCompleted <- gddFilesCompleted[!grepl("ensemble_", gddFilesCompleted, fixed = TRUE)]
      gddFilesCompleted <- gddFilesCompleted[!grepl("gddSum_", gddFilesCompleted, fixed = TRUE)]
      
      
      gddFiles_almond <- gddFilesCompleted[grepl("almond", gddFilesCompleted, fixed = TRUE)]
      gddFiles_winegrape <- gddFilesCompleted[grepl("winegrape", gddFilesCompleted, fixed = TRUE)]
      
      for (i in gddFiles_almond) {
        print(i)
        gdd_cherry <- gsub("almond", "cherry", i)
        print(gdd_cherry)
        file.copy(from = i, to = gdd_cherry, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
        gdd_apple <- gsub("almond", "apple", i)
        file.copy(from = i, to = gdd_apple, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
      }
      gdd_olive <- gsub("winegrape", "olive", gddFiles_winegrape)
      file.copy(from = gddFiles_winegrape, to = gdd_olive, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    }
  }
  
  f_gdd = function(cellVector, topt_min, topt_max) {
    # max1 <- pmin(cellVector, topt_max)-topt_min
    # ycalc <- pmax(0, max1)
    gdd <- terra::clamp(cellVector - topt_min, 0, (topt_max - topt_min), values = TRUE)  
    return(gdd)
  }
  
  f_readRast_gddSum <- function(modelChoice, speciesChoice, k, l, hem) {
    yearSpan <- paste0(l, "_", l + yearRange)
    modelChoice_lower <- tolower(modelChoice)
    fileName_in = paste0(locOfgddsFiles, "gddSum_mean", "_", modelChoice_lower, "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
    print(paste0("speciesChoice: ", speciesChoice, ", k: ", k, ", modelChoice: ", modelChoice, ", fileName in: ", fileName_in))
    r <- rast(fileName_in)
  }
  
  f_gddSums <- function(k, l, speciesChoice, hem, runsParms) {
    logicDirection <- runsParms[3]
    climVal <- runsParms[1]
    climateVariable <- runsParms[6]
    ldtext <- runsParms[4]
    runlength <- runsParms[5]
    yearSpan <- paste0(l, "_", l + yearRange)
    for (modelChoice in modelChoices) {
      modelChoice_lower <- tolower(modelChoice)
      fileName_gdd_in <- paste0(locOfgddsFiles, modelChoice_lower, "_", "gdd", "_", speciesChoice, "_", k, "_", yearSpan, ".tif")
      gdds <- rast(fileName_gdd_in)
      gdds <- crop(gdds, get(paste0("extent_", hem)))
      # gdds are daily for the 20 year period
      # if (hem == "SH")  {startDate <-  paste0(l, "-07-01"); endDate <- paste0(l + yearRange-1, "-06-30")} # in southern hemisphere search July 1 to June 30 of the next year. NH is just the calendar year
      # if (hem == "NH")  {startDate <-  paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")} # in southern hemisphere search July 1 to June 30 of the next year. NH is just the calendar year
      startDate <-  paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
      indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
      indicesChar <- paste0("X", indices)
      # in case gdds doesn't have correct names
      names(gdds) <- indicesChar
      indicesYr <- unique(as.numeric(format(indices, "%Y")))
      if (hem == "SH") indicesYr <- indicesYr[1:yearRange]
      if (!nlyr(gdds) == length(indicesChar)) gdds <- subset(gdds, indicesChar) # if SH, gets rid of the first 1/2 year and  last 1/2 year. may not be necessary because I think gdds in sh file already have this done. If statement may capture this
      fileName_startDay1_in <- paste0(locOfRunsFiles, "startday_1_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
      fileName_endDay1_in <- paste0(locOfRunsFiles, "endday_1_", climateVariable,"_",modelChoice_lower, "_run_", runlength, "_lim_", ldtext, climVal, "_", hem, "_", k, "_", yearSpan, ".tif")
      startDay <- rast(fileName_startDay1_in)
      endDay <- rast(fileName_endDay1_in)
      names(startDay) <-names(endDay) <- sort(unique(indicesYr))
      
      # now do calc by year
      for (yearNumber in 1:nlyr(startDay)) {
        print(paste0("Working on species choice: ", speciesChoice, ", ssp: " , k, ", startYear: ", l, ", hem: ", hem, ", model: ", modelChoice, ", yearNumber: ", yearNumber))
        startDay_yr <- subset(startDay, yearNumber)
        endDay_yr <- subset(endDay, yearNumber)
        startYear <- l + yearNumber - 1
        
        if (hem == "SH")  {
          startDate <-  paste0(startYear, "-07-01"); endDate <- paste0(startYear + 1, "-06-30")} # in southern hemisphere search July 1 to June 30 of the next year. NH is just the calendar year
        if (hem == "NH")  {
          startDate <-  paste0(startYear, "-01-01"); endDate <- paste0(startYear, "-12-31")
        }
        indices <- seq(as.Date(startDate), as.Date(endDate), by = "days")
        indicesChar <- paste0("X", indices)
        #            print(system.time(sum_gdds <- app(gdds_yr, f_sumVec, startDay_yr, endDay_yr)))
        if ((hem == "SH" & yearNumber < 20) | (hem == "NH")) {
          gdds_yr <- subset(gdds, indicesChar)
          #         print(paste0("Just before browser, iteration number: ",  yearNumber))
          #              browser()
        }
        # rapp needs to have a start day that is greater than 0. Next two lines sets all 0 values to 1
        print(system.time(startDay_yr[startDay_yr == 0] <- 1))
        print(system.time(endDay_yr[endDay_yr == 0] <- 1))
        print(system.time(endDay_yr[endDay_yr > nlyr(gdds_yr)] <- nlyr(gdds_yr)))  # needed because the end day can be # 367 if we're in a tropical region; end day is the frost day that ends the run
        print(system.time(sum_gdds <- rapp(gdds_yr, startDay_yr, endDay_yr, "sum")))
        plot(sum_gdds, main = paste0("Sum of gdds in a run of at least 100 days, scenario: ", k, ", period: ", yearSpan, ", year number: ", yearNumber, ", model: ", modelChoice_lower, ", crop: ", speciesChoice))
        if (yearNumber == 1 ) {
          period_sums <- sum_gdds
        } else {
          period_sums <- c(period_sums, sum_gdds)
        }
      }
      gc()
      fileName_gddSums_out <- paste0(locOfgddsFiles, "gddSum", "_", modelChoice_lower, "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
      print(system.time(writeRaster(period_sums, filename = fileName_gddSums_out,  overwrite = TRUE, wopt= woptList))); flush.console()
      print(paste0("fileName_gddSums_out: ", fileName_gddSums_out))
    }
  }
  
  #  Growing season of ‘frost free season’ for all crops is assumed to be the period from last spring frost (defined as -2C) to first autumn frost (Tmin≤-2°C).
  # get a years worth of data
  f_yearSubset <- function(l, yearRange, r, hem) {
    yearSpan <- paste0(l, "_", l + yearRange)
    startDate <- paste0(l, "-01-01"); endDate <- paste0(l, "-12-31") # one year of data
    if (hem == "SH") startDate <-  paste0(yearnum, "-07-01"); endDate <- paste0(yearnum + 1, "-06-30")
    indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    indices <- paste0("X", as.character(indices))
    yearLayers <- subset(r, indexList)
  }
}

# extreme cold calcs, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (hem in hemispheres) {
      for (speciesChoice in speciesChoices) {
        f_extremeCold(k, l, speciesChoice, hem, varietiesChoiceSuffix)
      }
    }
  }
}

# extreme cold calcs, historical -----
k <- "historical"
l <- 1991
for (hem in hemispheres) {
  for (speciesChoice in speciesChoices){
    f_extremeCold(k, l, speciesChoice, hem, varietiesChoiceSuffix)
  }
}

# frost damage, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (hem in hemispheres) {
      for (speciesChoice in speciesChoices) {
        f_frostDamage(k, l, speciesChoice, hem, varietiesChoice, suitabilityLevel)
      }
    }
  }
}

# heat damage, historical -----
k <- "historical"
l <- 1991
for (hem in hemispheres) {
  for (speciesChoice in speciesChoices) {
    f_heatDamage(k, l, speciesChoice, hem, varietiesChoice, suitabilityLevel)
  }
}

# heat damage, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (hem in hemispheres) {
      for (speciesChoice in speciesChoices) {
        f_heatDamage(k, l, speciesChoice, hem, varietiesChoice, suitabilityLevel)
      }
    }
  }
}

# frost damage, historical -----
k <- "historical"
l <- 1991
for (hem in hemispheres) {
  for (speciesChoice in speciesChoices) {
    f_frostDamage(k, l, speciesChoice, hem, varietiesChoice, suitabilityLevel)
  }
}

# gdd calcs ----
gddFilesCompleted <- list.files(locOfgddsFiles,  full.names = TRUE)
gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]
gddFilesCompleted <- gsub("//", "/", gddFilesCompleted)

# gdds, historical -----
k <- "historical"
l <- 1991
for (modelChoice in modelChoices) {
  for (hem in hemispheres) {
    f_computeGDDs(k, l, modelChoice, cropVals) 
  }
}

# gdds, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (modelChoice in modelChoices) {
      for (hem in hemispheres) {
        f_computeGDDs(k, l, modelChoice, cropVals) 
      }
    }
  }
}

# gdd sums, historical ------
k <- "historical"
l <- 1991
for (speciesChoice in c("almond_main", "winegrape_main")) { #}, "winegrape_main")) { #
  for (hem in hemispheres) {
    f_gddSums(k, l, speciesChoice, hem, runsParms)
  }
}

# gdd sums, scenarios ------
for (speciesChoice in c("almond_main", "winegrape_main")) { #
  for (k in sspChoices) {
    for (l in startYearChoices) {
      for (hem in hemispheres) {
        f_gddSums(k, l, speciesChoice, hem, runsParms)
      }
    }
  }
}

# added because almond, apple, and cherry have the same gdd requirements and winegrape and olive also have the same
# first get all the gdd file names and pull out just the ones for almond and winegrape
gddFilesCompleted <- list.files(locOfgddsFiles,  full.names = TRUE)
gddFilesCompleted <- gddFilesCompleted[!grepl("aux.xml", gddFilesCompleted, fixed = TRUE)]
gddFilesCompleted <- gsub("//", "/", gddFilesCompleted)
gddSumFilesCompleted <- gddFilesCompleted[!grepl("gddSum_mean_", gddFilesCompleted, fixed = TRUE)]
gddSumFilesCompleted <- gddSumFilesCompleted[!grepl("ensemble_", gddSumFilesCompleted, fixed = TRUE)]
gddSumFilesCompleted <- gddSumFilesCompleted[grepl("gddSum_", gddSumFilesCompleted, fixed = TRUE)]

gddFiles_almond <- gddSumFilesCompleted[grepl("almond", gddSumFilesCompleted, fixed = TRUE)]
gddFiles_winegrape <- gddSumFilesCompleted[grepl("winegrape", gddSumFilesCompleted, fixed = TRUE)]

for (i in gddFiles_almond) {
  gdd_cherry <- gsub("almond", "cherry", i)
  
  file.copy(from = i, to = gdd_cherry, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  gdd_apple <- gsub("almond", "apple", i)
  file.copy(from = i, to = gdd_apple, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  print(gdd_cherry)
  print(i)
  al <- rast(i)
  plot(al, 1)
  al <- rast(gdd_cherry)
  plot(al, 1)
  
}
for (i in gddFiles_winegrape) {
  gdd_olive <- gsub("winegrape", "olive", i)
  file.copy(from = i, to = gdd_olive, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
}
# GDD sum, means by model, historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
for (speciesChoice in speciesChoices) {
  for (modelChoice in modelChoices) {
    modelChoice_lower <- tolower(modelChoice)
    for (hem in hemispheres) {
      fileName_gddSums_in <- paste0(locOfgddsFiles, "gddSum", "_", modelChoice_lower, "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
      r_in <- rast(fileName_gddSums_in)
      fileName_out = paste0(locOfgddsFiles, "gddSum_mean", "_", modelChoice_lower, "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
      test <- app(r_in, mean, filename = fileName_out, overwrite = TRUE, wopt = woptList)
      print(paste0("fileName_out: ", fileName_out))
    }
  }
}

# GDD sum, means by model, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    # startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    # indices <- seq(as.Date(startDate), as.Date(endDate), 1)
    # indices_day <- as.numeric(format(indices, format = "%j"))
    # 
    for (speciesChoice in speciesChoices) {
      for (modelChoice in modelChoices) {
        modelChoice_lower <- tolower(modelChoice)
        for (hem in hemispheres) {
          fileName_gddSums_in <- paste0(locOfgddsFiles, "gddSum", "_", modelChoice_lower, "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
          r_in <- rast(fileName_gddSums_in)
          fileName_out = paste0(locOfgddsFiles, "gddSum_mean", "_", modelChoice_lower, "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
          test <- app(r_in, mean, filename = fileName_out, overwrite = TRUE, wopt = woptList)
          print(paste0("fileName_out: ", fileName_out))
        }
      }
    }
  }
}

# ensemble GDD sum, historical -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
#startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
for (speciesChoice in speciesChoices) {
  gddsRequired <- majorCropValues_main[cropName == speciesChoice, gdd]
  for (hem in hemispheres) {
    x <- lapply(modelChoices, f_readRast_gddSum, speciesChoice, k, l, hem)
    r <- rast(x)
    indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
    fileName_out <- paste0(locOfgddsFiles, "ensemble_gddSum_mean", "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
    print(paste0("speciesChoice: ", speciesChoice, ", ensemble ssp: ", k, ", start year: ", l , ", fileName out: ", fileName_out))
    print(system.time(r_mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
    main <- paste0("Perennial: ", speciesChoice, ", GDDs required: ", gddsRequired, ", hemisphere: ", hem, ", ssp: ", k, ", period: ", yearSpan)
    plot(r_mean, main = main, axes = FALSE)
  }
}

# ensemble GDD sum, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    #startDate <- paste0(l, "-01-01"); endDate <- paste0(l + yearRange, "-12-31")
    for (speciesChoice in speciesChoices) {
      gddsRequired <- majorCropValues_main[cropName == speciesChoice, gdd]
      for (hem in hemispheres) {
        x <- lapply(modelChoices, f_readRast_gddSum, speciesChoice, k, l, hem)
        r <- rast(x)
        indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
        fileName_out <- paste0(locOfgddsFiles, "ensemble_gddSum_mean", "_", hem, "_",  speciesChoice, "_", k, "_", yearSpan, ".tif")
        print(paste0("speciesChoice: ", speciesChoice, ", ensemble ssp: ", k, ", start year: ", l , ", fileName out: ", fileName_out))
        print(system.time(r_mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, wopt = woptList)))
        main <- paste0("Perennial: ", speciesChoice, ", GDDs required: ", gddsRequired, ", hemisphere: ", hem, ", ssp: ", k, ", period: ", yearSpan)
        plot(r_mean, main = main, axes = FALSE)
      }
    }
  }
}

# gdds suitable, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (hem in hemispheres) {
      for (speciesChoice in speciesChoices) {
        print(paste0("speciesChoice: ", speciesChoice, ", ssp choice: ", k, ", start year: ", l))
        f_gddsSuitability(k, l, speciesChoice, hem) 
      }
    }
  }
}

# gdds suitable, historical -----
k <- "historical"
l <- 1991
for (hem in hemispheres) {
  for (speciesChoice in speciesChoices) {
    print(paste0("speciesChoice: ", speciesChoice, ", ssp choice: ", k, ", start year: ", l))
    f_gddsSuitability(k, l, speciesChoice, hem) 
  }
}

# combined damage, scenarios -----
# code to read in 1/0 metrics files and produce 1/0 tifs where the crop is potentially growable. The chill portions files are created in the chillPortions.R script
# combined damage, historical -----
k <- "historical"
l <- 1991
for (speciesChoice in speciesChoices) {
  print(paste0("speciesChoice: ", speciesChoice, ", ssp choice: ", k, ", start year: ", l))
  f_combinedDamage(k, l, speciesChoice, suitabilityLevel) 
} 

for (k in sspChoices) {
  for (l in startYearChoices) {
    for (speciesChoice in speciesChoices) {
      print(paste0("speciesChoice: ", speciesChoice, ", ssp choice: ", k, ", start year: ", l))
      f_combinedDamage(k, l, speciesChoice, suitabilityLevel) 
    }
  }
}

# area calculations -----
{
  dt_area <- data.table(species = character(), cultivar = character(), chillPortions= numeric(), hemisphere = character(), ssp = character(), yearSpan = character(), quality = character(), area_suitable = numeric(), rasterName = character())
  suitabilityLevels <- "good"
  for (k in sspChoices) {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      for (speciesChoice in speciesChoices) {
        CPfruit <- cropVals[cropName == speciesChoice, chill_portions]
        cultivar <-  cropVals[cropName == speciesChoice, cultivar]
        
        for (hem in hemispheres) {
          for (suitabilityLevel in suitabilityLevels) {
            fileName_in <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
            rastName <- paste0("suitable_", speciesChoice, "_", hem, "_", k, "_", suitabilityLevel, "_", yearSpan)
            r <- rast(fileName_in)
            # r has all 5 suitability layers, get just the combined one
            r <- r$combinedSuit
            r[r == 0] <- NA # only want to get area of cells that have a value of 1; ie, suitable
            r_area <- expanse(r, unit = "km")
            dt_area <- rbind(dt_area, list(speciesChoice, cultivar, CPfruit, hem, k, yearSpan, suitabilityLevel, r_area, rastName))
          }
        }
      }
    }
  }
  
  k <- "historical"
  l <- 1991
  yearSpan <- paste0(l, "_", l + yearRange)
  for (speciesChoice in speciesChoices) {
    CPfruit <- cropVals[cropName == speciesChoice, chill_portions]
    cultivar <-  cropVals[cropName == speciesChoice, cultivar]
    for (hem in hemispheres) {
      for (suitabilityLevel in suitabilityLevels) {
        fileName_in <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_", k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
        r <- rast(fileName_in) 
        # r has all 5 suitability layers, get just the combined one
        r <- r$combinedSuit
        r[r == 0] <- NA # only want to get area of cells that have a value of 1; ie, suitable
        r_area <- expanse(r, unit = "km")
        dt_area <- rbind(dt_area, list(speciesChoice, cultivar, CPfruit, hem, k, yearSpan, suitabilityLevel, r_area, rastName))
      }
    }
  }
  fileName_out <- paste0(locOfDataFiles_perennials, "areaCalcs", varietiesChoiceSuffix, ".csv")
  write.csv(dt_area, file = fileName_out, row.names = FALSE)
  print(paste0("fileName out: ", fileName_out))
}

# area deltas, need to run code above first -----
dt_area_delta <- data.table(species = character(), cultivar = character(), chillPortions= numeric(), hemisphere = character(), ssp_base = character(), ssp = character(), yearSpan = character(), quality = character(), area_base = numeric(), area_delta = numeric(), delta_share = numeric())
for (speciesChoice in speciesChoices) {
  CPfruit <- cropVals[cropName == speciesChoice, chill_portions]
  cultivar <-  cropVals[cropName == speciesChoice, cultivar]
  for (hem in hemispheres) {
    for (suitabilityLevel in suitabilityLevels) {
      r_historical <- rast(paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_", "historical", "_", suitabilityLevel, "_", hem, "_", "1991_2010.tif"))
      r_historical[r_historical == 0] <- NA
      area_base <- expanse(r_historical, unit = "km")
      for (l in startYearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        for (k in sspChoices) {
          rastName <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_",  k, "_", suitabilityLevel, "_", hem, "_", yearSpan, ".tif")
          r_delta <- rast(rastName) - r_historical
          r_delta[r_delta == 0] <- NA
          r_delta_area <- expanse(r_delta, unit = "km")
          delta_ratio <- r_delta_area/area_base
          dt_area_delta <- rbind(dt_area_delta, list(speciesChoice, cultivar, CPfruit, hem, "historical", k, yearSpan, suitabilityLevel, area_base, r_delta_area, delta_ratio))
        }
      }
    }
  }
}

fileName_out <- paste0(locOfDataFiles_perennials, "areaCalcs_delta", varietiesChoiceSuffix, ".csv")
write.csv(dt_area_delta, file = fileName_out, row.names = FALSE)
print(paste0("fileName out: ", fileName_out))

# harvest and suitability area calcs ------
# area common to early and end century -----
dt_area_common <- data.table(species = character(), cultivar = character(), chillPortions= numeric(), hemisphere = character(), ssp = character(), area_common = numeric())
for (speciesChoice in speciesChoices) {
  CPfruit <- cropVals[cropName == speciesChoice, chill_portions]
  cultivar <-  cropVals[cropName == speciesChoice, cultivar]
  speciesName <- gsub(varietiesChoiceSuffix, "", speciesChoice) # needed for the harvested area data
  # the harvested area data is just for grapes so need to get rid of wine in the names
  if (speciesChoice == paste0("winegrape", varietiesChoiceSuffix)) speciesName <- "grape"
  fileName_in <- paste0(locOfHarvestDataFiles, speciesName,"/",  speciesName, "_HarvestedAreaHectares.tif")
  rInArea <- rast(fileName_in)
  harvestArea_earlyCent <- aggregate(rInArea, fact = 6, fun = "sum") # convert 5 arc minutes to 1/2 degrees
  # crop to no Antarctica
  harvestArea_earlyCent <- crop(harvestArea_earlyCent, extent_noAntarctica)
  # mask to land only
  harvestArea_earlyCent <- mask(harvestArea_earlyCent, landOnlyMaskNoAntarctica, maskvalues = 0)
  maskMin <- switch(
    speciesName,
    "almond" = 10,
    "apple" = 10,
    "cherry" = 10,
    "grape" = 10,
    "olive" = 10
  )
  #  harvestArea_earlyCent[harvestArea_earlyCent < maskMin] <- NA # set minimum area to be greater than maxmin hectares per grid cell
  harvestArea_earlyCent[harvestArea_earlyCent < maskMin] <- 0 # set minimum area to be greater than maxmin hectares per grid cell
  harvestArea_earlyCent[harvestArea_earlyCent > 0] <- 1
  harvestArea_earlyCent_NH <- crop(harvestArea_earlyCent, extent_NH)
  harvestArea_earlyCent_SH <- crop(harvestArea_earlyCent, extent_SH)
  
  for (k in sspChoices) {
    #suitable areas -----
    #   suitabilityLevel <- "good" defined above. Don't need this and could cause confusion.
    # suitable area end century
    yearSpan <- "2081_2100"
    fileName_in_SH <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_", k, "_", suitabilityLevel, "_", "SH", "_", yearSpan, ".tif")
    fileName_in_NH <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_", k, "_", suitabilityLevel, "_", "NH", "_", yearSpan, ".tif")
    suitableArea_endCent_SH <- rast(fileName_in_SH)
    suitableArea_endCent_NH <- rast(fileName_in_NH)
    # get just the overall suitability
    suitableArea_endCent_SH <- suitableArea_endCent_SH$combinedSuit
    suitableArea_endCent_NH <- suitableArea_endCent_NH$combinedSuit
    
    suitableArea_endCent <- merge(suitableArea_endCent_SH, suitableArea_endCent_NH)
    r_combined <- c(harvestArea_earlyCent, suitableArea_endCent)
    commonArea <- app(r_combined, prod) #???
    
    commonArea_NH <- suitableArea_endCent_NH * harvestArea_earlyCent_NH # changed to * June 9, 2021
    commonArea_SH <- suitableArea_endCent_SH * harvestArea_earlyCent_SH
    commonArea_NH[commonArea_NH == 0] <- NA
    commonArea_SH[commonArea_SH == 0] <- NA
    
    cellSize(commonArea_NH, mask=TRUE, unit="km")
    
    dt_area_common <- rbind(dt_area_common, list(speciesChoice, cultivar, CPfruit, "NH", k, expanse(commonArea_NH, unit = "km")))
    dt_area_common <- rbind(dt_area_common, list(speciesChoice, cultivar, CPfruit, "SH", k, expanse(commonArea_SH, unit = "km")))
  }# end of ssp loop
  
} 
# fileName_out <- paste0(locOfDataFiles_perennials, "areaCalcs_common", "_", k, varietiesChoiceSuffix, ".csv")
# write.csv(dt_area_common, file = fileName_out, row.names = FALSE)
# print(paste0("fileName out: ", fileName_out))

# create table of area changes ------
dt_area <- as.data.table(read.csv(file = paste0(locOfDataFiles_perennials, "areaCalcs", varietiesChoiceSuffix, ".csv")))
# fileName_in <- paste0(locOfDataFiles_perennials, "areaCalcs_common", "_", k, varietiesChoiceSuffix, ".csv")
# dt_area_common <- as.data.table(read.csv(fileName_in))

# delete rows that are for suitability bad or acceptable and mid century - keeping just early and end century
dt_area <- dt_area[!quality %in% c("bad", "acceptable") & !yearSpan %in% "2041_2060",]
dt_area[, c("quality", "rasterName", "yearSpan") := NULL]
dt_area_wide <- dcast(dt_area, species + cultivar + chillPortions + hemisphere ~ ssp, value.var = "area_suitable")
dt_area_common_wide <- dcast(dt_area_common, species + cultivar + chillPortions + hemisphere ~ ssp, value.var = "area_common")
setnames(dt_area_common_wide, old = c("ssp126", "ssp585"), c("ssp126_common", "ssp585_common"))
# dt_area_common[, ssp := NULL]
combined <- merge(dt_area_wide, dt_area_common_wide)
# convert units
colsToConvert <- c("historical", "ssp126", "ssp585", "ssp126_common", "ssp585_common")
combined[, (colsToConvert) := lapply(.SD, '/', 1000), .SDcols = (colsToConvert)] # convert to 1000 sq km
combined[, ratioEnd2Early_126 := -1 + ssp126/historical]
combined[, ratioEnd2Early_585 := -1 + ssp585/historical]
combined[, ratioCommon2Early_126 := ssp126_common/historical]
combined[, ratioCommon2Early_585 := ssp585_common/historical]
combined[, RatioLossOfEarly_585 := (historical - ssp585_common)/historical]
combined[, RatioLossOfEarly_126 := (historical - ssp126_common)/historical]
write.csv(combined, file = paste0(locOfDataFiles_perennials, "sumTable", varietiesChoiceSuffix, ".csv"), row.names = FALSE)

# prepare summary table ------
library(flextable)
sumTable <- as.data.table(read.csv(file = paste0(locOfDataFiles_perennials, "sumTable", varietiesChoiceSuffix, ".csv")))
ratioColumns <- c("ratioEnd2Early_126", "ratioEnd2Early_585", "ratioCommon2Early_126", "ratioCommon2Early_585", "RatioLossOfEarly_585", "RatioLossOfEarly_126")
sumColumns <- c("historical", "ssp126", "ssp585", "ssp126_common", "ssp585_common")
ssp126Columns <- c("ssp126", "ssp126_common", "ratioEnd2Early_126", "ratioCommon2Early_126", "RatioLossOfEarly_126") 
sumTable[, (ratioColumns):= round(.SD, 2), .SDcols = ratioColumns]
sumTable[, (sumColumns):= round(.SD, 0), .SDcols = sumColumns]
sumTable[, species := gsub("_main", "", species)]
sumTable[, chillPortions := NULL] # column not needed for presentation table
# focus just on SSP585
sumTable[, c(ssp126Columns, "ratioCommon2Early_585", "RatioLossOfEarly_585") := NULL]
sumTable_flex <- flextable(sumTable)
# sumTable_flex <- set_header_labels(sumTable_flex, values = list(
#   species = "species", cultivar = "cultivar", hemisphere = "hemisphere", 
#   historical = "early century area", ssp585 = "end century area, SSP585", ssp585_common = "common area, SSP end century and early century", ratioEnd2Early_585 = "area ratio, end spp585 to beginning"))
typology <- data.frame(
  col_keys = names(sumTable),
  what = c("Species", "Cultivar", "Hemi-\nsphere", "Area (sq. km)", "Area (sq. km)","Area (sq. km)", "Area ratio, \nend to early century"),
  measure = c("Species", "Cultivar", "Hemi-\nsphere", "Early century", "End century", "Common to both periods", "Area ratio, \nend to early century"),
  stringsAsFactors = FALSE )

sumTable_flex <- set_header_df(sumTable_flex, mapping = typology, key = "col_keys")
sumTable_flex <- merge_h(sumTable_flex, part = "header")
sumTable_flex <- merge_v(sumTable_flex, j = c("species", "cultivar", "hemisphere", "ratioEnd2Early_585"), part = "header")
sumTable_flex <- theme_vanilla(sumTable_flex)
sumTable_flex <- fix_border_issues(sumTable_flex)
sumTable_flex <- autofit(sumTable_flex)
sumTable_flex <- width(sumTable_flex, j = 7, width = 1.5)
sumTable_flex <- width(sumTable_flex, j = 6, width = 1.0)
sumTable_flex <- width(sumTable_flex, j = 3, width = 0.75)
#sumTable_flex <- fit_to_width(sumTable_flex, 8, inc = 1L, max_iter = 20)

sumTable_flex <- footnote(sumTable_flex, i = NULL, j = c(5,7),
                          value = as_paragraph(
                            c("SSP585")
                          ),
                          ref_symbols = "a", part = "header", inline = FALSE, sep = "; "
)
sumTable_flex <- align(sumTable_flex, align = "center", i = 1, j = 4, part = "header")
sumTable_flex <- height(sumTable_flex, height = .25, part = "body")
flextable_dim(sumTable_flex)

#suitable locations graphics, historical -----
k <- "historical"
l <- 1991
for (speciesChoice in speciesChoices) {
  print(paste0("Suitability level:  ", suitabilityLevel, ", speciesChoice: ", speciesChoice, ", ssp choice: ", k, ", start year: ", l))
  f_suitableLocsGraphics(k, l, speciesChoice, suitabilityLevel)
}

#suitable locations graphics, scenarios -----
for (k in sspChoices) {
  for (l in startYearChoices) {
    for (speciesChoice in speciesChoices) {
      print(paste0("Suitability level:  ", suitabilityLevel, ", speciesChoice: ", speciesChoice, ", ssp choice: ", k, ", start year: ", l))
      f_suitableLocsGraphics(k, l, speciesChoice, suitabilityLevel)
    }
  }
}

# demo slides ------
k = "historical"
l = 1991
yearSpan <- paste0(l, "_", l + yearRange)
speciesChoice <- "cherry_main"
legendTitle <- "Suitable"
defaultColor <- c("green", "red")
CPfruit <- cropVals[cropName == speciesChoice, chill_portions]
gddsFruit <- cropVals[cropName == speciesChoice, gdd]
excessiveHeat <- cropVals[cropName == speciesChoice, summer_heat_threshold]

caption <- paste0("Note: ", suitabilityLevel, " growing conditions for ", strsplit(speciesChoice, "_")[[1]][1], ", ", cropVals[cropName == speciesChoice, cultivar], " variety", " include chill portions of at least ", CPfruit, ", at least ", gddsFruit, " growing degree days, \nno more than ", frostRiskDays[2], " days of frost risk during spring and no more than ", heatRiskDays[2], " days of excessive heat (above ", excessiveHeat,  "°C) in summer.")

fileName_nonlimiting_all_NH_in <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_",  k, "_", suitabilityLevel, "_", "NH", "_", yearSpan, ".tif")
suitable_combined_NH <- rast(fileName_nonlimiting_all_NH_in)
suitable_NH <- subset(suitable_combined_NH, "combinedSuit") 
extremeCold_NH <- subset(suitable_combined_NH, "extremeColdSuit") 
unsuitableFrost_NH <- subset(suitable_combined_NH, "springFrostSuit")
unsuitableHeat_NH <- subset(suitable_combined_NH, "heatSuit")
chillPortionsCutoff_NH <- subset(suitable_combined_NH, "chillPortionsSuit")
gddsSuitable_NH <- subset(suitable_combined_NH, "gddsSuit") 

fileName_nonlimiting_all_SH_in <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_",  k, "_", suitabilityLevel, "_", "SH", "_", yearSpan, ".tif")
suitable_combined_SH <- rast(fileName_nonlimiting_all_SH_in)
suitable_SH <- subset(suitable_combined_SH, "combinedSuit") 
extremeCold_SH <- subset(suitable_combined_SH, "extremeColdSuit") 
unsuitableFrost_SH <- subset(suitable_combined_SH, "springFrostSuit")
unsuitableHeat_SH <- subset(suitable_combined_SH, "heatSuit")
chillPortionsCutoff_SH <- subset(suitable_combined_SH, "chillPortionsSuit")
gddsSuitable_SH <- subset(suitable_combined_SH, "gddsSuit") 

extremeCold <- merge(extremeCold_NH,  extremeCold_SH)
unsuitableFrost <- merge(unsuitableFrost_NH, unsuitableFrost_SH)
unsuitableHeat <- merge(unsuitableHeat_NH, unsuitableHeat_SH)
chillPortionsCutoff <- merge(chillPortionsCutoff_NH, chillPortionsCutoff_SH)
gddsSuitable <- merge(gddsSuitable_NH, gddsSuitable_SH)


# demo slide info
k = "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
r <- extremeCold
col <- rev(defaultColor)
tminExtremeVal <- cropVals[cropName == speciesChoice, low_temp_threshold]
titleText <- paste0("Temperatures below ", tminExtremeVal, "°C for at least one day can kill ", strsplit(speciesChoice, "_")[[1]][1], ". \nResults are for the ", k, " scenario, ", "period ", gsub("_", "-", yearSpan), ". \nGreen areas are not limited by this metric.")
fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_extremeColdMask_", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
f_graphics_demoSlides(r, titleText, caption, fileName_out, col)

r <- chillPortionsCutoff # 1 is where chill portions is adequate 
col <- rev(defaultColor)
titleText <- paste0("Locations where chill portions are adequate for ", strsplit(speciesChoice, "_")[[1]][1], ", ", cropVals[cropName == speciesChoice, cultivar], " variety", ". \nResults are for the ", k, " scenario, ", "period ", gsub("_", "-", yearSpan))
fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_chillPortionsCutoff_", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
f_graphics_demoSlides(r, titleText, caption, fileName_out, col)

{
  # these next lines of code must stay in this order.
  # titleText <- "Locations where the chill portions are not affected by extreme cold"
  # fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_chillPortionsGood", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
  # f_graphics_demoSlides(r, titleText, caption, fileName_out, col)
  
  r <- unsuitableFrost
  col <- rev(defaultColor)
  titleText = paste0("Locations where the frost days during spring are not limiting for ", strsplit(speciesChoice, "_")[[1]][1], ". \nResults are for the ", k, " scenario, ", "period ", gsub("_", "-", yearSpan))
  fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_springFrostRisk_", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
  f_graphics_demoSlides(r, titleText, caption, fileName_out, col)
  
  r <- unsuitableHeat # 1 is where heat Ct is above the unsuitable level
  col <- rev(defaultColor)
  titleText = paste0("Locations where summer hot days for ",  strsplit(speciesChoice, "_")[[1]][1], " are not limiting.", "\nResults are for the ", k, " scenario, ", "period ", gsub("_", "-", yearSpan))
  fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_summerHeat_", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
  f_graphics_demoSlides(r, titleText, caption, fileName_out, col)
  
  r <- gddsSuitable # 1 is where heat Ct is above the unsuitable level
  col <- rev(defaultColor)
  titleText = paste0("Locations where the growing degree days requirement for ",  strsplit(speciesChoice, "_")[[1]][1], " is met.", "\nResults are for the ", k, " scenario, ", "period ", gsub("_", "-", yearSpan))
  fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_gdds_", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
  f_graphics_demoSlides(r, titleText, caption, fileName_out, col)
}

# r <- heatCt
# col <- defaultColor
# titleText = "Locations in red are where the number of summer hot days are not suitable."
# fileName_out <- paste0(lofOfGraphicsFiles, "perennials/demoSlide_summerHeat_", speciesChoice, "_", suitabilityLevel, "_", k, "_", yearSpan, ".png")
# f_graphics_demoSlides(r, titleText, caption, fileName_out, col)

# powerpoint, suitable regions -----
{library(officer)
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
  titleString <- paste0("Climate change and temperature limits for perennial crops")
  contentString <- paste0("Powerpoint produced on ", Sys.Date())
  
  dataText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
  dataText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/, as corrected in Spring 2021). "
  dataText3 <- "It includes data from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three scenarios (ssp126, ssp370 and ssp585). In this powerpoint, only results using ssp 126 and ssp585 are presented. " 
  dataText4 <- "The data from a 20 year period for the individual models are averaged. "
  #dataText5 <- "Crop area is based on the SAGE cropping calendar data set, based in the early 2000s. Areas that are not cropped have an NA value and are displayed in white. Areas in gray have chilling less than the minimum requirements. Areas in yellow have chilling hours between the lower and upper range of the requirements."
  dataText <- c(dataText1, dataText2, dataText3, dataText4) #, dataText5)
  
  introText1 <- "Temperate perennial fruits face five temperature-based challenges - temperature extremes of cold and heat can severely damage or kill the plant, a minimum period of time during the winter season where temperatures are below freezing (a chilling period) is required, a minimum number of growing degree days is needed and periods during the growing season where high temperatures can cause damage in critical growth periods. These challenges, and specific values, are described in more detail in the table and text on the next page."
  introText2 <- "The initial slides in this ppt show how these weather-based challenges combine to identify locations across the globe today where these temperatures metrics are not limiting.  The remaining slides are specific to individual perennial fruits and show how non-limited locations change in the future under different climate scenarios ."
  introText3 <- 'The next slide shows the values for each of the metrics and their time frame.' 
  introText4 <- 'The definitions of the metrics are: "low temp threshold" - temperature that can kill a plant after a day\'s exposure; "frost threshold" - minimum temperature at which frost damage occurs; "summer heat threshold" - temperature at which heat damages occurs; "GDD base" and "GDD upper optimum" - range of temperatures where growing degree days accumulate; "Required GDDs" - minimum number of growing degree days to reach maturity.'
  introText5 <- paste0("The frost (daily minimum temperature of less than -2°C) damage window extends for ", springFrostLength, " days, beginning on day ", springStart_NH, " in the northern hemisphere and day ", springStart_SH, " in the southern hemisphere. Frost is not limiting if the number of frost days is less than ", frostRiskDays[2], 
                       ". The heat damage window extends for ", heatDamageLength, " days, beginning on day ", heatDamageStart_NH, " in the northern hemisphere and day ", heatDamageStart_SH, " in the southern hemisphere. ", 
                       "The chill portion window extends for ", chillPortionWindow, " days, beginning on day ", chillPortionStart_NH, " in the northern hemisphere and day ", chillPortionStart_SH, " in the southern hemisphere. High heat is not limiting if the number of hot days is less than ", heatRiskDays[2], 
                       ". The growing degree window begins in the first day of a window of at least 100 days with temperatures above the frost threshold.")
  introText6 <- "The table below illustrates the hemisphere-specific impacts of climate change on temperature-based suitability metrics on four perennial species. Early century non-limited area is for the period 1991-2010. End century suitable area is for the period 2081-2100 for the SSP585 scenario. Climate change results both in shifts in non-limited areas towards the poles, a net increase in non-limited areas in the northern hemisphere and a net loss in non-limited areas in the southern hemisphere."
  cropValues <- copy(cropVals)
  setcolorder(cropValues, neworder =  c("cropName","cultivar", "low_temp_threshold", "frost_threshold", "summer_heat_threshold", "gdd", "gddtb", "GDD_opt", "chill_portions"))
  cropValues[, cropName := gsub("_main", "", cropName)]
  setnames(cropValues, old = names(cropValues), new = gsub("_", " ", names(cropValues)))
  setnames(cropValues, old = c("cropName", "gdd", "gddtb", "GDD opt", "chill portions"), new = c("crop name", "Required GDDs", "GDD base", "GDD upper optimum", "Required chill portions"))
  
  fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
  fp_2 <- fp_text(bold = FALSE, font.size = 12)
  fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)
  
  blIntro <- block_list(
    fpar(
      ftext(introText1, fp_2)),
    fpar(),
    fpar(
      ftext(introText2, fp_2))
  )
  
  blIntro2 <- block_list(
    fpar(
      ftext(introText4, fp_2)),
    fpar(),
    fpar(
      ftext(introText5, fp_2))
  )
  
  blIntro3 <- block_list(
    fpar(
      ftext(introText6, fp_2))
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
}

my_pres <- read_pptx()
my_pres <- add_slide(x = my_pres, layout = 'Title Slide', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = titleString, location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(x = my_pres, value = contentString, location = ph_location_type(type = "subTitle"))

# fileName_RTable <- paste0(lofOfGraphicsFiles, "perennials/", "RebeccaTable.png")
# extImg_Rtable <- external_img(src = fileName_RTable, width = defaultWidth, height = defaultHeight)
my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
#my_pres <- ph_with(x = my_pres, value = extImg_Rtable, location = ph_location(left = defaultLeft, top = defaultHeight/2, width = 8, height = 3) )
my_pres <- ph_with(x = my_pres, value = blIntro, location = ph_location_type(type = "body") )

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Metrics", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = blIntro2, location = ph_location_type(type = "body") )
my_pres <- ph_with(x = my_pres, value = flextable(cropValues, cwidth = 1.0), location = ph_location(left = defaultLeft, top = defaultHeight/1.45, width = 8, height = 3) )
my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Area impacts of climate change", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = blIntro3, location = ph_location_type(type = "body") )
my_pres <- ph_with(x = my_pres, value = sumTable_flex, location = ph_location(left = defaultLeft, top = defaultHeight/2.5, width = 6, height = 4) )


# add graphics for the demo slides
k = "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
extremeColdMask <- external_img(src =  paste0(lofOfGraphicsFiles, "perennials/demoSlide_extremeColdMask_cherry_main_good_", k, "_", yearSpan, ".png"), width = defaultWidth, height = defaultHeight)
chillPortionsCutoff <- external_img(src =  paste0(lofOfGraphicsFiles, "perennials/demoSlide_chillPortionsCutoff_cherry_main_good_", k, "_", yearSpan, ".png"), width = defaultWidth, height = defaultHeight)
chillPortions <- external_img(src =  paste0(lofOfGraphicsFiles, "perennials/demoSlide_chillPortionsCutoff_cherry_main_good_", k, "_", yearSpan, ".png"), width = defaultWidth, height = defaultHeight)
springFrost <- external_img(src =  paste0(lofOfGraphicsFiles, "perennials/demoSlide_springFrostRisk_cherry_main_good_", k, "_", yearSpan, ".png"), width = defaultWidth, height = defaultHeight)
#summerHeatBad <- external_img(src =  paste0(lofOfGraphicsFiles, "perennials/demoSlide_summerHeat_cherry_bad_ssp126_2041_2060.png", width = defaultWidth, height = defaultHeight)
summerHeatGood <- external_img(src =  paste0(lofOfGraphicsFiles, "perennials/demoSlide_summerHeat_cherry_main_good_", k, "_", yearSpan, ".png"), width = defaultWidth, height = defaultHeight)
gddsGood <- external_img(src =  paste0(lofOfGraphicsFiles, "perennials/demoSlide_gdds_cherry_main_good_", k, "_", yearSpan, ".png"), width = defaultWidth, height = defaultHeight)

# demo slides in ppt -----
my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = "Demonstration Slides", location = ph_location_type(type = "title"))

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = extremeColdMask, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = chillPortionsCutoff, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

# my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
# my_pres <- ph_with(x = my_pres, value = chillPortions, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = springFrost, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = summerHeatGood, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = gddsGood, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )

ensembleTitle <- paste("Locations where temperature metrics are non-limiting for perennial fruits")
#ensembleBody <- "Results for locations with good suitability." #each quality level are presented sequentially; ie 'good locations for all species, followed by 'ok' locations for all species, and then 'bad' locations for all species."
my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))

# do historical first, then ssps and future periods
for (suitabilityLevel in "good") { #
  for (speciesChoice in speciesChoices) {
    ensembleTitle <- paste("Non-temperature-limited locations for ", strsplit(speciesChoice, "_")[[1]][1]) #, ", Locations quality: ", suitabilityLevel
    my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
    my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
    
    k <- "historical"
    l <- 1991
    yearSpan <- paste0(l, "_", l + yearRange)
    my_pres <- f_suitableLocsPpt(k, l, yearSpan, suitabilityLevel, speciesChoice)
    
    for (k in sspChoices) {
      for (l in startYearChoices) {
        my_pres <- f_suitableLocsPpt(k, l, yearSpan, suitabilityLevel, speciesChoice)
      }
    }
  }
}

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Data Sources", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = blData, location = ph_location_type(type = "body") )

print(my_pres, target = "presentations/cmip6/perennials/summerHeatandspringFrost.pptx") %>% browseURL

# plot points on a raster -----
k <- "historical"
l <- 1991
yearSpan <- paste0(l, "_", l + yearRange)
speciesChoice <- "winegrape_main"
suitabilityLevel = "good"
cropLocations <- as.data.table(read_excel("data-raw/crops/perennials/cropLocations.xlsx"))
#locsVector <- cropLocations[, location := NULL]
speciesName <- gsub("_main", "", speciesChoice)
if (speciesChoice == "winegrape_main") speciesName <- "wine grape"
locsVector <- cropLocations[species == speciesName,]
setnames(locsVector, old = c("lon", "lat"), new = c("x", "y"))
fileName_nonlimiting_all_in <- paste0(locOfDataFiles_perennials, "nonlimiting_all_", speciesChoice, "_",  k, "_", suitabilityLevel, "_", yearSpan, ".tif")
suit_all <- rast(fileName_nonlimiting_all_in)
suit_all <- suit_all$combinedSuit
main = paste0("Locations not temperature limited for ", speciesName, ", period: ", k)
plot(suit_all, main = main)
points(x = locsVector$x, y = locsVector$y)

suit_all_NH <- crop(suit_all, extent_NH)
plot(suit_all_NH, main = main)
points(x = locsVector$x, y = locsVector$y)

extent_Eur <- c(0, 20, 40, 55)
suit_all_eur <- crop(suit_all, extent_Eur)
plot(suit_all_eur, main = main)
points(x = locsVector$x, y = locsVector$y)

# test of graticule package
library(graticule)
xx <- seq(from = extent_Eur[1], to = extent_Eur[2], by = .5)
yy = seq(from = extent_Eur[3], to = extent_Eur[4], by = .5)
#prj <- "+proj=lcc +lon_0=150 +lat_0=-80 +lat_1=-85 +lat_2=-75 +ellps=WGS84"
prj <- "+proj=longlat +datum=WGS84 +no_defs"
plot(suit_all_eur)
grat <- graticule(lons = xx, lats = yy,  xlim = c(extent_Eur[1], extent_Eur[2]), c(ylim = extent_Eur[3], extent_Eur[4]), proj = prj)
plot(grat, add = TRUE)
labs <- graticule_labels(lons = xx, lats = yy, xline = extent_Eur[1], yline = extent_Eur[3],  proj = prj)
op <- par(xpd = NA)
#text(labs, lab = parse(text = labs$lab), pos = c(2, 1)[labs$islon + 1], adj = 1.2)
#text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3)
#text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2)
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 1, cex=0.75, srt = 90)
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, cex=0.75)
par(op)
points(x = locsVector$x, y = locsVector$y)


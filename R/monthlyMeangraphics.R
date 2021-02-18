# script to do monthly mean climate variable graphics
source("R/globallyUsed.R")
library(RColorBrewer)
library(colorspace)# use pal <- choose_palette() to see what this is about
sspChoices <- c("ssp126", "ssp585") #"ssp126", 
#modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startYearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9

climateVars <- c("tave", "tasmax", "tasmin", "pr", "hurs") 
varNamesInfo <- as.data.table(read_excel("data-raw/varNamesLookup.xlsx"))

#test values
k <- "ssp126"
l <- 2021
j = "tave"

jpgHeight = 8
jpgWidth = 8

# ensemble graphics
startYearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data
# get max values for the legends
# l = 2091
# yearSpan <- paste0(l, "_", l + yearRange)
for (k in sspChoices) {

  for (j in climateVars) {
    
  meanData <- c()
#  meanData <- rast(paste0("data/cmip6/monthlyMean/monthlyMean_", j, "_observed_2001_2010.tif"))
  meanData <- rast(paste0("data/cmip6/monthlyMean/ensembleMonthlyMean_", j, "_historical_2001_2010.tif"))
  for (q in startYearChoices_ensemble) {
    yearSpan <- paste0(q, "_", q + yearRange)
    meanData <- c(meanData, rast(paste0("data/cmip6/monthlyMean/ensemblemonthlyMean_", j,  "_",  yearSpan, "_", k, ".tif")))
    print(meanData)
  }
  
# #  climVar_crop <- crop(meanData, regionExt)
#   climVar_crop <- meanData
#   climVar_crop[is.nan(climVar_crop)] <- NA
#   climVarMin <- min(global(climVar_crop, fun = "min", na.rm = TRUE))
#   climVarMax <- max(global(climVar_crop, fun = "max", na.rm = TRUE))
#   climVarMax <- ceiling(climVarMax)
#   climVarMin <- floor(climVarMin)
  
    print(paste0("varname: ", j))
    fileNameMean <- paste0("data/cmip6/monthlyMean/ensembleMonthlyMean_", j,  "_",  yearSpan, "_", k, ".tif") 
    fileNameCV <- paste0("data/cmip6/monthlyMean/ensembleMonthlyCV_", j,  "_",  yearSpan, "_", k, ".tif")
    #  fileNameSD <- paste0("data/cmip6/monthlyMean/ensembleMonthSD_", j,  "_",  yearSpan, "_", k, ".tif")
#    meanData <- rast(fileNameMean) commented out because meanData is calculated above
    CVData <- rast(fileNameCV)
    # SDData <- rast(fileNameSD)
    maxVal_mean <- max(minmax(meanData))
    maxVal_mean <- ceiling(maxVal_mean)
     minVal_mean <- min(minmax(meanData))
    minVal_mean <- floor(minVal_mean)
    assign(paste0("maxVal_mean_", j, "_", k), maxVal_mean)
    assign(paste0("minVal_mean_", j, "_", k), minVal_mean)
    maxVal_CV <- max(minmax(CVData))
    maxVal_CV <- ceiling(maxVal_CV)
    minVal_CV <- min(minmax(CVData))
    minVal_CV <- floor(minVal_CV)
    assign(paste0("maxVal_CV_", j, "_", k), maxVal_CV)
    assign(paste0("minVal_CV_", j, "_", k), minVal_CV)
  }
}

for (k in sspChoices) {
  for (l in startYearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l))
    for (j in climateVars) {
      varName <- j
      print(paste0("varname: ", varName))
      fileNameMean <- paste0("data/cmip6/monthlyMean/ensembleMonthlyMean_", j,  "_",  yearSpan, "_", k, ".tif") 
      fileNameCV <- paste0("data/cmip6/monthlyMean/ensembleMonthlyCV_", j,  "_",  yearSpan, "_", k, ".tif")
      #      fileNameSD <- paste0("data/cmip6/monthlyMean/ensembleMonthSD_", j,  "_",  yearSpan, "_", k, ".tif")
      
      meanData <- rast(fileNameMean)
      # if (j %in% "pr") meanData[ meanData > 30] <- 30 # note that this is where precipitation is capped at 30 mm per day
      meanData <- terra::project(meanData, crsRob)
      CVData <- rast(fileNameCV)
      #   SDData <- rast(fileNameSD)
      CVData <- terra::project(CVData, crsRob)
      names(meanData) <- month.abb
      names(CVData) <- month.abb
      
      # plot Ensemble mean
      varNameLong <- as.character(varNamesInfo[variableShortName %in% varName, variableLongName])
      varNameLongUnits <- as.character(varNamesInfo[variableShortName %in% varName, units])
      titleText <- paste0("Ensemble monthly mean: ", varNameLong, " (", varNameLongUnits, "), ",   gsub("_", "-", yearSpan), ", SSP = ", k)
      col.l <- c("aliceblue", "darkslategray1","blue", "yellow", "brown", "red")
      mapTheme <- rasterTheme(region = col.l)  
      
      minVal_mean_j_k <- get(paste0("minVal_mean_", j, "_", k))
      maxVal_mean_j_k <- get(paste0("maxVal_mean_", j, "_", k))
      myat <- round(seq.int(from = minVal_mean_j_k, to = maxVal_mean_j_k, length = 6))
      if (j %in% "pr") myat <- c(0, 2, 4, 6, 8, 10,  minVal_mean_j_k)
      if (j %in% c("tasmax", "tasmin")) myat <- c(minVal_mean_j_k, 0, 10, 20, 30, 40,  maxVal_mean_j_k)
      
      mapTheme <- rasterTheme(region = col.l)  
      mapTheme$panel.background$col = 'white' 
      
      my.brks=seq.int(0, maxVal_mean_j_k, length = 6)
      myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=myat), space="right")
      
      meanData <- raster::brick(meanData)
      
      g <- levelplot(meanData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                     colorkey = myColorkey, #list(at = myat, col = col.l), margin = F,
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse.Rob, col = "black", lwd = 0.5))
      plotFileName <- paste0("graphics/cmip6/monthlyMean/ensembleMonthlyMean_",  varName, "_",  yearSpan, "_", k, ".jpg")
      print(paste0("plot file name: ", plotFileName, " for climate variable ", varNameLong))
      jpeg(plotFileName, width = jpgWidth, height = jpgHeight, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
      
      # plot Ensemble CV
      titleText <- paste0("Monthly CV, ", varName, "\n ", yearSpan, ", SSP = ", k, ", ensemble CV")
      titleText <- paste0("Ensemble monthly CV: ", varNameLong, ", ",   gsub("_", "-", yearSpan), ", SSP = ", k)
      minVal_CV_j_k <- get(paste0("minVal_CV_", j, "_", k))
      maxVal_CV_j_k <- get(paste0("maxVal_CV_", j, "_", k))
      
      if (j %in% c("tasmax", "tasmin")) myat <- c(0, 1, 2, 3, 4,  maxVal_CV_j_k)
      if (j %in% "hurs") myat <- c(0, 1, 2, 5, 10,  maxVal_CV_j_k)
      if (j %in% "pr") myat <- c(0, 1, 5, 10, 20, maxVal_CV_j_k)
      
      my.brks=seq.int(0, maxVal_CV_j_k, length = 6)
      myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=myat), space="right")
      
      CVData <- raster::brick(CVData)
      
      g <- levelplot(CVData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                     colorkey = myColorkey, #list(at = myat, col = col.l), margin = F,
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse.Rob, col = "black", lwd = 0.5))
      plotFileName <- paste0("graphics/cmip6/monthlyMean/ensembleMonthlyCV_",  varName, "_",  yearSpan, "_", k, ".jpg")
      print(paste0("plot file name: ", plotFileName, " for climate variable ", varNameLong))
      jpeg(plotFileName, width = jpgWidth, height = jpgHeight, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
    }
  }
}

# # do observed data
# yearRange <- 9
# yearSpan <- paste0(l, "_", l + yearRange)
# 
# for (j in climateVars) {
#   varName <- j
#   varNameLong <- as.character(varNamesInfo[variableShortName %in% varName, variableLongName])
#   varNameLongUnits <- as.character(varNamesInfo[variableShortName %in% varName, units])
#   fileNameMean <- paste0("data/cmip6/monthlyMean/monthlyMean_", j, "_observed_", yearSpan, ".tif") 
#   
#   print(paste0("fileName_in: ", fileNameMean))
#   meanData <- rast(fileNameMean)
#   meanData <- terra::project(meanData, crsRob)
#   names(meanData) <- month.abb
#   minVal_mean_j_k <- get(paste0("minVal_mean_", j))
#   maxVal_mean_j_k <- get(paste0("maxVal_mean_", j))
#   
#   myat <- round(seq.int(from = minVal_mean_j, to = maxVal_mean_j, length = 6))
#   if (j %in% "pr") myat <- c(0, 2, 4, 6, 8, 10,  maxVal_mean_j)
#   if (j %in% c("tasmax", "tasmin")) myat <- c(minVal_mean_j, 0, 10, 20, 30, 40,  maxVal_mean_j)
#   
#   # plot mean for observed data
#   titleText <- paste0("Monthly mean, observed data: ", varNameLong, " (", varNameLongUnits, "), ",   gsub("_", "-", yearSpan))
#   # titleText <- paste0("Monthly mean, observed data: ", varNameLong, " (", bquote(degree*C), "), ",   gsub("_", "-", yearSpan))
#   col.l <- c("aliceblue", "darkslategray1","blue", "yellow", "brown", "red")
#   # maxVal <- max(minmax(meanData))
#   # minVal <- min(minmax(meanData))
#   # maxVal <- round(maxVal)
#   # minVal <- round(minVal)
#   mapTheme <- rasterTheme(region = col.l)  
#   mapTheme$panel.background$col = 'white' 
#   
#   my.brks=seq.int(0, maxVal_mean_j, length = 6)
#   myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=myat), space="right")
#   
#   meanData <- raster::brick(meanData)
#   
#   g <- levelplot(meanData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
#                  colorkey = myColorkey, #list(at = myat, col = col.l), margin = F,
#                  xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
#   
#   g <- g + latticeExtra::layer(sp.polygons(coastsCoarse.Rob, col = "black", lwd = 0.5))
#   
#   plotFileName <- paste0("graphics/cmip6/monthlyMean/monthlyMean_",  varName, "_observed_",  yearSpan, ".jpg")
#   jpeg(plotFileName, width = jpgWidth, height = jpgHeight, quality = 100, units = "in", res = 300)
#   print(g)
#   dev.off()
#   
# }


# do historical data
yearRange <- 9
yearSpan <- paste0(l, "_", l + yearRange)

for (j in climateVars) {
  varName <- j
  varNameLong <- as.character(varNamesInfo[variableShortName %in% varName, variableLongName])
  varNameLongUnits <- as.character(varNamesInfo[variableShortName %in% varName, units])
  fileNameMean <- paste0("data/cmip6/monthlyMean/monthlyMean_", j, "_observed_", yearSpan, ".tif") 
  
  print(paste0("fileName_in: ", fileNameMean))
  meanData <- rast(fileNameMean)
  meanData <- terra::project(meanData, crsRob)
  names(meanData) <- month.abb
  minVal_mean_j <- get(paste0("minVal_mean_", j))
  maxVal_mean_j <- get(paste0("maxVal_mean_", j))
  
  myat <- round(seq.int(from = minVal_mean_j, to = maxVal_mean_j, length = 6))
  if (j %in% "pr") myat <- c(0, 2, 4, 6, 8, 10,  maxVal_mean_j)
  if (j %in% c("tasmax", "tasmin")) myat <- c(minVal_mean_j, 0, 10, 20, 30, 40,  maxVal_mean_j)
  
  # plot mean for observed data
  titleText <- paste0("Monthly mean, observed data: ", varNameLong, " (", varNameLongUnits, "), ",   gsub("_", "-", yearSpan))
  # titleText <- paste0("Monthly mean, observed data: ", varNameLong, " (", bquote(degree*C), "), ",   gsub("_", "-", yearSpan))
  col.l <- c("aliceblue", "darkslategray1","blue", "yellow", "brown", "red")
  # maxVal <- max(minmax(meanData))
  # minVal <- min(minmax(meanData))
  # maxVal <- round(maxVal)
  # minVal <- round(minVal)
  mapTheme <- rasterTheme(region = col.l)  
  mapTheme$panel.background$col = 'white' 
  
  my.brks=seq.int(0, maxVal_mean_j, length = 6)
  myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=myat), space="right")
  
  meanData <- raster::brick(meanData)
  
  g <- levelplot(meanData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                 colorkey = myColorkey, #list(at = myat, col = col.l), margin = F,
                 xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
  
  g <- g + latticeExtra::layer(sp.polygons(coastsCoarse.Rob, col = "black", lwd = 0.5))
  
  plotFileName <- paste0("graphics/cmip6/monthlyMean/monthlyMean_",  varName, "_observed_",  yearSpan, ".jpg")
  jpeg(plotFileName, width = jpgWidth, height = jpgHeight, quality = 100, units = "in", res = 300)
  print(g)
  dev.off()
  
}



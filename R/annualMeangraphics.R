# script to do annual mean climate variable graphics
source("R/globallyUsed.R")
library(RColorBrewer)
library(colorspace)# use pal <- choose_palette() to see what this is about
sspChoices <- c("ssp585") #"ssp126", 
#modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9

variableChoices <- c( "tasmax", "tasmin", "tave", "pr", "hurs") 
#variableChoices <- c( "tave") 
varNamesInfo <- as.data.table(read_excel("data-raw/varNamesLookup.xlsx"))

#test value
k <- "ssp585"

jpgHeight = 8
jpgWidth = 8

# ensemble graphics
# apply masks, can only do this to animals we have in THIlist and that have area mask raster
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data
# get max values for the legends
l = 2091
yearSpan <- paste0(l, "_", l + yearRange)
for (j in variableChoices) {
  varName <- j
  print(paste0("varname: ", varName))
  fileNameMean <- paste0("data/cmip6/annualMean/ensembleAnnualMean_", j,  "_",  yearSpan, "_", k, ".tif") 
  fileNameCV <- paste0("data/cmip6/annualMean/ensembleAnnualCV_", j,  "_",  yearSpan, "_", k, ".tif")
#  fileNameSD <- paste0("data/cmip6/annualMean/ensembleAnnualSD_", j,  "_",  yearSpan, "_", k, ".tif")
  meanData <- rast(fileNameMean)
  CVData <- rast(fileNameCV)
 # SDData <- rast(fileNameSD)
  maxVal_mean <- max(minmax(meanData))
  maxVal_mean <- round(maxVal_mean)
  minVal_mean <- min(minmax(meanData))
  minVal_mean <- round(minVal_mean)
  assign(paste0("maxVal_mean_", j), maxVal_mean)
  assign(paste0("minVal_mean_", j), minVal_mean)
  maxVal_CV <- max(minmax(CVData))
  maxVal_CV <- round(maxVal_CV)
  minVal_CV <- min(minmax(CVData))
  minVal_CV <- round(minVal_CV)
  assign(paste0("maxVal_CV_", j), maxVal_CV)
  assign(paste0("minVal_CV_", j), minVal_CV)
}

#test values
k <- "ssp585"
l <- 2021
j = "tasmax"

for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l))
    for (j in variableChoices) {
      varName <- j
      print(paste0("varname: ", varName))
      fileNameMean <- paste0("data/cmip6/annualMean/ensembleAnnualMean_", j,  "_",  yearSpan, "_", k, ".tif") 
      fileNameCV <- paste0("data/cmip6/annualMean/ensembleAnnualCV_", j,  "_",  yearSpan, "_", k, ".tif")
#      fileNameSD <- paste0("data/cmip6/annualMean/ensembleAnnualSD_", j,  "_",  yearSpan, "_", k, ".tif")
      
      meanData <- rast(fileNameMean)
       meanData <- terra::project(meanData, crsRob)
      CVData <- rast(fileNameCV)
   #   SDData <- rast(fileNameSD)
      CVData <- terra::project(CVData, crsRob)
      
      # plot Ensemble mean
      varNameLong <- as.character(varNamesInfo[variableShortName %in% varName, variableLongName])
      varNameLongUnits <- as.character(varNamesInfo[variableShortName %in% varName, units])
      titleText <- paste0("Ensemble annual mean: ", varNameLong, " (", varNameLongUnits, "), \n ",   gsub("_", "-", yearSpan), " SSP = ", k)
      col.l <- c("aliceblue", "darkslategray1","blue", "yellow", "brown", "red")
      mapTheme <- rasterTheme(region = col.l)  
      
      minVal_mean_j <- get(paste0("minVal_mean_", j))
      maxVal_mean_j <- get(paste0("maxVal_mean_", j))
      myat <- round(seq.int(from = minVal_mean_j, to = maxVal_mean_j, length = 7))
      if (j %in% "pr") myat <- c(0, 2, 4, 6, 8, 10,  maxVal_mean_j)
      if (j %in% c("tasmax", "tave", "tasmin")) myat <- c(minVal_mean_j, 0, 8, 16, 24, 32,  maxVal_mean_j)
      
      mapTheme <- rasterTheme(region = col.l)  
      mapTheme$panel.background$col = 'white' 
      
      my.brks=seq.int(0, maxVal_mean_j, length = 7)
      myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=myat), space="right")
     
      meanData <- raster::brick(meanData)
      
      g <- levelplot(meanData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                     colorkey = myColorkey, #list(at = myat, col = col.l), margin = F,
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      g$legend$top <- g$legend$right <-  NULL # get rid of the graphs above and to the right of a single layer levelplot
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse.Rob, col = "black", lwd = 0.5))
      plotFileName <- paste0("graphics/cmip6/annualMean/ensembleAnnualMean_",  varName, "_",  yearSpan, "_", k, ".jpg")
      print(paste0("plot file name: ", plotFileName, " for climate variable ", varNameLong))
      jpeg(plotFileName, width = jpgWidth, height = jpgHeight, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
      
      # plot Ensemble CV
      titleText <- paste0("Ensemble Annual CV: ", varNameLong, ", \n ",   gsub("_", "-", yearSpan), " SSP = ", k)
      minVal_CV_j <- get(paste0("minVal_CV_", j))
      maxVal_CV_j <- get(paste0("maxVal_CV_", j))

     if (j %in% c("tasmax", "tave", "tasmin")) myat <- c(0, 0.5, 1, 1.5, 2, 2.5,  maxVal_CV_j)
      if (j %in% "hurs") myat <- c(0, 1, 2, 5, 10,  maxVal_CV_j)
      if (j %in% "pr") myat <- c(0, 1, 5, 10, 20, maxVal_CV_j)
      
      my.brks=seq.int(0, maxVal_CV_j, length = 7)
      myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=myat), space="right")
      
      CVData <- raster::brick(CVData)
      
      g <- levelplot(CVData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                     colorkey = myColorkey, #list(at = myat, col = col.l), margin = F,
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      g$legend$top <- g$legend$right <-  NULL # get rid of the graphs above and to the right of a single layer levelplot
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse.Rob, col = "black", lwd = 0.5))
      plotFileName <- paste0("graphics/cmip6/annualMean/ensembleAnnualCV_",  varName, "_",  yearSpan, "_", k, ".jpg")
      print(paste0("plot file name: ", plotFileName, " for climate variable ", varNameLong))
      jpeg(plotFileName, width = jpgWidth, height = jpgHeight, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
    }
  }
}

# do observed data
yearRange <- 9
l = 2001
yearSpan <- paste0(l, "_", l + yearRange)

for (j in variableChoices) {
  varName <- j
  varNameLong <- as.character(varNamesInfo[variableShortName %in% varName, variableLongName])
  varNameLongUnits <- as.character(varNamesInfo[variableShortName %in% varName, units])
  fileNameMean <- paste0("data/cmip6/annualMean/annualMean_", j, "_observed_", yearSpan, ".tif") 
  
  print(paste0("filenamein: ", fileNameMean))
  meanData <- rast(fileNameMean)
  meanData <- terra::project(meanData, crsRob)
  minVal_mean_j <- get(paste0("minVal_mean_", j))
  maxVal_mean_j <- get(paste0("maxVal_mean_", j))
  
  myat <- round(seq.int(from = minVal_mean_j, to = maxVal_mean_j, length = 7))
  if (j %in% "pr") myat <- c(0, 2, 4, 6, 8, 10,  maxVal_mean_j)
  if (j %in% c("tasmax", "tave", "tasmin")) myat <- c(minVal_mean_j, 0, 8, 16, 24, 32,  maxVal_mean_j)
  
  # plot mean for observed data
  titleText <- paste0("Annual mean, observed data: ", varNameLong, " (", varNameLongUnits, "), \n ",   gsub("_", "-", yearSpan))
  # titleText <- paste0("Annual mean, observed data: ", varNameLong, " (", bquote(degree*C), "), ",   gsub("_", "-", yearSpan))
  col.l <- c("aliceblue", "darkslategray1","blue", "yellow", "brown", "red")
  # maxVal <- max(minmax(meanData))
  # minVal <- min(minmax(meanData))
  # maxVal <- round(maxVal)
  # minVal <- round(minVal)
  mapTheme <- rasterTheme(region = col.l)  
  mapTheme$panel.background$col = 'white' 
  
  my.brks=seq.int(0, maxVal_mean_j, length = 7)
  myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=myat), space="right")
  
  meanData <- raster::brick(meanData)
  
  g <- levelplot(meanData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                 colorkey = myColorkey, #list(at = myat, col = col.l), margin = F,
                 xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
  g$legend$top <- g$legend$right <-  NULL # get rid of the graphs above and to the right of a single layer levelplot
  g <- g + latticeExtra::layer(sp.polygons(coastsCoarse.Rob, col = "black", lwd = 0.5))

  plotFileName <- paste0("graphics/cmip6/annualMean/annualMean_",  varName, "_observed_",  yearSpan, ".jpg")
  jpeg(plotFileName, width = jpgWidth, height = jpgHeight, quality = 100, units = "in", res = 300)
  print(g)
  dev.off()
  
}




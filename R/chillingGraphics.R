# script to do chilling hours graphics
source("R/globallyUsed.R")
library(RColorBrewer)
library(colorspace)# use pal <- choose_palette() to see what this is about
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

northerHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-  c( -180, 180, -90, 0)
yearRange <- 9

jpgHeight = 4
jpgWidth = 8
chillRequirements <- as.data.table(read_excel("data-raw/crops/chillRequirements.xlsx", range = "a1:e16"))
col.regionsDefault = c("gray70", "yellow", "orange", "red", "brown")

#test values
k <- "ssp585"
l <- 2091
m = "apple"
# get max values for the legends
l = 2091
yearSpan <- paste0(l, "_", l + yearRange)
for (j in fruits) {
  for (k in c("SouthernHem", "NorthernHem")) {
  print(paste0("fruit: ", j))
  fileNameMean <- paste0("data/cmip6/chillingHours/chillHrs_", k, "_ensembleMean_masked_", j,  "_",  yearSpan, "_", k, ".tif")
  fileNameCV <- paste0("data/cmip6/chillingHours/chillHrs_", k, "_ensembleCV_masked_", j,  "_",  yearSpan, "_", k, ".tif") 
#  fileNameSD <- paste0("data/cmip6/chillingHours/chillHrs_", k, "_ensembleSD_masked_", j,  "_",  yearSpan, "_", k, ".tif") 
  meanData <- rast(fileNameMean)
  CVData <- rast(fileNameCV)
#  SDData <- rast(fileNameSD)
  maxVal_mean <- max(minmax(meanData))
  maxVal_mean <- round(maxVal_mean)
  minVal_mean <- min(minmax(meanData))
  minVal_mean <- round(minVal_mean)
  assign(paste0("maxVal_mean_", k, "_", j), maxVal_mean)
  assign(paste0("minVal_mean_", k, "_", j), minVal_mean)
  maxVal_CV <- max(minmax(CVData))
  maxVal_CV <- round(maxVal_CV)
  minVal_CV <- min(minmax(CVData))
  minVal_CV <- round(minVal_CV)
  assign(paste0("maxVal_CV_", k, "_", j), maxVal_CV)
  assign(paste0("minVal_CV_", k, "_", j), minVal_CV)
  }
}

#fruits list is from globallyUsed.R

# read the ensemble mean masked rasters back in and create graphic outputs
for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    for (j in fruits) {
      cropName <- j
      yearSpan <- paste0(l, "_", l + yearRange)
      chillHoursRange <- chillRequirements[crop %in% cropName, common_varieties]
      minChillHours <- as.numeric(unlist(strsplit(chillHoursRange, split = "-"))[1])
      maxChillHours <- as.numeric(unlist(strsplit(chillHoursRange, split = "-"))[2])
      
      fileNameNH_mean <- paste0("chillHrs_NorthernHem_ensembleMean_masked_", cropName, "_", yearSpan, "_", k, ".tif")
      fileNameSH_mean <- paste0("chillHrs_SouthernHem_ensembleMean_masked_",  cropName, "_", yearSpan, "_", k, ".tif")
      fileNameNH_CV <- paste0("chillHrs_NorthernHem_ensembleCV_masked_", cropName, "_", yearSpan, "_", k, ".tif")
      fileNameSH_CV <- paste0("chillHrs_SouthernHem_ensembleCV_masked_",  cropName, "_", yearSpan, "_", k, ".tif")
      
      chillHrsNorthernHem_mean <- rast(paste0("data/cmip6/chillingHours/", fileNameNH_mean))
      chillHrsNorthernHem_mean <- crop(chillHrsNorthernHem_mean, extent(northerHemExtent))
      
      chillHrsSouthernHem_mean <- rast(paste0("data/cmip6/chillingHours/", fileNameSH_mean))
      chillHrsSouthernHem_mean <- crop(chillHrsSouthernHem_mean, extent(southernHemExtent))
      
      chillHrsNorthernHem_CV <- rast(paste0("data/cmip6/chillingHours/", fileNameNH_CV))
      chillHrsNorthernHem_CV <- crop(chillHrsNorthernHem_CV, extent(northerHemExtent))
      
      chillHrsSouthernHem_CV <- rast(paste0("data/cmip6/chillingHours/", fileNameSH_CV))
      chillHrsSouthernHem_CV <- crop(chillHrsSouthernHem_CV, extent(southernHemExtent))
      
       # maxMean <- max(minmax(chillHrsSouthernHem_mean), minmax(chillHrsNorthernHem_mean))
       # maxCV <- max(minmax(chillHrsSouthernHem_CV), minmax(chillHrsNorthernHem_CV))

       col.l <- c("aliceblue", "darkslategray1","blue", "yellow", "brown", "red")
       mapTheme <- rasterTheme(region = col.l)  
#      mapTheme$panel.background$col = 'gray100' 
      mapTheme$panel.background$col = 'white' 
      minVal_mean_j_k <- get(paste0("minVal_mean_",  k, "_", j))
      maxVal_mean_j_k <- get(paste0("maxVal_mean_",  k, "_", j))
      minCV_j_k <- paste0("minVal_CV_", k, "_", j)
      maxCV_j_k <- paste0("maxVal_CV_", k, "_", j)
      
      # Northern hemisphere
      ## mean
      titleText <- paste0("Ensemble mean, Northern hemisphere winter (Nov to Apr)")
#      myat <- round(seq.int(from = 0, to =  maxMean, length.out = 5))
      myat <- round(seq.int(from = minVal_mean_j_k, to = maxVal_mean_j_k, length = 6))
#      myat[2] <- minChillHours; myat[3] <- maxChillHours
      plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_NorthernHem_ensembleMean_masked_", cropName, "_", yearSpan, "_", k, ".jpg")

          my.brks=seq.int(0, maxVal_mean_j_k, length = 6)
      myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=myat), space="right")
      
      
      chillHrsNorthernHem_mean <- terra::project(chillHrsNorthernHem_mean, crsRob)
      chillHrsNorthernHem_mean <- raster::brick(chillHrsNorthernHem_mean)
      
      g <- levelplot(chillHrsNorthernHem_mean,list(label = titleText, side = 1,line = 0.5, cex = 1), col.regions = col.regionsDefault, #margin = FALSE,
                     at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myColorkey),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
      jpeg(plotFileName, width = jpgWidth, height = jpgHeight, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
#      system2('pdfcrop', c(plotFileName, plotFileName))
      
      ##Coefficient of variation
      titleText <- paste0("CV, Northern hemisphere winter (Nov to Apr)")
      myat <- round(seq.int(from = 0, to =  maxCV, length.out = 5))
      #      myat[2] <- minChillHours; myat[3] <- maxChillHours
      plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_NorthernHem_ensembleCV_masked_", cropName, "_", yearSpan, "_", k, ".jpg")
      jpeg(plotFileName, width = jpgWidth, height = jpgWidth, quality = 100, units = "in", res = 300)
      chillHrsNorthernHem_CV <- terra::project(chillHrsNorthernHem_CV, crsRob)
      chillHrsNorthernHem_CV <- raster::brick(chillHrsNorthernHem_CV)
      g <- levelplot(chillHrsNorthernHem_CV, col.regions = col.regionsDefault, margin = FALSE,
                     at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myat),
                     main = list(label = titleText, side = 1,line = 0.5, cex = 1),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
      print(g)
      dev.off()
#      system2('pdfcrop', c(plotFileName, plotFileName))
      
      # Southern hemisphere
      ## mean
      titleText <- paste0("Ensemble mean, Southern hemisphere winter (May to Oct)")
      myat <- round(seq.int(from = 0, to =  maxMean, length.out = 5))
      myat[2] <- minChillHours; myat[3] <- maxChillHours
      plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_SouthernHem_ensembleMean_masked_", cropName, "_", yearSpan, "_", k, ".jpg")
      jpeg(plotFileName, width = jpgWidth, height = jpgWidth, quality = 100, units = "in", res = 300)
      chillHrsSouthernHem_mean <- terra::project(chillHrsSouthernHem_mean, crsRob)
      chillHrsSouthernHem_mean <- raster::brick(chillHrsSouthernHem_mean)
      g <- levelplot(chillHrsSouthernHem_mean, col.regions = col.regionsDefault, margin = FALSE,
                     at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myat),
                     main = list(label = titleText, side = 1,line = 0.5, cex = 1),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
      
      print(g)
      dev.off()
#      system2('pdfcrop', c(plotFileName, plotFileName))
      
      ##Coefficient of variation
      titleText <- paste0("CV, Southern hemisphere winter (May to Oct)")
      myat <- round(seq.int(from = 0, to =  maxCV, length.out = 5))
      #       myat[2] <- minChillHours; myat[3] <- maxChillHours
      
      plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_SouthernHem_ensembleCV_masked_", cropName, "_", yearSpan, "_", k, ".jpg")
      jpeg(plotFileName, width = jpgWidth, height = jpgWidth, quality = 100, units = "in", res = 300)
      chillHrsSouthernHem_CV <- terra::project(chillHrsSouthernHem_CV, crsRob)
      chillHrsSouthernHem_CV <- raster::brick(chillHrsSouthernHem_CV)
      g <- levelplot(chillHrsSouthernHem_CV, col.regions = col.regionsDefault, margin = FALSE,
                     at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myat),
                     main = list(label = titleText, side = 1,line = 0.5, cex = 1),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
      print(g)
      dev.off()
#      system2('pdfcrop', c(plotFileName, plotFileName))
    }
  }
}

# do observed data
yearRange <- 9
yearSpan <- paste0(l, "_", l + yearRange)

for (m in fruits) {
  cropName <- m
  chillHoursRange <- chillRequirements[crop %in% cropName, common_varieties]
  minChillHours <- as.numeric(unlist(strsplit(chillHoursRange, split = "-"))[1])
  maxChillHours <- as.numeric(unlist(strsplit(chillHoursRange, split = "-"))[2])
  
  fileNameNH_mean <- paste0("chillHrs_NorthernHem_masked_", cropName, "_observed_", yearSpan, ".tif")
  fileNameSH_mean <- paste0("chillHrs_SouthernHem_masked_",  cropName, "_observed_", yearSpan, ".tif")
  
  chillHrsNorthernHem_mean <- rast(paste0("data/cmip6/chillingHours/", fileNameNH_mean))
  chillHrsSouthernHem_mean <- rast(paste0("data/cmip6/chillingHours/", fileNameSH_mean))
  
  chillHrsNorthernHem_mean <- crop(chillHrsNorthernHem_mean, extent(northerHemExtent))
  chillHrsSouthernHem_mean <- crop(chillHrsSouthernHem_mean, extent(southernHemExtent))
  maxMean <- max(minmax(chillHrsSouthernHem_mean), minmax(chillHrsNorthernHem_mean))
  
  # plot  mean
  col.l <- c("darkslategray1", "blue", "yellow", "red")
  mapTheme <- rasterTheme(region = col.l)  
  mapTheme$panel.background$col = 'gray100' 
  
  # Northern hemisphere
  ## mean
  titleText <- paste0("Northern hemisphere winter (Nov to Feb)")
  myat <- round(seq.int(from = 0, to =  maxMean, length.out = 5))
  myat[2] <- minChillHours; myat[3] <- maxChillHours
  plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_NorthernHem_masked_", cropName, "_observed_",  yearSpan, ".jpg")
  jpeg(plotFileName, width = jpgWidth, height = jpgWidth, quality = 100, units = "in", res = 300)
  
  chillHrsNorthernHem_mean <- terra::project(chillHrsNorthernHem_mean, crsRob)
  chillHrsNorthernHem_mean <- raster::brick(chillHrsNorthernHem_mean)
  g <- levelplot(chillHrsNorthernHem_mean, col.regions = col.regionsDefault, margin = FALSE,
                 at = myat, par.settings = mapTheme, 
                 colorkey = list(at = myat),
                 main = list(label = titleText, side=1,line = 0.5, cex = 1),
                 xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
  
  g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
  print(g)
  dev.off()
 # system2('pdfcrop', c(plotFileName, plotFileName))
  
  # Southern hemisphere
  ## mean
  titleText <- paste0("Southern hemisphere winter (May to Oct)")
  myat <- round(seq.int(from = 0, to = maxMean, length.out = 5))
  myat[2] <- minChillHours; myat[3] <- maxChillHours
  
  plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_SouthernHem_masked_", cropName, "_observed_",  yearSpan, ".jpg")
  jpeg(plotFileName, width = jpgWidth, height = jpgWidth, quality = 100, units = "in", res = 300)
  chillHrsSouthernHem_mean <- terra::project(chillHrsSouthernHem_mean, crsRob)
  chillHrsSouthernHem_mean <- raster::brick(chillHrsSouthernHem_mean)
  g <- levelplot(chillHrsSouthernHem_mean, col.regions = col.regionsDefault, margin = FALSE,
                 at = myat, par.settings = mapTheme, 
                 colorkey = list(at = myat),
                 main = list(label = titleText, side = 1,line = 0.5, cex = 1),
                 xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
  
  g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
  
  print(g)
  dev.off()
#  system2('pdfcrop', c(plotFileName, plotFileName))
}


# read the rasters back in to plot optimum temperature range graphs
for (i in modelChoices) {
  modelName.lower <- tolower(i)
  for (k in sspChoices) {
    for (l in startyearChoices) {
      for (m in 1:nrow(ann_crop_temp_table)) {
        cropName <- as.character(ann_crop_temp_table[m, "crop"])
        upperOpt <- as.numeric(ann_crop_temp_table[m, "topt.mean"])
        lowerOpt <- as.numeric(ann_crop_temp_table[m, "topt.lower"])
        yearSpan <- paste0(l, "_", l + yearRange)
        fileNameIn <- paste0("data/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_", modelName.lower, "_", k, "_", yearSpan, ".tif")
        if (l == 2001) {
          fileNameIn <- paste0("data/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_observed_", yearSpan, ".tif")
        }
        
        yearRange <- 9
        yearSpan <- paste0(l, "_", l + yearRange)
        print(paste0(cropName, ", lower optimum: ", lowerOpt, ", upper optimum: ", upperOpt, " start year: ", l))
        print(fileNameIn)
        
        temp <- rast(fileNameIn)
        names(temp) <- month.abb
        titleText <- paste0("Average number of days by month with optimum temp. range (", lowerOpt, " - ", upperOpt, "°C)\n", yearSpan, ", model = ", i, ", SSP = ", k)
        
        if (l == 2001) {
          titleText <- paste0("Average number of days by month with optimum temperature range (", lowerOpt, " - ", upperOpt, "°C) for ", cropName, " ", yearSpan)
        }
        myat <- c(0, 5, 10, 15, 20, 25, 31)
        plotFileName <- paste0("graphics/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_",  modelName.lower, "_", k,  "_", yearSpan, ".jpg")
        if (l == 2001) plotFileName <- paste0("graphics/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_observed_", yearSpan, ".jpg")
        jpeg(plotFileName, width = jpgWidth, height = jpgWidth, quality = 100, units = "in", res = 300)
        temp <- terra::project(temp, crsRob)
        temp <- raster::brick(temp)
        g <- levelplot(temp, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red", "brown" ), margin = FALSE,
                       xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
        g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
        main = list(label = titleText, side = 1,line = 0.5, cex = 1)
        
        #  jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
        print(g)
        dev.off()
#        system2('pdfcrop', c(plotFileName, plotFileName))
      }
    }
  }
}


# script to do chilling hours graphics
source("R/globallyUsed.R")
library(RColorBrewer)
library(colorspace)# use pal <- choose_palette() to see what this is about
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

yearRange <- 9

#test values
k <- "ssp585"
l <- 2091
m = "apple"

fruits <-  c("apple", "apricot", "avocado", "berrynes", "blueberry", 
             "cherry", "cranberry", "currant", "grape", 
             "grapefruitetc", "lemonlime", "orange", "peachetc", "persimmon", "rasberry", "sourcherry", 
             "stonefruitnes", "walnut")

# read the ensemble mean masked rasters back in and create graphic outputs
for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    for (m in fruits) {
      cropName <- m
      yearRange <- 9
     yearSpan <- paste0(l, "_", l + yearRange)
      
      fileNameNH_mean <- paste0("chillHrs_ensembleMean_masked_NorthernHem_", cropName, "_", yearSpan, "_", k, ".tif")
      fileNameSH_mean <- paste0("chillHrs_ensembleMean_masked_SouthernHem_",  cropName, "_", yearSpan, "_", k, ".tif")
      fileNameNH_CV <- paste0("chillHrs_ensembleCV_masked_NorthernHem_", cropName, "_", yearSpan, "_", k, ".tif")
      fileNameSH_CV <- paste0("chillHrs_ensembleCV_masked_SouthernHem_",  cropName, "_", yearSpan, "_", k, ".tif")
      
      chillHrsNorthernHem_mean <- brick( paste0("data/cmip6/chillingHours/", fileNameNH_mean))
      chillHrsSouthernHem_mean <- brick( paste0("data/cmip6/chillingHours/", fileNameSH_mean))
      chillHrsNorthernHem_CV <- brick( paste0("data/cmip6/chillingHours/", fileNameNH_CV))
      chillHrsSouthernHem_CV <- brick( paste0("data/cmip6/chillingHours/", fileNameSH_CV))
      
      col.l <- c("darkslategray1", "blue", "yellow", "red")
      mapTheme <- rasterTheme(region = col.l)  
      mapTheme$panel.background$col = 'gray100' 
      
      # Northern hemisphere
      ## mean
      titleText <- paste0("Northern hem. ensemble mean chilling hours for ", tolower(cropName), ", ", yearSpan, ", SSP = ", k)
     myat <- round(seq.int(from = minValue(chillHrsNorthernHem_mean), to =  maxValue(chillHrsNorthernHem_mean), length.out = 5))
      # g <- levelplot(chillHrsNorthernHem_mean, main = titleText, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red", "brown" ),
      #                xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
       g <- levelplot(chillHrsNorthernHem_mean, main = titleText, col.regions = c("gray70", "yellow", "orange", "red", "brown" ), 
                     at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myat),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
      plotFileName <- paste0("graphics/cmip6/chillingHours/chillHrs_ensembleMean_masked_NorthernHem_", cropName, "_", yearSpan, "_", k, ".jpg")
      #        jpeg(plotFileName, width = 1800, height = 1400, quality = 100)
      jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
      
      ##Coefficient of variation
      titleText <- paste0("Northern hem. ensemble CoV for chilling hours for ", tolower(cropName), ", ", yearSpan, ", SSP = ", k)
 #     myat <- c(0, 40, 80, 120, 160, 230)
      myat <- round(seq.int(from = minValue(chillHrsNorthernHem_CV), to =  maxValue(chillHrsNorthernHem_CV), length.out = 5))
      # g <- levelplot(chillHrsNorthernHem_CV, main = titleText, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red", "brown" ),
      #                xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- levelplot(chillHrsNorthernHem_CV, main = titleText, col.regions = c("gray70", "yellow", "orange", "red", "brown" ), 
                     at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myat),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
      plotFileName <- paste0("graphics/cmip6/chillingHours/chillHrs_ensembleCV_masked_NorthernHem_", cropName, "_", yearSpan, "_", k, ".jpg")
      
      #        jpeg(plotFileName, width = 1800, height = 1400, quality = 100)
      jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
      
      # Southern hemisphere
      ## mean
      titleText <- paste0("Southern hem. ensemble mean chilling hours for ", tolower(cropName), ", ", yearSpan, ", SSP = ", k)
#      myat <- c(0, 1000, 2000, 3000, 4000, 4500)
      myat <- round(seq.int(from = minValue(chillHrsSouthernHem_mean), to =  maxValue(chillHrsSouthernHem_mean), length.out = 5))
      # g <- levelplot(chillHrsSouthernHem_mean, main = titleText, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red", "brown" ),
      #                xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- levelplot(chillHrsSouthernHem_mean, main = titleText, col.regions = c("gray70", "yellow", "orange", "red", "brown" ), 
                     at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myat),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
      plotFileName <- paste0("graphics/cmip6/chillingHours/chillHrs_ensembleMean_masked_SouthernHem_", cropName, "_", yearSpan, "_", k, ".jpg")
      
      #        jpeg(plotFileName, width = 1800, height = 1400, quality = 100)
      jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
      
      ##Coefficient of variation
      titleText <- paste0("Southern hem. ensemble CoV for chilling hours for ", tolower(cropName), ", ", yearSpan, ", SSP = ", k)
#      myat <- c(0, 40, 80, 120, 160, 230)
      myat <- round(seq.int(from = minValue(chillHrsSouthernHem_CV), to =  maxValue(chillHrsSouthernHem_CV), length.out = 5))
      # g <- levelplot(chillHrsSouthernHem_CV, main = titleText, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red", "brown" ),
      #                xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- levelplot(chillHrsSouthernHem_CV, main = titleText, col.regions = c("gray70", "yellow", "orange", "red", "brown" ), 
                     at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myat),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
      plotFileName <- paste0("graphics/cmip6/chillingHours/chillHrs_ensembleCV_masked_SouthernHem_", cropName, "_", yearSpan, "_", k, ".jpg")
      #        jpeg(plotFileName, width = 1800, height = 1400, quality = 100)
      jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
    }
  }
}

# do observed data
yearRange <- 9
yearSpan <- "2001_2010"

for (m in fruits) {
  cropName <- m
  fileNameNH_mean <- paste0("chillHrs_masked_NorthernHem_", cropName, "_observed_", yearSpan, ".tif")
  fileNameSH_mean <- paste0("chillHrs_masked_SouthernHem_",  cropName, "_observed_", yearSpan, ".tif")

  chillHrsNorthernHem_mean <- brick( paste0("data/cmip6/chillingHours/", fileNameNH_mean))
  chillHrsSouthernHem_mean <- brick( paste0("data/cmip6/chillingHours/", fileNameSH_mean))

  # plot ensemble mean
  col.l <- c("darkslategray1", "blue", "yellow", "red")
  mapTheme <- rasterTheme(region = col.l)  
  mapTheme$panel.background$col = 'gray100' 
  
  # Northern hemisphere
  ## mean
  titleText <- paste0("Northern hem. ensemble mean chilling hours for ", tolower(cropName), ", ", yearSpan, ", SSP = ", k)
#  myat <- c(0, 1000, 2000, 3000, 4000, 4500)
  myat <- round(seq.int(from = minValue(chillHrsNorthernHem_mean), to =  maxValue(chillHrsNorthernHem_mean), length.out = 5))
  
  g <- levelplot(chillHrsNorthernHem_mean, main = titleText, col.regions = c("gray70", "yellow", "orange", "red", "brown" ), 
                 at = myat, par.settings = mapTheme, 
                 colorkey = list(at = myat),
                 xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
  
  g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
  plotFileName <- paste0("graphics/cmip6/chillingHours/chillHrs_ensembleMean_masked_NorthernHem_", cropName, "_observed_",  yearSpan, ".jpg")
  #        jpeg(plotFileName, width = 1800, height = 1400, quality = 100)
  jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
  print(g)
  dev.off()
  
  # Southern hemisphere
  ## mean
  titleText <- paste0("Southern hem. ensemble mean chilling hours for ", tolower(cropName), ", ", yearSpan, ", SSP = ", k)
#  myat <- c(0, 1000, 2000, 3000, 4000, 4500)
  myat <- round(seq.int(from = minValue(chillHrsSouthernHem_mean), to =  maxValue(chillHrsSouthernHem_mean), length.out = 5))
  
  g <- levelplot(chillHrsSouthernHem_mean, main = titleText, col.regions = c("gray70", "yellow", "orange", "red", "brown" ), 
                 at = myat, par.settings = mapTheme, 
                 colorkey = list(at = myat),
                 xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
  
  g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
  plotFileName <- paste0("graphics/cmip6/chillingHours/chillHrs_ensembleMean_masked_SouthernHem_", cropName, "_observed_",  yearSpan, ".jpg")
  
  jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
  print(g)
  dev.off()
}

# read the rasters back in to plot optimum temperature range graphs
for (i in modelChoices) {
  for (k in sspChoices) {
    for (l in startyearChoices) {
      for (m in 1:nrow(IPCC_WG2_Ch5_crop_temperature_table)) {
        cropName <- as.character(IPCC_WG2_Ch5_crop_temperature_table[m, "crop"])
        upperOpt <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "topt mean"])
        lowerOpt <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "topt lower"])
        yearSpan <- paste0(l, "_", l + yearRange)
        fileNameIn <- paste0("data/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_", modelName.lower, "_", k, "_", yearSpan, ".tif")
        if (l == 2001) {
          fileNameIn <- paste0("data/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_observed_", yearSpan, ".tif")
        }
        
        yearRange <- 9
        yearSpan <- paste0(l, "_", l + yearRange)
        print(paste0(cropName, ", lower optimum: ", lowerOpt, ", upper optimum: ", upperOpt, " start year: ", l))
        print(fileNameIn)
        
        temp <- brick(fileNameIn)
        names(temp) <- month.abb
        titleText <- paste0("Average number of days by month with optimum temp. range (", lowerOpt, " - ", upperOpt, "°C)\n", yearSpan, ", model = ", i, ", SSP = ", k)
        
        if (l == 2001) {
          titleText <- paste0("Average number of days by month with optimum temperature range (", lowerOpt, " - ", upperOpt, "°C) for ", cropName, " ", yearSpan)
        }
        myat <- c(0, 5, 10, 15, 20, 25, 31)
        g <- levelplot(temp, main = titleText, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red", "brown" ),
                       xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
        g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
        plotFileName <- paste0("graphics/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_",  modelName.lower, "_", k,  "_", yearSpan, ".jpg")
        if (l == 2001) plotFileName <- paste0("graphics/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_observed_", yearSpan, ".jpg")
        
        jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
        print(g)
        dev.off()
      }
    }
  }
}


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

pngHeight = 400
pngWidth = 960
chillRequirements <- as.data.table(read_excel("data-raw/crops/chillRequirements.xlsx", range = "a1:e16"))
col.regionsDefault = c("gray70", "yellow", "orange", "red", "brown")

#test values
k <- "ssp585"
l <- 2091
m = "apple"
#pdf.options(width = 8, height = 8, reset = FALSE)

# attempt to get rid of space around the figure
# lattice.options(
#   layout.heights=list(bottom.padding = list(x=0), top.padding = list(x=0)),
#   layout.widths=list(left.padding = list(x=0), right.padding = list(x=0))
# )

#fruits list is from globallyUsed.R

# read the ensemble mean masked rasters back in and create graphic outputs
for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    for (m in fruits) {
      cropName <- m
      yearRange <- 9
      yearSpan <- paste0(l, "_", l + yearRange)
      chillHoursRange <- chillRequirements[crop %in% cropName, common_varieties]
      minChillHours <- as.numeric(unlist(strsplit(chillHoursRange, split = "-"))[1])
      maxChillHours <- as.numeric(unlist(strsplit(chillHoursRange, split = "-"))[2])
      
      fileNameNH_mean <- paste0("chillHrs_NorthernHem_ensembleMean_masked_", cropName, "_", yearSpan, "_", k, ".tif")
      fileNameSH_mean <- paste0("chillHrs_SouthernHem_ensembleMean_masked_",  cropName, "_", yearSpan, "_", k, ".tif")
      fileNameNH_CV <- paste0("chillHrs_NorthernHem_ensembleCV_masked_", cropName, "_", yearSpan, "_", k, ".tif")
      fileNameSH_CV <- paste0("chillHrs_SouthernHem_ensembleCV_masked_",  cropName, "_", yearSpan, "_", k, ".tif")
      
      chillHrsNorthernHem_mean <- rastpaste0("data/cmip6/chillingHours/", fileNameNH_mean))
      chillHrsNorthernHem_mean <- crop(chillHrsNorthernHem_mean, extent(northerHemExtent))
      
      chillHrsSouthernHem_mean <- rastpaste0("data/cmip6/chillingHours/", fileNameSH_mean))
      chillHrsSouthernHem_mean <- crop(chillHrsSouthernHem_mean, extent(southernHemExtent))
      
      chillHrsNorthernHem_CV <- rast paste0("data/cmip6/chillingHours/", fileNameNH_CV))
      chillHrsNorthernHem_CV <- crop(chillHrsNorthernHem_CV, extent(northerHemExtent))
      
      chillHrsSouthernHem_CV <- rast paste0("data/cmip6/chillingHours/", fileNameSH_CV))
      chillHrsSouthernHem_CV <- crop(chillHrsSouthernHem_CV, extent(southernHemExtent))
      
      maxMean <- max(maxValue(chillHrsSouthernHem_mean), maxValue(chillHrsNorthernHem_mean))
      maxCV <- max(maxValue(chillHrsSouthernHem_CV), maxValue(chillHrsNorthernHem_CV))
      
      col.l <- c("darkslategray1", "blue", "yellow", "red")
      mapTheme <- rasterTheme(region = col.l)  
      mapTheme$panel.background$col = 'gray100' 
      
      # Northern hemisphere
      ## mean
      titleText <- paste0("Ensemble mean, Northern hemisphere winter (Nov to Apr)")
      myat <- round(seq.int(from = 0, to =  maxMean, length.out = 5))
      myat[2] <- minChillHours; myat[3] <- maxChillHours
      plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_NorthernHem_ensembleMean_masked_", cropName, "_", yearSpan, "_", k, ".png")
      png(plotFileName, width = pngWidth, height = pngHeight)
      g <- levelplot(chillHrsNorthernHem_mean, col.regions = col.regionsDefault, margin = FALSE,
                     at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myat),
                     main = list(label = titleText, side = 1,line = 0.5, cex = 1),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
      #      #  jpeg(plotFileName, width = 1800, height = 1400, quality = 100)
      #   #  jpeg(plotFileName, width = 5, height = 5, quality = 100, units = "in", res = 300, par(mar = rep(0, 4), xaxs='i', yaxs='i'))
      print(g)
      dev.off()
#      system2('pdfcrop', c(plotFileName, plotFileName))
      
      ##Coefficient of variation
      titleText <- paste0("CV, Northern hemisphere winter (Nov to Apr)")
      myat <- round(seq.int(from = 0, to =  maxCV, length.out = 5))
      #      myat[2] <- minChillHours; myat[3] <- maxChillHours
      plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_NorthernHem_ensembleCV_masked_", cropName, "_", yearSpan, "_", k, ".png")
      png(plotFileName, width = pngWidth, height = pngHeight)
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
      plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_SouthernHem_ensembleMean_masked_", cropName, "_", yearSpan, "_", k, ".png")
      png(plotFileName, width = pngWidth, height = pngHeight)
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
      
      plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_SouthernHem_ensembleCV_masked_", cropName, "_", yearSpan, "_", k, ".png")
      png(plotFileName, width = pngWidth, height = pngHeight)
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
yearSpan <- "2001_2010"

for (m in fruits) {
  cropName <- m
  chillHoursRange <- chillRequirements[crop %in% cropName, common_varieties]
  minChillHours <- as.numeric(unlist(strsplit(chillHoursRange, split = "-"))[1])
  maxChillHours <- as.numeric(unlist(strsplit(chillHoursRange, split = "-"))[2])
  
  fileNameNH_mean <- paste0("chillHrs_NorthernHem_masked_", cropName, "_observed_", yearSpan, ".tif")
  fileNameSH_mean <- paste0("chillHrs_SouthernHem_masked_",  cropName, "_observed_", yearSpan, ".tif")
  
  chillHrsNorthernHem_mean <- rast paste0("data/cmip6/chillingHours/", fileNameNH_mean))
  chillHrsSouthernHem_mean <- rast paste0("data/cmip6/chillingHours/", fileNameSH_mean))
  
  chillHrsNorthernHem_mean <- crop(chillHrsNorthernHem_mean, extent(northerHemExtent))
  chillHrsSouthernHem_mean <- crop(chillHrsSouthernHem_mean, extent(southernHemExtent))
  maxMean <- max(maxValue(chillHrsSouthernHem_mean), maxValue(chillHrsNorthernHem_mean))
  
  # plot  mean
  col.l <- c("darkslategray1", "blue", "yellow", "red")
  mapTheme <- rasterTheme(region = col.l)  
  mapTheme$panel.background$col = 'gray100' 
  
  # Northern hemisphere
  ## mean
  titleText <- paste0("Northern hemisphere winter (Nov to Feb)")
  myat <- round(seq.int(from = 0, to =  maxMean, length.out = 5))
  myat[2] <- minChillHours; myat[3] <- maxChillHours
  plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_NorthernHem_masked_", cropName, "_observed_",  yearSpan, ".png")
  png(plotFileName, width = pngWidth, height = pngHeight)
  
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
  
  plotFileName <- paste0(getwd(), "/graphics/cmip6/chillingHours/", "chillHrs_SouthernHem_masked_", cropName, "_observed_",  yearSpan, ".png")
  png(plotFileName, width = pngWidth, height = pngHeight)
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
  for (k in sspChoices) {
    for (l in startyearChoices) {
      for (m in 1:nrow(ann_crop_temp_table)) {
        cropName <- as.character(ann_crop_temp_table[m, "crop"])
        upperOpt <- as.numeric(ann_crop_temp_table[m, "topt mean"])
        lowerOpt <- as.numeric(ann_crop_temp_table[m, "topt lower"])
        yearSpan <- paste0(l, "_", l + yearRange)
        fileNameIn <- paste0("data/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_", modelName.lower, "_", k, "_", yearSpan, ".tif")
        if (l == 2001) {
          fileNameIn <- paste0("data/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_observed_", yearSpan, ".tif")
        }
        
        yearRange <- 9
        yearSpan <- paste0(l, "_", l + yearRange)
        print(paste0(cropName, ", lower optimum: ", lowerOpt, ", upper optimum: ", upperOpt, " start year: ", l))
        print(fileNameIn)
        
        temp <- rastfileNameIn)
        names(temp) <- month.abb
        titleText <- paste0("Average number of days by month with optimum temp. range (", lowerOpt, " - ", upperOpt, "°C)\n", yearSpan, ", model = ", i, ", SSP = ", k)
        
        if (l == 2001) {
          titleText <- paste0("Average number of days by month with optimum temperature range (", lowerOpt, " - ", upperOpt, "°C) for ", cropName, " ", yearSpan)
        }
        myat <- c(0, 5, 10, 15, 20, 25, 31)
        plotFileName <- paste0("graphics/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_",  modelName.lower, "_", k,  "_", yearSpan, ".png")
        if (l == 2001) plotFileName <- paste0("graphics/cmip6/optTempRange/optTempRange_", cropName, "_", lowerOpt, "_", upperOpt, "_observed_", yearSpan, ".png")
        png(plotFileName, width = pngWidth, height = pngHeight)
        
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


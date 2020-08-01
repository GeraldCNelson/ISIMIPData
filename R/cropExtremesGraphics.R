# script to do crop extreme graphics
source("R/globallyUsed.R")
library(RColorBrewer)
library(colorspace)# use pal <- choose_palette() to see what this is about
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9

#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2021

# commented out, now in the globallyUsed.R script
#ann_crop_temp_table <- as.data.table(read_excel("data-raw/crops/ann_crop_temp_table_summary_02052020.xlsx", range = "A1:S26"))
#setnames(ann_crop_temp_table, old = names(ann_crop_temp_table), new = make.names(names(ann_crop_temp_table)))

# read the tdamage rasters back in and create jpegs
for (i in modelChoices) {
  for (k in sspChoices) {
    for (l in startyearChoices) {
      for (m in 1:nrow(ann_crop_temp_table)) {
        cropName <- as.character(ann_crop_temp_table[m, "crop"])
        tdamage_mean <- as.numeric(ann_crop_temp_table[m, "tdamage mean"])
        yearRange <- 9
        print(paste0(cropName, ", damage temp: ", tdamage_mean, " start year: ", l))
        modelName.lower <- tolower(i)
        yearSpan <- paste0(l, "_", l + yearRange)
        
        fileNameIn_damage <- paste0("data/cmip6/damageTemp/tdamage_mean_", cropName, "_", tdamage_mean, "C_",  modelName.lower, "_", k,  "_", yearSpan, ".tif")
        if (l == 2001) {
          fileNameIn_damage <- paste0("data/cmip6/damageTemp/tdamage_mean_", cropName, "_", tdamage_mean, "C",  "_observed_", yearSpan, ".tif")
        }
        
        print(fileNameIn_damage)
        temp <- rastfileNameIn_damage)
        temp <- readAll(temp)
        names(temp) <- month.abb
        yearSpan <- paste0(l, "_", l + yearRange)
        
        titleText <- paste0("Days with temps above damage level for ", tolower(cropName), " (", tdamage_mean, "°C)\n ", yearSpan, ", model = ", i, ", SSP = ", k)
        if (l == 2001) {
          titleText <- paste0("Days with temps above damage level for ", tolower(cropName), " (", tdamage_mean, "°C)\n ", yearSpan)
        }  
        myat <- c(0, 5, 10, 15, 20, 25, 31)
        g <- levelplot(temp, main = titleText, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red", "brown" ),
                       xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
        g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5)) # layout.heights = list(top.padding = 0, bottom.padding = 0))
        plotFileName <- paste0("graphics/cmip6/damageTemp/tdamage_mean_", cropName, "_", tdamage_mean, "C ", modelName.lower, "_", k,  "_", yearSpan, ".jpg")
        if (l == 2001) plotFileName <- paste0("graphics/cmip6/damageTemp/tdamage_mean_", cropName, "_", tdamage_mean, "C", "_observed_", yearSpan, ".jpg")
        
        #        jpeg(plotFileName, width = 1800, height = 1400, quality = 100)
        jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
        print(g)
        dev.off()
      }
    }
  }
}

# read the rasters back in to plot optimum temperature range graphs
for (i in modelChoices) {
  for (k in sspChoices) {
    for (l in startyearChoices) {
      for (m in 1:nrow(ann_crop_temp_table)) {
        cropName <- as.character(ann_crop_temp_table[m, "crop"])
        modelName.lower <- tolower(i)
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

# ensemble graphics
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data
for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l))
    for (m in 1:nrow(ann_crop_temp_table)) {
      cropName <- as.character(ann_crop_temp_table[m, "crop"])
      tdamage_mean <- as.numeric(ann_crop_temp_table[m, "tdamage mean"])
      yearRange <- 9
      yearSpan <- paste0(l, "_", l + yearRange)
      print(paste0(cropName, ", damage temp: ", tdamage_mean, ", start year: ", l))
      
      fileNameMean <- paste0("data/cmip6/damageTemp/tdamage_ensembleMean_masked_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      fileNameCV <- paste0("data/cmip6/damageTemp/tdamage_ensembleCV_masked_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".tif")
      temp.mean <- rastfileNameMean)
      names(temp.mean) <- month.abb
      temp.cv <- rastfileNameCV)
      names(temp.cv) <- month.abb
      
      # plot Ensemble mean
      titleText <- paste0("Average number of days by month, temps above damage level, ", tolower(cropName), " (", tdamage_mean, "°C)\n ", yearSpan, ", SSP = ", k, ", ensemble results")
      myat <- c(0, 5, 10, 15, 20, 25, 31)
      g <- levelplot(temp.mean, main = titleText, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red", "brown" ),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
      plotFileName <- paste0("graphics/cmip6/damageTemp/tdamage_ensembleMean_masked_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".jpg")
      jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
      
      # plot Ensemble CV
      titleText <- paste0("CV, number of days by month, temps above damage level, ", tolower(cropName), " (", tdamage_mean, "°C)\n ", yearSpan, ", SSP = ", k, ", ensemble results")
      myat <- c(0, 1, 2, 3, 4, 5, 15)
      g <- levelplot(temp.cv, main = titleText, at = myat, col.regions = c("white", "blue", "yellow", "orange", "red", "brown" ),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
      plotFileName <- paste0("graphics/cmip6/damageTemp/tdamage_ensembleCV_masked_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".jpg")
      jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
    }
  }
  # do observed data
  for (m in 1:nrow(ann_crop_temp_table)) {
    cropName <- as.character(ann_crop_temp_table[m, "crop"])
    tdamage_mean <- as.numeric(ann_crop_temp_table[m, "tdamage mean"])
    yearRange <- 9
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0(cropName, ", damage temp: ", tdamage_mean, ", start year: ", "2001"))
    
    fileNameMean <- paste0("data/cmip6/damageTemp/tdamage_mean_masked_", cropName, "_", tdamage_mean, "C", "_observed_", yearSpan, ".tif")
    print(paste0("filenamein ", fileNameMean))
    temp.mean <- rast(fileNameMean)
    names(temp.mean) <- month.abb
    
    # plot Ensemble mean
    titleText <- paste0("Average number of days by month, temps above damage level, ", tolower(cropName), " (", tdamage_mean, "°C)\n ", yearSpan,  " ensemble results")
    myat <- c(0, 5, 10, 15, 20, 25, 31)
    g <- levelplot(temp.mean, main = titleText, at = myat, col.regions = c("aliceblue", "blue", "yellow", "orange", "red", "brown" ),
                   xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
    g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
    plotFileName <- paste0("graphics/cmip6/damageTemp/tdamage_mean_masked_", cropName, "_", tdamage_mean, "C", "_observed_", yearSpan, ".jpg")
    jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
    print(g)
    dev.off()
  }
}

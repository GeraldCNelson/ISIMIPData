# script to do THI graphics
source("R/globallyUsed.R")
library(RColorBrewer)
library(colorspace)# use pal <- choose_palette() to see what this is about
sspChoices <- c("ssp585") #"ssp126", 
#modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#  Calculate the number of growing degrees per day 
source("R/globallyUsed.R")
library(doParallel) #Foreach Parallel Adaptor 
# library(foreach) #Provides foreach looping construct, called with doParallel

locOfFiles <- locOfCMIP6ncFiles
sspChoices <- c("ssp585") #"ssp126", 

startyearChoices <-  c(2021, 2051, 2091) #, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
hemisphereList <- c("Northern", "Southern")
northerHemExtent <- c( -180, 180, 0, 90)
southernHemExtent <-  c( -180, 180, -90, 0)

yearRange <- 9
tmaxList <- sort(unique(ann_crop_temp_table$tdamage.mean)) #get all the damaging temperature levels for the annual crops

#test values
i <- "GFDL-ESM4"
k <- "ssp585"
l <- 2051

startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

for (k in sspChoices)  {
  for (l in startyearChoices_ensemble) {
    for (o in 1:length(cropChoices)) {
      for (m in get(cropChoices[o])) {
        print(paste0("crop: ", m))  
        yearSpan <- paste0(l, "_", l + yearRange)
        tmaxLimit <- ann_crop_temp_table[crop %in% m, tdamage.mean]
        j <- tmaxLimit
        fileNameMean <- paste0("data/cmip6/tmaxMonthlySums/tmaxMonthlySums_ensembleMean_", m, "_", j,  "_", k, "_",  yearSpan, ".tif") 
        fileNameCV <- paste0("data/cmip6/tmaxMonthlySums/tmaxMonthlySums_ensembleCV_", m, "_", j,  "_", k, "_",  yearSpan, ".tif") 
        
        meanData <- rastfileNameMean)
        CVData <- rastfileNameCV)
        names(meanData) <- month.abb
        names(CVData) <- month.abb
        
        # plot Ensemble mean
        titleText <- paste0("Ave. number of days with temp above lethal for ",tolower(m), " (", j, "°C)", "\n ", gsub("_", "-", yearSpan), ", SSP = ", k, ", ensemble mean")
        col.l <- c("darkslategray1", "blue", "yellow", "red")
        mapTheme <- rasterTheme(region = col.l)  
        mapTheme$panel.background$col = 'white' 
        myat <- c(0, 8, 16, 24, 31)
        g <- levelplot(meanData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                       colorkey = list(at = myat, col = col.l),
                       xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
        
        g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
        plotFileName <- paste0("graphics/cmip6/tmaxMonthlySums/tmax_ensembleMean_masked_", m, "_",  j, "_",  yearSpan, "_", k, ".jpg")
        print(paste0("plot file name: ", plotFileName, " for crop ", m))
        jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
        print(g)
        dev.off()
         maxMean <- max(cellStats(meanData, stat='max'), na.rm = TRUE)
        maxCV <- max(cellStats(CVData, stat='max'), na.rm = TRUE)

        # plot Ensemble CV
        titleText <- paste0("Lethal temp days CV for ",tolower(m), " (", j, "°C)", "\n ", gsub("_", "-", yearSpan), ", SSP = ", k, ", ensemble CV")
        myat <- round(seq.int(from = 0, to =  maxCV, length.out = 5))
        g <- levelplot(CVData, main = titleText, col.regions = col.l, at = myat,
                       colorkey = list(at = myat, col = col.l),
                       xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
        
        g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
        plotFileName <- paste0("graphics/cmip6/tmaxMonthlySums/tmax_ensembleCV_masked_", tolower(m), "_", j, "_",  yearSpan, "_", k, ".jpg")
        jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
        print(g)
        dev.off()
      }
    }
  }
}

# do observed data
yearRange <- 9
yearSpan <- paste0(l, "_", l + yearRange)

for (o in 1:length(cropChoices)) {
  for (m in get(cropChoices[o])) {
    print(paste0("crop: ", m))  
    tmaxLimit <- ann_crop_temp_table[crop %in%tolower(m), tdamage.mean]
    j <- tmaxLimit
    
    fileNameMean <- paste0("data/cmip6/tmaxMonthlySums/tmax_ensembleMean_masked",m, "_", j, "_observed_",  yearSpan, ".tif") 
    
    print(paste0("filenamein ", fileNameMean))
    meanData <- rast(fileNameMean)
    names(meanData) <- month.abb
    
    # plot Ensemble mean
    titleText <- paste0("THI stress levels by month, ", speciesName, "\n ", yearSpan)
    titleText <- paste0("Ave. number of days with temp above lethal for ",tolower(m), " (", j, "°C)", "\n ", gsub("_", "-", yearSpan), ", ", "ensemble mean")
    
    zeroLevel <- bpList[species %in% speciesName, zeroLevel]
    noStress <- bpList[species %in% speciesName, noStress]
    moderateStress <- bpList[species %in% speciesName, moderateStress]
    extremeStress <- bpList[species %in% speciesName, extremeStress]
    col.l <- c("darkslategray1", "blue", "yellow", "red")
    mapTheme <- rasterTheme(region = col.l)  
    mapTheme$panel.background$col = 'white' 
    
    myat <- c(zeroLevel, noStress, moderateStress, extremeStress, 100)
    g <- levelplot(meanData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                   colorkey = list(at = myat, col = col.l, labels = c( "","No stress", "moderate stress", "extreme stress", "maximum")),
                   xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
    
    g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
    plotFileName <- paste0("graphics/cmip6/tmaxMonthlySums/tmaxMonthlySums_ensembleMean_",tolower(m), "_", j, "_observed_",  yearSpan, ".jpg")
    print(paste0("plot file name: ", plotFileName, " for crop ", m))
    jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
    print(g)
    dev.off()
  }
}
  
  # old code
  # for (i in modelChoices) {
  #   for (k in sspChoices) {
  #     for (l in startyearChoices) {
  #       
  #       meanFileName <- paste0("results/modMean", "_", k, "_", l, "_",j, ".tif")
  #       raster.mean <-          stack(meanFileName)
  #       #      raster.extremeStress <- stack(paste0("results/modMean", "_", k, "_", l, "_",j, ".tif"))
  #       rasterNameMean <- paste0("modMean", "_", k, "_", l, "_",j)
  #       assign(rasterNameMean, raster.mean)
  #     }
  #   }
  # }
  # 
  # # Do observed. Note that these are not actually model means, just the means over the observed period. There is no model-based CV for this period.
  # for (k in thiList) {
  #   raster.mean <- stack(paste0("results/", k, "_observed", ".tif"))
  #   rasterNameMean <- paste0("modMean", "_", k, "_observed")
  #   names(raster.mean) <- month.abb
  #   assign(rasterNameMean, raster.mean)
  # }
  # 
  # #read in the animal numbers data
  # sourceDir <- "data-raw/animals/arcrasters/"
  # animalsList <- list.files(sourceDir)
  # # load the data for the number of animals in each 1/2 degree cell
  # for (i in animalsList) {
  #   species <- unlist(strsplit(i, "-"))[2]
  #   if (species %in% "recl.asc") species = "livestockSystem"
  #   fileName <- paste0("data/raster_", species, ".tif")
  #   print(fileName)
  #   rIn <- rast(fileName)
  #   rName <- paste0("raster_", species)
  #   assign(rName, rIn)
  # }
  # 
  # # plot the THI extremes
  # 
  # library(RColorBrewer)
  # library(rworldmap)
  # library(colorspace)# use pal <- choose_palette() to see what this is about
  # 
  # 
  # reds = brewer.pal(5, "YlOrRd")
  # greens = brewer.pal(3, "Greens")
  # blues = brewer.pal(5, "Blues")
  # mapTheme <- rasterTheme(region = c('white', blues, greens, reds, "gray"))
  # for (l in comboChoices) {
  #   for (j in sspChoices) {
  #     for (k in thiList) {
  #       i <- "modMean"
  #       speciesName <- gsub("thi.", "", k)
  #       rasterToPlot <- paste0(i, "_", k, "_", l, "_",j)
  #       if (l %in% "observed") rasterToPlot <-  paste0("results/modMean_modMean", "_", k, "_observed", ".tif")
  # 
  #       titleText <- paste0("Model Mean THI, ", speciesName, ", ", l, ", ", j)
  #       if (l %in% "observed") titleText <- paste0("Model Mean THI, ", speciesName, ", monthly average, ", l)
  #       print(titleText)
  #       #       print(levelplot(x = get(thiList[m]), main = titleText, par.settings = mapTheme))
  #       if (l %in% "observed") {
  #         bpExtreme <- bpList[species %in% speciesName & model %in% i & period %in% l & is.na(rcp), extremeStress]
  #       }else{
  #         bpExtreme <- bpList[species %in% speciesName & model %in% i & period %in% l & rcp %in% j, extremeStress]
  #       }
  #       print(paste0("bpExtreme: ", bpExtreme))
  #       #        g <- levelplot(x = get(thiList[m]), main = titleText,  at = get(bpExtreme), col.regions = c("white", "green", "yellow", "red"))
  #       # thiname <- paste(k, i, l, j, sep = "_")
  #       thiname <- paste(k, i, l, j, sep = "_")
  #       if (i %in% "modMean") {
  #         thiname <- paste("modMean",  k, l, j, sep = "_")
  #         thiCV <- paste("modCV",  k, l, j, sep = "_")
  #       }
  #       if (l %in% "observed") { 
  #         thiname <- paste(k, l, sep = "_")
  #         if (i %in% "modMean") thiname <- paste("modMean", k, l, sep = "_")
  #       }
  #       myat <- c(0, bpExtreme, 100)
  #       myat_CV <- c(0, 1, 2, 3, 4, 7)
  #       g <- levelplot(x = get(thiname), main = titleText,  at = myat, col.regions = c("white", "red"))
  #       g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
  #       plotFileName <- paste0("graphics/cmip6/THI/lp_", i, "_", l, "_", j, "_", gsub(".", "_", k, fixed = TRUE), ".jpg")
  #       if (l %in% "observed") plotFileName <- paste0("graphics/cmip6/THI/lp_", i, "_", l, "_", gsub(".", "_", k, fixed = TRUE), ".jpg")
  #       print(paste0("plotFileName: ", plotFileName))
  #       jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
  #       print(g)
  #       dev.off()
  #       
  #     }
  #   }
  # }
  
  

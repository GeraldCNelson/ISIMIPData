# calculate 1-0 values for locations where chill portions are enough, by fruit
library(terra)
library(sf)
library("crayon")
library(ggplot2)
fileLoc <- "data/cmip6/chillPortions/chill_portions/"
speciesChoice <- c("cherry", "almond", "winegrape", "apple") #, "olive", "berries") 
fruitCPs <- readxl::read_excel("data-raw/crops/fruitCPs.xlsx")
sspChoices <- c("ssp126", "ssp585") 
modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
modelChoices_lower <- tolower(modelChoices)
startyearChoices <-  c(2041, 2081) 
hemispheres <- c("north", "south")
yearRange <- 19
woptList <- list(gdal=c("COMPRESS=LZW"))

coastline <- st_read("data-raw/regionInformation/ne_50m_coastline/ne_50m_coastline.shp")

#function to get rid of Antarctica
crop_custom <- function(poly.sf) {
  poly.sp <- as(poly.sf, "Spatial")
  extR <- raster::extent(c(-180, 180, -60, 90))
  poly.sp.crop <- crop(poly.sp, extR)
  st_as_sf(poly.sp.crop)
}
coastline <- crop_custom(coastline)

RobinsonProj <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
crsRob <- RobinsonProj
coastline <- st_transform(coastline, crsRob)

readRast_ensemble <- function(i) {
  modelName.lower <- tolower(i)
  fileNameIn <- paste0("data/cmip6/chillPortions/chill_portions/", k,"/", modelName.lower, "/", k, "_", modelName.lower, "_", midYear, "_chill_portions_", m, ".tif")
  print(fileNameIn)
  r <- rast(fileNameIn)
  system.time(chillPortion <- app(r, fun = quantile, probs=0.1, na.rm = TRUE))
  chillPortion
}

#test values
i <- "UKESM1-0-LL"
k <- "ssp585"
l <- 2041
m <- "north"
n <- "cherry"

for (k in sspChoices) {
  #  k = "ssp585"
  for (l in startyearChoices) {
    # l <- 2041
    midYear <- l + 9
    yearSpan <- paste0(l, "_", l + yearRange)
    for (m in hemispheres) {
      
      system.time(x <- lapply(modelChoices_lower, readRast_ensemble))
      SWC <- rast(x)
      indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
      
      # now do ensembles
      
      for (n in speciesChoice) {
        print(paste0("working on ssp: ", k, ", start year ", l, ", hemisphere ", m, ", crop ", n))
        cplimit <- fruitCPs$chillRequirement[fruitCPs$crop %in% n]
        r <- SWC
        
        maxVal <- round(max(minmax(r)), 2)
        minVal <- round(min(minmax(r)), 2)
        #     print(r)
        fileName_out <- paste0(fileLoc, "ensemble_chill_cutoff_", n, "_", k, "_", m, "_", yearSpan, ".tif")
        #       print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
        print(system.time(r.mean <- app(r, fun = "mean", na.rm = TRUE)))
        r.mean[r.mean < cplimit] <- 0
        r.mean[r.mean > cplimit] <- 1
        print(system.time(writeRaster(r.mean, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
        cat(paste0(red("species: ", n, ", ensemble ssp: ", k, ", start year: ", l, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName out: ", fileName_out), "\n\n"))
        #   names(r.mean) <- gsub("X", "Day ", names(r.mean))
      }
    }
  }
}

# historical -----
k <- "historical"
l = 1991
midYear <- l + 9
yearSpan <- paste0(l, "_", l + yearRange)
for (m in hemispheres) {
  
  system.time(x <- lapply(modelChoices_lower, readRast_ensemble))
  SWC <- rast(x)
  indices_day <- rep(seq(1, nlyr(x[[1]]), 1), 5) # 5 is number of models; if omitted should get the same result
  
  # now do ensembles
  
  for (n in speciesChoice) {
    print(paste0("working on ssp: ", k, ", start year ", l, ", hemisphere ", m, ", crop ", n))
    cplimit <- fruitCPs$chillRequirement[fruitCPs$crop %in% n]
    r <- SWC
    
    maxVal <- round(max(minmax(r)), 2)
    minVal <- round(min(minmax(r)), 2)
    #     print(r)
    fileName_out <- paste0(fileLoc, "ensemble_chill_cutoff_", n, "_", k, "_", m, "_", yearSpan, ".tif")
    #       print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
    print(system.time(r.mean <- app(r, fun = "mean", na.rm = TRUE)))
    r.mean[r.mean < cplimit] <- 0
    r.mean[r.mean > cplimit] <- 1
    print(system.time(writeRaster(r.mean, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
    cat(paste0(red("species: ", n, ", ensemble ssp: ", k, ", start year: ", l, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName out: ", fileName_out), "\n\n"))
    #   names(r.mean) <- gsub("X", "Day ", names(r.mean))
  }
}

# graphics -----
ext_noAntarctica <- ext(-180, 180, -60, 90)

for (k in sspChoices) {
  #  k = "ssp585"
  for (l in startyearChoices) {
    # l <- 2041
    midYear <- l + 9
    yearSpan <- paste0(l, "_", l + yearRange)
    for (n in speciesChoice) {
      fileName_in_north <- paste0(fileLoc, "ensemble_chill_cutoff_", n, "_", k, "_", "north", "_", yearSpan, ".tif")
      fileName_in_south <- paste0(fileLoc, "ensemble_chill_cutoff_", n, "_", k, "_", "south", "_", yearSpan, ".tif")
      r_north <- rast(fileName_in_north)
      r_south <- rast(fileName_in_south)
      r <- merge(r_north, r_south)
      r <- crop(r, ext_noAntarctica)
      r <- project(r, crsRob)
      
      r_df <- as.data.frame(r, xy = TRUE)
      names(r_df) <- c("x", "y", "value")
      titleText <- paste0("species: ", n, ", scenario:  ", k,  ", year span: ", gsub("_", "-", yearSpan))
      legendTitle <- "Adequate chill portions"
      #        colorList <- (RColorBrewer::brewer.pal(2, "YlOrRd"))
      colorList <- c("white", "green")
      #    custom_bins = c(0, 1)
      g <- ggplot(data = coastline) +
        labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
        labs(x = "", y = "") +
        geom_raster(data = r_df, aes(x, y, fill = value), show.legend = FALSE) +
        scale_fill_gradientn(colours=c("white","green")) +
        geom_sf(color = "ghostwhite", lwd = 0.2) +
        theme(panel.background = element_rect(fill = "aliceblue"))
      #          theme(legend.text.align = 1) +
      #   theme(legend.position = "none")
      outFilename <- paste0("graphics/cmip6/chillPortions/adeqChillPortions_", n, "_", k, "_", yearSpan, ".png")
      
      ggsave(filename = outFilename, plot = g, width = 6, height = 6, units = "in", dpi = 300)
      print(paste0("out file name: ", outFilename))
      g <- NULL
      #     print(g)
      #      dev.off()
    }
  }
}


k = "historical"
l <- 1991
midYear <- l + 9
yearSpan <- paste0(l, "_", l + yearRange)
for (n in speciesChoice) {
  fileName_in_north <- paste0(fileLoc, "ensemble_chill_cutoff_", n, "_", k, "_", "north", "_", yearSpan, ".tif")
  fileName_in_south <- paste0(fileLoc, "ensemble_chill_cutoff_", n, "_", k, "_", "south", "_", yearSpan, ".tif")
  r_north <- rast(fileName_in_north)
  r_south <- rast(fileName_in_south)
  r <- merge(r_north, r_south)
  r <- crop(r, ext_noAntarctica)
  r <- project(r, crsRob)
  
  r_df <- as.data.frame(r, xy = TRUE)
  names(r_df) <- c("x", "y", "value")
  titleText <- paste0("species: ", n, ", scenario:  ", k,  ", year span: ", gsub("_", "-", yearSpan))
  legendTitle <- "Adequate chill portions"
  #        colorList <- (RColorBrewer::brewer.pal(2, "YlOrRd"))
  colorList <- c("white", "green")
  #    custom_bins = c(0, 1)
  g <- ggplot(data = coastline) +
    labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
    labs(x = "", y = "") +
    geom_raster(data = r_df, aes(x, y, fill = value), show.legend = FALSE) +
    scale_fill_gradientn(colours=c("white","green")) +
    geom_sf(color = "ghostwhite", lwd = 0.2) +
    theme(panel.background = element_rect(fill = "aliceblue"))
  #          theme(legend.text.align = 1) +
  #   theme(legend.position = "none")
  outFilename <- paste0("graphics/cmip6/chillPortions/adeqChillPortions_", n, "_", k, "_", yearSpan, ".png")
  
  ggsave(filename = outFilename, plot = g, width = 6, height = 6, units = "in", dpi = 300)
  print(paste0("out file name: ", outFilename))
  g <- NULL
  #     print(g)
  #      dev.off()
}

# get list of files with their sizes
# filesIn <- list.files(fileLoc, recursive = TRUE, full.names = TRUE)
# for (i in filesIn){
#   info <- file.info(filename = i)
#   print(paste0("name: ", i, ", size ", info$size))
# }

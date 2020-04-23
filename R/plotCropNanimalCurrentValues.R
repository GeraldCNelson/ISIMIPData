# produce maps of where animals and plants are currently located.
# script to do THI graphics
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

# do plants
# now do plants
getcropAreaYield <- function(cropName, dataType) {
  tifZipUrl <-  " https://s3.us-east-2.amazonaws.com/earthstatdata/HarvestedAreaYield175Crops_Geotiff.zip"
  tifzipFile <- paste0("data-raw/crops/HarvestedAreaYield175Crops_Geotiff.zip")
  tiffilecrop <- cropName
  if (dataType %in% "area") {
    tifcropFile <- paste0(tiffilecrop, "_HarvestedAreaHectares.tif")
  } else {tifcropFile <- paste0(tiffilecrop, "_YieldPerHectare.tif")
  }
  tifzipFilePath <- paste0("HarvestedAreaYield175Crops_Geotiff/GeoTiff/", tiffilecrop, "/", tifcropFile)
  tifOut <- unzip(zipfile = tifzipFile, files = tifzipFilePath)
  return(tifOut)
}

crops <- c("abaca", "agave", "alfalfa", "almond", "aniseetc", "apple", "apricot", 
           "areca", "artichoke", "asparagus", "avocado", "bambara", "banana", 
           "barley", "bean", "beetfor", "berrynes", "blueberry", "brazil", "broadbean", 
           "buckwheat", "cabbage", "cabbagefor", "canaryseed", "carob", "carrot", 
           "carrotfor", "cashew", "cashewapple", "cassava", "castor", "cauliflower", "cerealnes", 
           "cherry", "chestnut", "chickpea", "chicory", "chilleetc", "cinnamon", "citrusnes", 
           "clove", "clover", "cocoa", "coconut", "coffee", "cotton", "cowpea", "cranberry", 
           "cucumberetc", "currant", "date", "eggplant", "fibrenes", "fig", "flax", "fonio", 
           "fornes", "fruitnes", "garlic", "ginger", "gooseberry", "grape", 
           "grapefruitetc", "grassnes", "greenbean", "greenbroadbean", "greencorn", 
           "greenonion", "greenpea", "groundnut", "hazelnut", "hemp", "hempseed", "hop", 
           "jute", "jutelikefiber", "kapokfiber", "kapokseed", "karite", "kiwi", "kolanut", 
           "legumenes", "lemonlime", "lentil", "lettuce", "linseed", "lupin", "maize", "maizefor", 
           "mango", "mate", "melonetc", "melonseed", "millet", "mixedgrain", "mixedgrass", 
           "mushroom", "mustard", "nutmeg", "nutnes", "oats", "oilpalm", "oilseedfor", "oilseednes", 
           "okra", "olive", "onion", "orange", "papaya", "pea", "peachetc", "pear", "pepper", 
           "peppermint", "persimmon", "pigeonpea", "pimento", "pineapple", "pistachio", 
           "plantain", "plum", "poppy", "potato", "pulsenes", "pumpkinetc", "pyrethrum", 
           "quince", "quinoa", "ramie", "rapeseed", "rasberry", "rice", "rootnes", "rubber", 
           "rye", "ryefor", "safflower", "sesame", "sisal", "sorghum", "sorghumfor", "sourcherry", 
           "soybean", "spicenes", "spinach", "stonefruitnes", "strawberry", "stringbean", 
           "sugarbeet", "sugarcane", "sugarnes", "sunflower", "swedefor", "sweetpotato", 
           "tangetc", "taro", "tea", "tobacco", "tomato", "triticale", "tropicalnes", "tung", 
           "turnipfor", "vanilla", "vegetablenes", "vegfor", "vetch", "walnut", "watermelon", 
           "wheat", "yam", "yautia")

for (i in crops) {
  #  i <- "wheat"
  tempTifArea <- getcropAreaYield(i, "area")
  
  rInArea <- raster(tempTifArea)
  # rInYield <- raster(fileInYield)
  
  # Earthstat data (using its harvest area) has a pixel size of  5 minute resolution. Need to convert to 1/2 degree to get to cmip6 cell size
  # 5 min = 0.0833333 degree
  # 30 min = 0.5 degree
  
  rInAreaAgg <- aggregate(rInArea, fact = 6, fun = "sum")
  cutoff <- 1000 # only include 1/2 cells where crop area is great than cutoff
  rInAreaAgg[rInAreaAgg < cutoff] <- 0
  
  # # change projection from longlat to something else
  # library(rgdal)
  # # projInfo(type = "proj") shows the list of projections
  # crs_old <- crs(rInAreaAgg)
  # crs_new <- gsub("longlat", "webmerc", crs_old)
  # crs_new <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  # rInAreaAgg.p <- projectRaster(rInAreaAgg, crs = crs_new)
  titleText <- paste0("Current crop area , ", i, " (000 ha)")
  rInAreaAgg_df <- as.data.frame(rInAreaAgg, xy = TRUE)
  colorRange <- colorRampPalette(c("white", "red", "#7F0000"))
  ggplot(data = rInAreaAgg_df, mapping = aes(x = x, y = y, fill = wheat_HarvestedAreaHectares)) +
    geom_raster() + 
    scale_fill_gradientn(colors = colorRange(7), limits = c(0.0, 240427)) + 
    borders() + 
#    scale_fill_viridis_c(limits = c(0.0, 240427)) +
    coord_quickmap()
}

# for animals
sourceDir <- "data-raw/animals/arcrasters/"
animalsList <- list.files(sourceDir)
for (i in animalsList) {
  speciesName <- unlist(strsplit(i, "-"))[2]
  if (speciesName %in% "recl.asc") speciesName = "livestockSystem"
  fileName <- paste0("data/animals/raster_", speciesName, ".tif")
  rIn <- raster(fileName)
  if (speciesName %in% "chicken") 
  {cutoff <- 100000
  }else{cutoff <- 1000}
  rIn[rIn < cutoff] <- 0
  
  titleText <- paste0("Current livestock numbers , ", i, " (000)")
  rIn_df <- as.data.frame(rIn, xy = TRUE)
  colorRange <- colorRampPalette(c("white", "red", "#7F0000"))
  ggplot(data = rIn_df, mapping = aes(x = x, y = y, fill = raster_cattle)) +
    geom_raster() + 
    scale_fill_gradientn(colors = colorRange(7), limits = c(0.0, 1492071)) + 
    borders() + 
    #    scale_fill_viridis_c(limits = c(0.0, 240427)) +
    coord_quickmap()
}
  
  
  fileNameout <- paste0("data/animals/rasterMask_", speciesName, ".tif")
  print(fileNameout)
  writeRaster(rIn, fileNameout, format = "GTiff", overwrite = TRUE)
}
 

# for potential use later
#   g <- plot(rInAreaAgg, main = titleText, #col.regions = col.l, at = myat, par.settings = mapTheme, 
# #                 colorkey = list(at = myat, col = col.l, labels = c( "","No stress", "moderate stress", "extreme stress", "maximum")),
#                  xlab = "", ylab = "") # , scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
#   
#   g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
#   plotFileName <- paste0("graphics/cmip6/THI/THI_ensembleMean_masked_",  speciesName, "_",  yearSpan, "_", k, ".jpg")
#   print(paste0("plot file name: ", plotFileName, " for species ", speciesName))
#   jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
#   print(g)
#   dev.off()
#   
#   # plot Ensemble SD
#   titleText <- paste0("THI SD by month, ", speciesName, "\n ", yearSpan, ", SSP = ", k, ", ensemble SD")
#   myat <- c(0, .5, 1.0, 1.5, 2.0)
#   g <- levelplot(SDData, main = titleText, col.regions = col.l, at = myat,
#                  colorkey = list(at = myat, col = col.l),
#                  xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
#   
#   g <- g + latticeExtra::layer(sp.polygons(coastsCoarse, col = "black", lwd = 0.5))
#   plotFileName <- paste0("graphics/cmip6/THI/THI_ensembleSD_masked_",   speciesName, "_",  yearSpan, "_", k, ".jpg")
#   jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
#   print(g)
#   dev.off()
#   fileNameout <- paste0("data/crops/rasterMask_", i, ".tif")
#   print(fileNameout)
#   writeRaster(rInAreaAgg, fileNameout, format = "GTiff", overwrite = TRUE)
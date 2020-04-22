# construct masks for locations of animals and plants in the early 20th century

source("R/globallyUsed.R")

sourceDir <- "data-raw/animals/arcrasters/"
animalsList <- list.files(sourceDir)

# run this code only when new animal data come in. This aggregates to 1/2 degree grids
# globeExtent   <- extent(c(-180, 180, -90, 90))
# 
# for (i in animalsList) {
#   species <- unlist(strsplit(i, "-"))[2]
#   if (species %in% "recl.asc") species = "livestockSystem"
#   fileName <- paste0("data/raster_", species)
#   rAnimal <- raster(paste0(sourceDir, i), format = "ascii")
#   rAnimal <- extend(rAnimal, globeExtent)
#   crs(rAnimal) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#   rAnimal <- aggregate(rAnimal, 6, fun = sum)
#   rAnimal <- extend(rAnimal, globeExtent)
#   print(fileName)
#   writeRaster(rAnimal, filename = fileName, format = "GTiff", overwrite = TRUE)
#   assign(fileName, rAnimal)
# }

# load the data for the number of animals in each 1/2 degree cell
for (i in animalsList) {
  speciesName <- unlist(strsplit(i, "-"))[2]
  if (speciesName %in% "recl.asc") speciesName = "livestockSystem"
  fileName <- paste0("data/animalCount/raster_", speciesName, ".tif")
  rIn <- raster(fileName)
  if (speciesName %in% "chicken") 
  {cutoff <- 100000
  }else{cutoff <- 1000}
  
  rIn[rIn < cutoff] <- NA
  rIn[rIn >= cutoff] <- 1
  fileNameout <- paste0("data/animalCount/rasterMask_", speciesName, ".tif")
  print(fileNameout)
  writeRaster(rIn, fileNameout, format = "GTiff", overwrite = TRUE)
}

# now do plants
getcropAreaYield <- function(cropName, dataType) {
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
  fileInArea <- paste0(pathToCrops, i, "/", i, plantAreaSuffix)
  fileInYield <- paste0(pathToCrops, i, "/", i, plantYieldSuffix)
  
  rInArea <- raster(fileInArea)
  # rInYield <- raster(fileInYield)
  
  # Earthstat data (using its harvest area) has a pixel size of  5 minute resolution. Need to convert to 1/2 degree to get to cmip6 cell size
  # 5 min = 0.0833333 degree
  # 30 min = 0.5 degree
  
  rInAreaAgg <- aggregate(rInArea, fact = 6, fun = "sum")
  cutoff <- 1000 # only include 1/2 cells where crop area is great than cutoff
  rInAreaAgg[rInAreaAgg < cutoff] <- NA
  rInAreaAgg[rInAreaAgg >= cutoff] <- 1
  
  fileNameout <- paste0("data/crops/rasterMask_", i, ".tif")
  print(fileNameout)
  writeRaster(rInAreaAgg, fileNameout, format = "GTiff", overwrite = TRUE)
  
}



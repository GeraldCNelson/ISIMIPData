# construct masks for locations of animals and plants in the early 20th century

source("R/globallyUsed.R")

sourceDir <- "data-raw/animals/arcrasters/"
animalsList <- list.files(sourceDir)

# run this code only when new animal data come in. This aggregates to 1/2 degree grids
globeExtent   <- ext(-180, 180, -90, 90)

pastureMask <-  rast("data-raw/crops/CroplandPastureArea2000_Geotiff/Pasture2000_5m.tif")
pastureMask <- aggregate(pastureMask, 6, fun = max)
pastureMask[pastureMask == 0] <- NA
pastureMask[pastureMask > 0] <- 1
fileName_mask <- paste0("data/animals/rasterMask_", "pasture", ".tif")
writeRaster(pastureMask, filename = fileName_mask, format = "GTiff", overwrite = TRUE)

i = "glw3-cattle-numbers.asc" # for testing
animalStats <- data.table(species = character(), ct = numeric())
for (i in animalsList) {
  species <- unlist(strsplit(i, "-"))[2]
  if (species %in% "recl.asc") species = "livestockSystem"
  fileName <- paste0("data/animals/raster_ct_", species, ".tif")
  rAnimal <- rast(paste0(sourceDir, i))
  values(rAnimal)[values(rAnimal) == -99] <- NA # -99 used for missing values
#  if (!i %in% "sheep") rAnimal <- aggregate(rAnimal, 6, fun = sum) # sheep are already in 30 sec
#  rAnimal <- extend(rAnimal, globeExtent)
  ct <- global(rAnimal, fun = "sum", na.rm = TRUE)
  if (!species %in% "livestockSystem") {
    ct <- global(rAnimal, fun = "sum", na.rm = TRUE)
    animalStats <- rbind(animalStats, list(species, ct$sum))
  }
  print(paste0(fileName))
  print(rAnimal)
  writeRaster(rAnimal, filename = fileName, format = "GTiff", overwrite = TRUE)
  rAnimal[rAnimal <= 100] <- NA
  fileName_mask <- paste0("data/animals/rasterMask_", species, ".tif")
  writeRaster(rAnimal, filename = fileName_mask, format = "GTiff", overwrite = TRUE)
  print(paste0(fileName_mask))
  print(rAnimal)
}

write.csv(animalStats, "data/animals/animalCt.csv")

# 
# # load the data for the number of animals in each 1/2 degree cell
# for (i in animalsList) {
#   speciesName <- unlist(strsplit(i, "-"))[2]
#   if (speciesName %in% "recl.asc") speciesName = "livestockSystem"
#   fileName <- paste0("data/animals/raster_ct_", speciesName, ".tif")
#   r <- rast(fileName)
#   # if (speciesName %in% "chicken") 
#   # {cutoff <- 1000
#   # }else{cutoff <- 10}
#   
#   # rIn[rIn < cutoff] <- NA
#   # rIn[rIn >= cutoff] <- 1
#   r[r <= 0] <- NA
#   r <- reclassify(rIn, rbind(c(-Inf, cutoff, NA), c(cutoff, Inf, 1)))
#   
#   fileName_out <- paste0("data/animals/rasterMask_", speciesName, ".tif")
#   print(fileName_out)
#   writeRaster(r, fileName_out, format = "GTiff", overwrite = TRUE)
# }

# now do plants

annCropsToAnalyze <- tolower(cropCharacteristics_annual$crop)

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

fruitsOnly <-  c("almond", "apple", "apricot", "avocado", "berrynes", "blueberry", 
                 "cherry", "cranberry", "currant", "grape", 
                 "grapefruitetc", "lemonlime", "orange", "peachetc", "persimmon", "rasberry", "sourcherry", 
                 "stonefruitnes", "walnut")

fruitsToAnalyse <- fruits # from globallyUsed.R

otherCrops <- crops[!crops %in% fruitsOnly]

crops.new <- c(annCropsToAnalyze, fruitsToAnalyse)

for (i in crops.new) {
  print(i)
  #  i <- "wheat"
  rInArea <- getcropAreaYield(i, "area") # returns a spatRaster
  

  # Earthstat data (using its harvest area) has a pixel size of 5 minute resolution. Need to convert to 1/2 degree to get to cmip6 cell size
  # 5 min = 0.0833333 degree
  # 30 min = 0.5 degree
  
  rInAreaAgg <- aggregate(rInArea, fact = 6, fun = "sum")
  if (i %in% fruitsToAnalyse) {
    cutoff <- .001 # only include 1/2 degree cells where crop area is great than cutoff
  }
  if (i %in% annCropsToAnalyze) {
    cutoff <- 5 # only include 1/2 degree cells where crop area is great than cutoff
  }
  # rInAreaAgg[rInAreaAgg < cutoff] <- NA
  # rInAreaAgg[rInAreaAgg > cutoff] <- 1
  r <- classify(rInAreaAgg, rbind(c(-Inf, cutoff, NA), c(cutoff, Inf, 1)))
  
  fileName_out <- paste0("data/crops/rasterMask_", i, ".tif")
  print(fileName_out)
  writeRaster(r, fileName_out, format = "GTiff", overwrite = TRUE)
  
}



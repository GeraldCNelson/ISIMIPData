# calculate 1-0 values for locations where chill portions are enough, by fruit
library(terra)
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
      # m <- "north"
       
        #   fileName_in <- paste0(fileLoc, k, "/", modelName.lower, "/", k, "_", modelName.lower, "_", midyear, "_chill_portions_", m, ".tif")
        # chillPortion <- rast(fileName_in)
        # system.time(SWC <- app(chillPortion, fun = quantile, probs=0.1, na.rm = TRUE)) 
        
        system.time(x <- lapply(modelChoices_lower, readRast_ensemble))
        SWC <- rast(x)
        
        # now do ensembles
        for (n in speciesChoice) {
          print(paste0("working on ssp: ", k, ", start year ", l, ", hemisphere ", m, ", crop ", n))
          cplimit <- fruitCPs$chillRequirement[fruitCPs$crop %in% n]
          SWC[SWC < cplimit] <- 0
          SWC[SWC > cplimit] <- 1
          
          maxVal <- round(max(minmax(SWC)), 2)
          minVal <- round(min(minmax(SWC)), 2)
          #     print(r)
          fileName_out <- paste0(fileLoc, "ensemble_chill_cutoff_", n, "_", k, "_", m, "_", yearSpan, ".tif")
          cat(paste0(red("species: ", m, ", ensemble ssp: ", k, ", start year: ", l, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName out: ", fileName_out), "\n\n"))
          print(system.time(r.mean <- tapp(r, indices_day, fun = "mean", na.rm = TRUE, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
          names(r.mean) <- gsub("X", "Day ", names(r.mean))
     }
    }
  }
}

# historical -----
k <- "historical"
l = 1991
midYear <- l + 9
for (m in hemispheres) {
  # m <- "north"
  
  #   fileName_in <- paste0(fileLoc, k, "/", modelName.lower, "/", k, "_", modelName.lower, "_", midyear, "_chill_portions_", m, ".tif")
  # chillPortion <- rast(fileName_in)
  # system.time(SWC <- app(chillPortion, fun = quantile, probs=0.1, na.rm = TRUE)) 
  
  system.time(x <- lapply(modelChoices_lower, readRast_ensemble))
  SWC <- rast(x)
  
  # now do ensembles
  for (n in speciesChoice) {
    print(paste0("working on ssp: ", k, ", start year ", l, ", hemisphere ", m, ", crop ", n))
    cplimit <- fruitCPs$chillRequirement[fruitCPs$crop %in% n]
    SWC[SWC < cplimit] <- 0
    SWC[SWC > cplimit] <- 1
    
    fileName_out <- paste0(fileLoc, k, "/", modelName.lower, "/", k, "_", modelName.lower, "_", l, "_chill_cutoff_", n, "_", m, ".tif")
    system.time(writeRaster(SWC, fileName_out, format = GTiff, overwrite = TRUE, wopt = woptList))
  }
}

# get list of files with their sizes
# filesIn <- list.files(fileLoc, recursive = TRUE, full.names = TRUE)
# for (i in filesIn){
#   info <- file.info(filename = i)
#   print(paste0("name: ", i, ", size ", info$size))
# }

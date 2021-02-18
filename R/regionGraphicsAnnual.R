# script to do cropping to specific locations
source("R/globallyUsed.R")
library(maps)
#remotes::install_github("ropensci/rnaturalearthhires") need to do once to get the library from github
library(rnaturalearthhires)
library(ggspatial)
library(data.table)
library(readxl)
library(ggplot2)
library(terra)
library(sf)
library(dplyr)
locOfCMIP6ncFiles <- "data-raw/ISIMIP/cmip6/unitsCorrected/"
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startYearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices_ensemble <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9
pal <- colorRampPalette(c("green","red"))
extentRange <- 2 # a value of 2 means 2 of the units of the raster; if it is 1/2 degree cells, this would be 1 degree

regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookup.xlsx", range = "A1:k7"))
#regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/CSVs2016_geocoordinates.xlsx", range = "F7:J47"))
regionInfoLookup[, ctyRegion := paste0("\n", region, ", ",  country)]

fileLoc_annualMean <- "data/cmip6/annualMean/"
climateVars <- c("pr", "hurs", "tasmax", "tasmin", "tave")

world <- loadSpatialData("world")
lakes <- loadSpatialData("lakes")
rivers <- loadSpatialData("rivers")
roads <- loadSpatialData("roads")
cities <- loadSpatialData("cities")
protectedAreas0 <- loadSpatialData("protectedAreas0")
protectedAreas1 <- loadSpatialData("protectedAreas1")
protectedAreas2 <- loadSpatialData("protectedAreas2")
populatedAreas <- loadSpatialData("populatedAreas")
# 
# fileName_rtave_observed <- "data/cmip6/annualMean/annualMean_tave_observed_2001_2010.tif"
# fileName_rtave_2021_2030_ssp585 <- "data/cmip6/annualMean/ensembleAnnualMean_tave_2021_2030_ssp585.tif"
# fileName_rtave_2051_2060_ssp585 <- "data/cmip6/annualMean/ensembleAnnualMean_tave_2051_2060_ssp585.tif"
# fileName_rtave_2091_2100_ssp585 <- "data/cmip6/annualMean/ensembleAnnualMean_tave_2091_2100_ssp585.tif"

for (j in climateVars) {
  
  #      world <- ne_countries(scale = "large", returnclass = "sf")
  # lakes10 <- ne_download(scale = 10, type = 'lakes', category = 'physical', returnclass = "sf")
  # rivers10 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf")
  # roads10 <- ne_download(scale = 10, type = 'roads', category = 'cultural', returnclass = "sf")
  # cities10 <- ne_download(scale = 10, type = 'urban_areas', category = 'cultural', returnclass = "sf")
  # 
  # write out the xxx10 files to shapefiles
  # st_write(lakes10, "data-raw/regioninformation/lakes10.shp", layer_options = "ENCODING=UTF-8")
  # st_write(rivers10, "data-raw/regioninformation/rivers10.shp", layer_options = "ENCODING=UTF-8")
  # st_write(roads10, "data-raw/regioninformation/roads10.shp", layer_options = "ENCODING=UTF-8")
  # st_write(cities10, "data-raw/regioninformation/cities10.shp", layer_options = "ENCODING=UTF-8")
  
  #shadedRelief <- rast("data-raw/regioninformation/SR_HR/SR_HR.tif")
  
  for (k in sspChoices) {
    for (i in 1:(nrow(regionInfoLookup))) {
      region <- regionInfoLookup[i, region]
      print(paste0("working on ", region))
      country <- regionInfoLookup[i, country]
      iso3Code <- regionInfoLookup[i, ISO3]
      locLat <- regionInfoLookup[i, latitude]
      locLong <- regionInfoLookup[i, longitude]
      bb <- c(regionInfoLookup[i, ll.lat], regionInfoLookup[i, ll.lon], regionInfoLookup[i, ur.lat], regionInfoLookup[i, ur.lon])  
      xlim <- c( regionInfoLookup[i, ll.lon], regionInfoLookup[i, ur.lon])
      ylim <- c(regionInfoLookup[i, ll.lat], regionInfoLookup[i, ur.lat])
      regionExt <- ext(c(xlim, ylim))
      regionBox <- c(xmin = regionInfoLookup[i, ll.lon], ymin = regionInfoLookup[i, ll.lat], xmax = regionInfoLookup[i, ur.lon], ymax = regionInfoLookup[i, ur.lat])
      
      pa0 <- protectedAreas0[protectedAreas0$PARENT_ISO == iso3Code,]
      pa1 <- protectedAreas1[protectedAreas1$PARENT_ISO == iso3Code,]
      pa2 <- protectedAreas2[protectedAreas2$PARENT_ISO == iso3Code,]
      #     world_region <- world[world$sov_a3 == iso3Code,]
      world_region <- createRegionSpatialData("world", regionBox)
      lakes_region <- createRegionSpatialData("lakes", regionBox)
      rivers_region <- createRegionSpatialData("rivers", regionBox)
      roads_region <- createRegionSpatialData("roads", regionBox)
      cities_region <- createRegionSpatialData("cities", regionBox)
      populatedAreas_region <- createRegionSpatialData("populatedAreas", regionBox)
      # urbanAreas_region <- createRegionSpatialData("urbanAreas", regionBox)
      
      # code to get min and max values across all periods
      meanData <- c()
      meanData <- rast(paste0("data/cmip6/annualMean/annualMean_", j, "_observed_2001_2010.tif"))
      for (l in startYearChoices_ensemble) {
        yearSpan <- paste0(l, "_", l + yearRange)
        meanData <- c(meanData, rast(paste0("data/cmip6/annualMean/ensembleAnnualMean_", j,  "_",  yearSpan, "_", k, ".tif")))
      }
      
      climVar_crop <- crop(meanData, regionExt)
      climVarMin <- min(global(climVar_crop, fun = "min", na.rm = TRUE))
      climVarMax <- max(global(climVar_crop, fun = "max", na.rm = TRUE))
      climVarMax <- ceiling(climVarMax)
      climVarMin <- floor(climVarMin)
      
      custom_bins <- round(seq.int(from = climVarMin, to = climVarMax, length = 6))
      
      dataHolder <- data.table(x = numeric(), y = numeric(), mean = numeric())
      for (l in startYearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        if (l == 2001) {
          r_climVar <- paste0("data/cmip6/annualMean/annualMean_", j, "_observed_2001_2010.tif")
        } else {
          r_climVar <- paste0(fileLoc_annualMean, "ensembleAnnualMean_", j, "_", yearSpan,"_", k, ".tif")
        }
        r_climVar <- rast(r_climVar)
        #        relief <- crop(shadedRelief, regionExt)
        r_climVar_region <- crop(r_climVar, regionExt)
        r_climVar_region <- as.data.frame(r_climVar_region, xy = TRUE)
        r_climVar_region$mean <- round(r_climVar_region$mean, 2)
        if (l %in% startYearChoices[1]) {
          dataHolder <- as.data.table(r_climVar_region)
          setnames(dataHolder, old = c("x", "y", "mean"), new = c("longitude", "latitude", paste0(j, " ", l)))
        } else {
          newColName <- paste0(j, " ", l)
           dataHolder[, c(newColName) := r_climVar_region$mean]
        }
        
        # get 5 color palatte
        p <- colorspace::sequential_hcl(n = 5, palette = "Terrain 2", rev = TRUE)
        
        my_col <-  heat.colors(5, rev = TRUE)
        
        titleText <- paste0("Annual mean ", varNamesInfo[variableShortName %in% j, variableLongName], " ", gsub("_", "-", yearSpan), ", ",  "scenario ", k, ", ", regionInfoLookup[i, ctyRegion])
        legendTitle <- varNamesInfo[variableShortName %in% j, units]
        print(titleText)
        g <- ggplot(data = world_region) +
          labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
          labs(x = "", y = "") +
          geom_raster(data = r_climVar_region, aes(x, y, fill = mean)) + 
          # scale_fill_gradientn(colors = p, # name = legendText,
          #                      na.value = "grey50",
          #                      guide = "colorbar") +
          scale_fill_gradientn(colours=topo.colors(7),na.value = "transparent",
                               breaks=custom_bins,labels = custom_bins,
                               limits=c(climVarMin, climVarMax)) +
          geom_sf(fill = NA, color = "gray") + 
          
          geom_sf(data = roads_region, color = "red", lwd = 0.5, fill = NA)
        if (nrow(populatedAreas_region) > 0) { 
          g <- g + geom_sf(data = populatedAreas_region, color = "black", lwd = 0.5, fill = NA) +
            geom_sf_label(data = populatedAreas_region, aes(label = NAME), size = 2, fill = NA)
        }
        if (nrow(rivers_region) > 0) { 
          g <- g + geom_sf(data = rivers_region, color = "blue", lwd = 0.5, fill = NA) 
        }
        # geom_sf(data = pa0_region) +
        # geom_sf(data = pa1_region) +
        # geom_sf(data = pa2_region) +
        
        # geom_sf_label(data = pa0_region, aes(label = NAME), color = "green") + 
        # geom_sf_label(data = pa1_region, aes(label = NAME), color = "green") + 
        # geom_sf_label(data = pa2_region, aes(label = NAME), color = "green") + 
 #       g <- g +  geom_sf(data = world_region, fill = NA, color = gray(.5)) +
          g <- g + geom_point(aes(x = locLong, y= locLat), colour="blue", size = 2, fill = NA) +
          geom_label(x = locLong + .35 , y= locLat, label = region,  label.size = .4, color = "black",
                     fill = NA) +
          
          annotation_scale(location = "bl", width_hint = 0.5) +
          annotation_north_arrow(location = "bl", which_north = "true", 
                                 pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                                 style = north_arrow_minimal) 
        
        print(g)
        outFilename <- paste0("graphics/cmip6/regionInfo/", j, "_", k, "_", yearSpan, "_", regionInfoLookup[i, region], ".png")
        unlink(outFilename)
        ggsave(outFilename, plot = last_plot(), device = "png", width = 6, height = 6, overwrite = TRUE)
        # ggsave("map_web.png", width = 6, height = 6, dpi = "screen")
      }
      write.csv(dataHolder, file = paste0("data/regionResults/", j, "_", regionInfoLookup[i, region], "_", k, ".csv"), row.names = FALSE)
    }
  }
}
  
  
  

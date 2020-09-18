# script to do cropping to specific locations
source("R/globallyUsed.R")
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9
pal <- colorRampPalette(c("green","red"))
extentRange <- 2 # a value of 2 means 2 of the units of the raster; if it is 1/2 degree cells, this would be 1 degree

#test values
i <- "GFDL"
k <- "ssp585"
l <- 2091
j = 1 # for cattle

globeExtent   <- ext(-180, 180, -90, 90)
globeExtent_reduced   <- ext( -180, 180, -50, 50  )


#extent is  vector (length=4; order= xmin, xmax, ymin, ymax)
# do a globe with site location
r_tave <- rast("data/cmip6/annualMean/ensembleAnnualMean_tave_2021_2030_ssp585.tif")
r_tave <- rast("data/cmip6/annualMean/ensembleAnnualMean_tave_2091_2100_ssp585.tif")
r_tave_df <- as.data.frame(r_tave, xy = TRUE)
plotTitle <- "Average annual temperature (°C), 2021-2030; scenario = ssp 585"
plotTitle <- "Average annual temperature (°C), 2091-2100; scenario = ssp 585"
my_col <- heat.colors
library(ggspatial)
library(maptools)
library(ggsn)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
countries <- ne_countries(scale=110)


locLat <- regionInfoLookup[i, latitude]
locLong <- regionInfoLookup[i, longitude]
extentWindow_xlim <- c(locLong - extentRange, locLong + extentRange)
extentWindow_ylim <- c(locLat - extentRange, locLat + extentRange)

g <- ggplot() +
  geom_raster(data = r_tave_df, aes(x = x, y = y, fill = mean)) +
  #theme_bw() +
  # borders(
  #   database = "world",
  #   regions = ".",
  #   fill = NA,
  #   colour = "grey50",
  #   xlim = NULL,
  #   ylim = NULL
  # ) +
  #  scale_color_continuous(type =  "gradient") +
  scale_fill_gradientn(colours=c("#5533FF","#FFFFFFFF","#FF0000FF"), breaks = c(-20, 0, 5, 10, 20)) +
  #  geom_polygon(data= countries, aes(x=long, y=lat, group=group),  color="white", lwd = .25) +
  
  theme(legend.position = "bottom") +
  geom_sf() +
  #  coord_sf(xlim = extentWindow_xlim, ylim = extentWindow_ylim) +
  #  annotation_scale(location = "bl", width_hint = 0.5, plot_unit = "km") +
  # ggspatial::annotation_scale(
  #   location = "bl", plot_unit = "km",
  #   bar_cols = c("grey60", "white")
  # )+ 
  # coord_sf(
  #   # xlim = NULL,
  #   # ylim = NULL,
  #   # expand = TRUE,
  #   crs = crs(r_tave),
#   # #    datum = sf::st_crs(4326),
#   # label_graticule = waiver(),
#   # label_axes = waiver(),
#   # ndiscr = 100,
#   # default = FALSE,
#   # clip = "on"
# ) +
geom_point(data=regionInfoLookup, aes(x = longitude, y = latitude)) +
  geom_text(data=regionInfoLookup, size = 2, aes(x = longitude, y = latitude, label = ctyRegion, hjust=.1, vjust=.6)) +
  labs(title = plotTitle,
       subtitle = "",
       color = "",
       x = "", y = "") +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.title = element_blank()) + 
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(family = "Cambria", face = "plain")) +
  labs(fill = "Mean temperature (°C)") # legend title

# theme(legend.title = "Mean temperature (°C)") +
#  scale_fill_gradient(low = lowColor, high = highColor, guide = "legend", name = legendText, limits = fillLimits) +
#coord_quickmap()

fileOut <- paste0(getwd(), "/globeLocations.png")

# png(filename = fileOut,  width = 480, height = 480, units = "px", pointsize = 12,
#     bg = "white",  res = NA, type = c("cairo", "cairo-png", "Xlib", "quartz"), antialias)
png(filename = fileOut, bg = "white",  width = 960, height = 960)
dev.new( , noRStudioGD = FALSE) 
plot(r_tave, main = "Average annual temperature (°C), 2021-2030; scenario = ssp 585", col = pal(7))
scalebar(10000, xy = NULL, type = "bar", divs = 4, below = "kilometers", 
         lonlat = TRUE, adj=c(1.1, -1.1), lwd = 2)

for (i in 1:nrow(regionInfoLookup)) {
  
  locLat <- regionInfoLookup[i, latitude]
  locLong <- regionInfoLookup[i, longitude]
  points(locLong, locLat)
  locText_region <- regionInfoLookup[i, region]
  locText_country <- regionInfoLookup[i, country]
  locText <- paste0(locText_region, ",\n ", locText_country)
  text(locLong, locLat, locText, pos = 1, cex = 1) # pos = 1 is below
}
dev.off()
#now do site specific maps
pal <- colorRampPalette(c("green","red"))
for (i in 1:nrow(regionInfoLookup)) {
  locLat <- regionInfoLookup[i, latitude]
  locLong <- regionInfoLookup[i, longitude]
  locExtent <- ext(locLong - extentRange, locLong + extentRange, locLat - extentRange, locLat + extentRange)
  locText_region <- regionInfoLookup[i, region]
  locText_country <- regionInfoLookup[i, country]
  locText <- paste0(locText_region, ",\n ", locText_country)
  r_crop <- crop(r_tave, locExtent)
  locText <- paste0(locText_region, ", ", locText_country)
  mainText <- paste0( "Average annual temperature (°C), 2021-2030; scenario = ssp 585", "\n", locText)
  plot(r_crop, main = mainText, col = pal(7))
  points(locLong, locLat)
  
  text(locLong, locLat, locText, pos = 1, cex = 0.7) # pos = 1 is below
  plot(coastsCoarse, add = TRUE)
  scalebar(100, xy = NULL, type = "bar", divs = 4, below = "kilometers", 
           lonlat = TRUE, adj=c(0.5, -0.5), lwd = 2)
}

testExtent <- ext(locLong - extentRange, locLong + extentRange, locLat - extentRange, locLat + extentRange)
testCrop <- crop(r, testExtent)
points(locLong, locLat)
plot(coastsCoarse, add = TRUE)
plot(testCrop, 40)
points(locLong, locLat)
plot(coastsCoarse, add = TRUE)
}
yearSpan <- paste0(l, "_", l + yearRange)
speciesName <- gsub("thi.", "", thiListReduced[j])
fileNameCt <- paste0("data/animals/raster_ct_", speciesName,  ".tif")
fileNameMean.masked <- paste0("data/cmip6/THI/THI_ensembleMean_masked_",speciesName, "_",  yearSpan, "_", k, ".tif")

temp <- rast(fileNameMean.masked)

temp_WA <- crop(temp, westAfricaExtent)
temp_nG <- crop(temp, northernGhanaExtent)

extentToUse <- northernGhanaExtent

# ensemble graphics
# apply masks, can only do this to animals we have in THIlist and that have area mask raster
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data
bpList <- as.data.table(read_excel("data-raw/animals/AnimalbreakpointslistRaw.xlsx"))

for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange)
    print(paste0("ssp choice: ", k, ", start year: ", l))
    for (j in 1:length(thiListReduced)) {
      speciesName <- gsub("thi.", "", thiListReduced[j])
      fileNameMean.masked <- paste0("data/cmip6/THI/THI_ensembleMean_masked_", speciesName, "_",  yearSpan, "_", k, ".tif")
      print(paste0("fileNameMean.masked: ", fileNameMean.masked))
      fileNameCV.masked <- paste0("data/cmip6/THI/THI_ensembleCV_masked_", speciesName, "_",  yearSpan, "_", k, ".tif")
      print(paste0("fileNameCV.masked: ", fileNameCV.masked))
      meanData <- rast(fileNameMean.masked)
      meanData <- crop(meanData, extentToUse)
      #      meanData <- terra::project(meanData, crsRob)
      names(meanData) <- month.abb
      CVData <- rast(fileNameCV.masked)
      #      CVData <- terra::project(CVData, crsRob)
      names(CVData) <- month.abb
      
      # plot Ensemble mean
      titleText <- paste0("THI stress levels by month, ", speciesName, "\n ", yearSpan, ", SSP = ", k, ", ensemble mean")
      zeroLevel <- bpList[species %in% speciesName, zeroLevel]
      noStress <- bpList[species %in% speciesName, noStress]
      moderateStress <- bpList[species %in% speciesName, moderateStress]
      extremeStress <- bpList[species %in% speciesName, extremeStress]
      col.l <- c("darkslategray1", "blue", "yellow", "red")
      mapTheme <- rasterTheme(region = col.l)  
      mapTheme$panel.background$col = 'white' 
      myat <- c(zeroLevel, noStress, moderateStress, extremeStress, 100)
      #     writeRaster(meanData, filename ="temp.tif", format = "GTiff", overwrite = TRUE)
      meanData <- raster::brick(meanData)
      #      meanData <- as(meanData, "Raster")
      # meanDataR <- as(meanData, "rasterBrick")
      # meanDataR <- raster::brick(meanData)
      g <- levelplot(meanData, main = titleText, col.regions = col.l, at = myat, par.settings = mapTheme, 
                     colorkey = list(at = myat, col = col.l, labels = c( "","No stress", "moderate stress", "extreme stress", "maximum")),
                     xlab = "", ylab = "", scales  = list(x = list(draw = FALSE), y = list(draw = FALSE)))
      
      g <- g + latticeExtra::layer(sp.polygons(coastsCoarse.Rob, col = "black", lwd = 0.5))
      plotFileName <- paste0("graphics/cmip6/THI/THI_ensembleMean_masked_",  speciesName, "_",  yearSpan, "_", k, ".jpg")
      print(paste0("plot file name: ", plotFileName, " for species ", speciesName))
      jpeg(plotFileName, width = 8, height = 8, quality = 100, units = "in", res = 300)
      print(g)
      dev.off()
      
      # get map from google maps
      library(ggmap)
      for (i in 1:nrow(regionInfoLookup)) {
        locLat <- regionInfoLookup[i, latitude]
        locLong <- regionInfoLookup[i, longitude]
        bb <- c(regionInfoLookup[i, ll.lat], regionInfoLookup[i, ll.lon], regionInfoLookup[i, ur.lat], regionInfoLookup[i, ur.lon])  
        xlim <- c( regionInfoLookup[i, ll.lon], regionInfoLookup[i, ur.lon])
        ylim <- c(regionInfoLookup[i, ll.lat], regionInfoLookup[i, ur.lat])
        titleText <- paste0("Location of ", regionInfoLookup[i, ctyRegion])
        map_rds <- get_map(location = c(lon = locLong, lat = locLat), zoom = 10)
        # map_sat <- get_map(location = c(lon = locLong, lat = locLat), maptype = "satellite")
        mapName_rds <- paste0("map_location_rds", i)
        # mapName_sat <- paste0("map_location_sat", i)
        # map_rds <- get_stamenmap(bbox = bb, maptype = "terrain-labels")
        assign(mapName_rds, map_rds)
        #       assign(mapName_sat, map_sat)
        print(ggmap(get(mapName_rds))  +  labs(title = titleText) + theme(plot.title = element_text(size = 12, hjust = 0.5)))
        #      print(ggmap(get(mapName_sat))  +  labs(title = titleText) + theme(plot.title = element_text(size = 12, hjust = 0.5)))
      }
      
      # bbholder <- data.table(ll.lat = numeric(), ll.lon = numeric(),  ur.lat = numeric(), ur.lon  = numeric())
      # for (i in 1:5) {
      #   mapName <- paste0("map_location_rds", i)
      #   bb <- as.data.table(attr(get(mapName), "bb"))
      #   bbholder <- rbind(bbholder, bb)
      # }
      write.csv(bbholder, "data-raw/regionInformation/googleBBs.csv")
      g <- ggmap(get(mapName_rds))  +  labs(title = titleText) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
        geom_raster(data = r_tave_df, aes(x = x, y = y, fill = mean)) +
        
        scale_fill_gradientn(colours=c("#5533FF","#FFFFFFFF","#FF0000FF"), breaks = c(-20, 0, 5, 10, 20)) +
        
        theme(legend.position = "bottom")
      
      
      # import natural earth shapefile
      library("sf")
      library("rnaturalearth")
      library("rnaturalearthdata")
      library(maps)
      #remotes::install_github("ropensci/rnaturalearthhires") need to do once to get the library from github
      library(rnaturalearthhires)
      library(ggspatial)
      library(data.table)
      library(readxl)
      library(ggplot2)
      library(terra)
      
      regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookup.xlsx"))
      #regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/CSVs2016_geocoordinates.xlsx", range = "F7:J47"))
      regionInfoLookup[, ctyRegion := paste0(region , "\n", country)]
      regionInfoLookup[, ctyRegion := paste0(`region` , "\n", country)]
      
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
      
      world <- st_read("data-raw/regioninformation/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")
      lakes10 <-  st_read("data-raw/regioninformation/lakes10.shp")
      rivers10 <- st_read("data-raw/regioninformation/rivers10.shp")
      roads10 <- st_read("data-raw/regioninformation/roads10.shp")
      cities10 <- st_read("data-raw/regioninformation/cities10.shp")
      
      populatedAreas10 <- st_read("data-raw/regioninformation/ne_10m_populated_places/ne_10m_populated_places.shp")
      protectedAreas2 <- st_read("data-raw/regioninformation/WDPA_Aug2020-shapefile/WDPA_Aug2020-shapefile2/WDPA_Aug2020-shapefile-polygons.shp")
      protectedAreas1 <- st_read("data-raw/regioninformation/WDPA_Aug2020-shapefile/WDPA_Aug2020-shapefile1/WDPA_Aug2020-shapefile-polygons.shp")
      protectedAreas0 <- st_read("data-raw/regioninformation/WDPA_Aug2020-shapefile/WDPA_Aug2020-shapefile0/WDPA_Aug2020-shapefile-polygons.shp")
      shadedRelief <- rast("data-raw/regioninformation/SR_HR/SR_HR.tif")
      r_tave <- rast("data/cmip6/annualMean/ensembleAnnualMean_tave_2021_2030_ssp585.tif")
      r_tave <- rast("data/cmip6/annualMean/ensembleAnnualMean_tave_2091_2100_ssp585.tif")
      
      
      for (i in 1:(nrow(regionInfoLookup) - 1)) {
        print(i)
        locLat <- regionInfoLookup[i, latitude]
        locLong <- regionInfoLookup[i, longitude]
        bb <- c(regionInfoLookup[i, ll.lat], regionInfoLookup[i, ll.lon], regionInfoLookup[i, ur.lat], regionInfoLookup[i, ur.lon])  
         xlim <- c( regionInfoLookup[i, ll.lon], regionInfoLookup[i, ur.lon])
        ylim <- c(regionInfoLookup[i, ll.lat], regionInfoLookup[i, ur.lat])
        regionExt <- ext(c(xlim, ylim))
#        relief <- crop(shadedRelief, regionExt)
        r_tave_region <- crop(r_tave, regionExt)
        r_tave_region <- as.data.frame(r_tave_region, xy = TRUE)
        my_col <-  heat.colors(5)
        
        titleText <- paste0("Location of ", regionInfoLookup[i, ctyRegion])
        titleText <- paste0("Annual mean temperature, 2090-2100 ", regionInfoLookup[i, ctyRegion])
        iso3Code <- regionInfoLookup[i, ISO3]
        pa0 <- protectedAreas0[protectedAreas0$PARENT_ISO == iso3Code,]
        pa1 <- protectedAreas1[protectedAreas1$PARENT_ISO == iso3Code,]
        pa2 <- protectedAreas2[protectedAreas2$PARENT_ISO == iso3Code,]
        world_region <- world[world$sov_a3 == iso3Code,]
        popArea_region <- populatedAreas10[populatedAreas10$ADM0_A3 == iso3Code, ]
        r_tave_region <- r_tave_region %>% mutate(mean_breaks = cut(mean, breaks = 5))
        g <- ggplot(data = world_region) +
          geom_sf() + 
          labs(title = titleText) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
          geom_raster(data = r_tave_region , aes(x = x, y = y, fill = mean_breaks)) + 
          geom_sf(data = world_region, fill = NA, color = gray(.5)) +
          geom_sf(data = roads10, color = "red", lwd = 1) +
          geom_sf(data = populatedAreas10) +
          geom_sf_label(data = popArea_region,  aes(label = NAME), size = 5) +
          # theme(axis.title = element_blank()) + 
           scale_fill_manual(values = my_col, name = "Mean Temperature (°C)") + 
          coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
          
          geom_sf(data = lakes10, color = "darkblue") +
          geom_sf(data = rivers10, color = "blue") +
          geom_sf(data = pa0) +
          geom_sf(data = pa1) +
          geom_sf(data = pa2) +
          geom_sf(data = populatedAreas10) +
          
          geom_sf_label(data = pa0, aes(label = NAME), color = "green") + 
          geom_sf_label(data = pa1, aes(label = NAME), color = "green") + 
          geom_sf_label(data = pa2, aes(label = NAME), color = "green") + 
          geom_sf_label(data = popArea_region,  aes(label = NAME), size = 5) +
          
          coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
          guides(color = guide_legend(override.aes = list(size = 6))) +
          theme_bw() + 
          
          annotation_scale(location = "bl", width_hint = 0.5) +
          annotation_north_arrow(location = "bl", which_north = "true", 
                                 pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                                 style = north_arrow_fancy_orienteering) 
        
        print(g)
        outFilename <- paste0("graphics/cmip6/regionInfo/", regionInfoLookup[i, region], ".png")
        unlink(outFilename)
        ggsave(outFilename, plot = last_plot(), device = "png", overwrite = TRUE)
        # ggsave("map_web.png", width = 6, height = 6, dpi = "screen")
      }
      
      
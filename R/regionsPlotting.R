# script to do cropping to specific locations
#source("R/globallyUsed.R")
library(maps)
#remotes::install_github("ropensci/rnaturalearthhires") need to do once to get the library from github
library(rnaturalearthhires)
library(ggspatial)
library(data.table)
library(readxl)
library(ggplot2)
library(terra)
locOfCMIP6ncFiles <- "data-raw/ISIMIP/cmip6/unitsCorrected/"
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

fileName_rtave_observed <- "data/cmip6/annualMean/annualMean_tave_observed_2001_2010.tif"
fileName_rtave_2021_2030_ssp585 <- "data/cmip6/annualMean/ensembleAnnualMean_tave_2021_2030_ssp585.tif"
fileName_rtave_2091_2100_ssp585 <- "data/cmip6/annualMean/ensembleAnnualMean_tave_2091_2100_ssp585.tif"
fileLoc_annualMean <- paste("data/cmip6/annualMean/annualMean")

fileName_tave_in <- paste()

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
#shadedRelief <- rast("data-raw/regioninformation/SR_HR/SR_HR.tif")


for (k in sspChoices) {
  for (l in startyearChoices_ensemble) {
    yearSpan <- paste0(l, "_", l + yearRange))
    r_tave <- paste0(fileLoc_annualMean, "_tave_", yearRange <- 9
                     
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
      ggsave(outFilename, plot = last_plot(), device = "png")
      # ggsave("map_web.png", width = 6, height = 6, dpi = "screen")
    }
  }
}
  
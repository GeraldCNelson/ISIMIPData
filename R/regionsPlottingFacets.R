# region data with facets, say for months
source("R/globallyUsed.R")
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
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoicesEnsemble <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9
pal <- colorRampPalette(c("green","red"))
extentRange <- 2 # a value of 2 means 2 of the units of the raster; if it is 1/2 degree cells, this would be 1 degree

regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookup.xlsx"))
#regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/CSVs2016_geocoordinates.xlsx", range = "F7:J47"))
regionInfoLookup[, ctyRegion := paste0(region , "\n", country)]

world <- loadSpatialData("world")
lakes <- loadSpatialData("lakes")
rivers <- loadSpatialData("rivers")
roads <- loadSpatialData("roads")
cities <- loadSpatialData("cities")

for (i in 1:(nrow(regionInfoLookup))) {
  region <- regionInfoLookup[i, region]
  country <- regionInfoLookup[i, country]
  iso3Code <- regionInfoLookup[i, ISO3]
  locLat <- regionInfoLookup[i, latitude]
  locLong <- regionInfoLookup[i, longitude]
  bb <- c(regionInfoLookup[i, ll.lat], regionInfoLookup[i, ll.lon], regionInfoLookup[i, ur.lat], regionInfoLookup[i, ur.lon])  
  xlim <- c( regionInfoLookup[i, ll.lon], regionInfoLookup[i, ur.lon])
  ylim <- c(regionInfoLookup[i, ll.lat], regionInfoLookup[i, ur.lat])
  regionExt <- ext(c(xlim, ylim))
  
  pa0 <- protectedAreas0[protectedAreas0$PARENT_ISO == iso3Code,]
  pa1 <- protectedAreas1[protectedAreas1$PARENT_ISO == iso3Code,]
  pa2 <- protectedAreas2[protectedAreas2$PARENT_ISO == iso3Code,]
  #     world_region <- world[world$sov_a3 == iso3Code,]
  regionBox <- c(xmin = regionInfoLookup[i, ll.lon], ymin = regionInfoLookup[i, ll.lat], xmax = regionInfoLookup[i, ur.lon], ymax = regionInfoLookup[i, ur.lat])
  world_region <- createRegionSpatialData("world", regionBox)
  lakes_region <- createRegionSpatialData("lakes", regionBox)
  rivers_region <- createRegionSpatialData("rivers", regionBox)
  roads_region <- createRegionSpatialData("roads", regionBox)
  cities_region <- createRegionSpatialData("cities", regionBox)
  populatedAreas_region <- createRegionSpatialData("populatedAreas", regionBox)
 # urbanAreas_region <- createRegionSpatialData("urbanAreas", regionBox)
  
  for (k in sspChoices) {
 #   dataHolder <- data.table(x = numeric(), y = numeric(), mean = numeric())
    # from https://stackoverflow.com/questions/37376398/how-to-create-an-empty-datatable-with-columns-names-and-then-append-datatables-t
    dataHolder <- data.table(1)[,`:=`(c("x", "y", month.abb),NA)][,V1:=NULL][.0]
    for (l in startyearChoicesEnsemble) {
      tmax <- rast("data/cmip6/monthlyMean/ensembleMonthlyMean_tasmax_2051_2060_ssp585.tif")
      yearSpan <- paste0(l, "_", l + yearRange)
      
      tmax_region <- crop(tmax, regionExt)
      tmax_region_df <- as.data.frame(tmax_region, xy = TRUE)
      if (l %in% startyearChoicesEnsemble[1]) {
        dataHolder <- as.data.table(tmax_region_df)
        dataHolder[, c(month.abb) := round(month.abb, 2)]
        setnames(dataHolder, old = c("x", "y"), new = c("longitude", "latitude"))
      } else {
        newColName <- paste0("meanTemp_", l)
        dataHolder[, c(newColName) := r_tave_region$mean]
      }
      
      tmax_region_df_melt <- tidyr::pivot_longer(tmax_region_df, cols = Jan:Dec, names_to = c("variable"))
      taveMin <- min(global(tmax_region, fun = "min", na.rm = TRUE))
      taveMin <- floor(taveMin)
      taveMax <- max(global(tmax_region, fun = "max", na.rm = TRUE))
      taveMax <- ceiling(taveMax)
      custom_bins <- round(seq.int(from = taveMin, to = taveMax, length = 5))
      
      titleText <- paste0("Ensemble monthly mean temperature, ", gsub("_", "-", yearSpan), ", ",  "scenario ", k, ", ", regionInfoLookup[i, ctyRegion])
      legendTitle <- "°C"
      
      tmax_region_df_melt$variable <- factor(tmax_region_df_melt$variable, levels = month.abb)
      
      g <- ggplot(data = world_region) +
        labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) + 
        labs(x = "", y = "") +
       geom_raster(data = tmax_region_df_melt, aes(x = x, y = y, fill = value)) +
        geom_sf(fill = NA, color = "gray") +
        scale_fill_gradientn(colours = topo.colors(7), na.value = "transparent",
                             breaks=custom_bins,labels = custom_bins,
                             limits=c(taveMin, taveMax)) +
        facet_wrap(vars(variable)) +
        geom_sf(data = roads_region, color = "red", lwd = 0.5, fill = NA) +
        geom_sf(data = populatedAreas_region, color = "black", lwd = 0.5, fill = NA) +
        geom_sf(data = lakes_region, color = "darkblue", fill = NA) +
        geom_sf(data = rivers_region, color = "blue", lwd = 0.5, fill = NA) +
        geom_sf_label(data = populatedAreas_region,  aes(label = name), size = 2, fill = NA)
      # geom_sf(data = pa0_region) +
        # geom_sf(data = pa1_region) +
        # geom_sf(data = pa2_region) +
        
        # geom_sf_label(data = pa0_region, aes(label = NAME), color = "green") + 
        # geom_sf_label(data = pa1_region, aes(label = NAME), color = "green") + 
        # geom_sf_label(data = pa2_region, aes(label = NAME), color = "green") + 
      print(g)
      outFilename <- paste0("graphics/cmip6/regionInfo/tmaxMonthAve", "_", k, "_", yearSpan, "_", regionInfoLookup[i, region], ".png")
      ggsave(outFilename, plot = last_plot(), device = "png", width = 6, height = 6)
      # ggsave("map_web.png", width = 6, height = 6, dpi = "screen")
    }
  }
}

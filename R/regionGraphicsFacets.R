# region data with facets, say for months
source("R/globallyUsed.R")
#library(rnaturalearthhires)
library(ggspatial)
library(sf)
library(dplyr)

#test vars
j <- "pr"
k <- "ssp585"
l <- 2051
i <- 6

locOfCMIP6ncFiles <- "data-raw/ISIMIP/cmip6/unitsCorrected/"
sspChoices <- c("ssp585") #"ssp126"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_ensemble <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9

climateVars <- c("pr", "hurs", "tasmax", "tave", "tasmin") #""pr", "hurs", tasmax",   "tasmin", "tave"
#climateVars <- c("hurs") #""pr", "hurs", tasmax",   "tasmin", "tave"
fileLoc_monthlyMean <- "data/cmip6/monthlyMean/"

#regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookup.xlsx", range = "A1:k7"))
regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/wg2ch5Locations.xlsx", range = "a1:k16")) # climate smart villages
#regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookupCSVs.xlsx",range = "F7:P43")) #perennial crop author locations
#regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/PerennialCrops.xlsx",range = "A1:K6")) #perennial crop author locations
regionInfoLookup[, ctyRegion := paste0("\n", region, ", " , country)]

world <- loadSpatialData("world")
lakes <- loadSpatialData("lakes")
rivers <- loadSpatialData("rivers")
roads <- loadSpatialData("roads")
cities <- loadSpatialData("cities")
protectedAreas0 <- loadSpatialData("protectedAreas0")
protectedAreas1 <- loadSpatialData("protectedAreas1")
protectedAreas2 <- loadSpatialData("protectedAreas2")
populatedAreas <- loadSpatialData("populatedAreas")

for (j in climateVars) {
  print(j)
  for (k in sspChoices) {
    for (i in 11){ #1:nrow(regionInfoLookup)) {
      gc()
      
      region <- regionInfoLookup[i, region]
      country <- regionInfoLookup[i, country]
      iso3Code <- regionInfoLookup[i, ISO3]
      print(system.time(adminRegions <- readRDS(paste0("data-raw/regionInformation/regionsISO3/ISO3_", iso3Code, "1.RDS"))))
      adminRegions <- st_as_sf(adminRegions)
      locLat <- regionInfoLookup[i, latitude]
      locLong <- regionInfoLookup[i, longitude]
      bb <- c(regionInfoLookup[i, ll.lat], regionInfoLookup[i, ll.lon], regionInfoLookup[i, ur.lat], regionInfoLookup[i, ur.lon])  
      xlim <- c( regionInfoLookup[i, ll.lon], regionInfoLookup[i, ur.lon])
      ylim <- c(regionInfoLookup[i, ll.lat], regionInfoLookup[i, ur.lat])
      regionExt <- ext(c(xlim, ylim))
      regionBox <- c(xmin = regionInfoLookup[i, ll.lon], ymin = regionInfoLookup[i, ll.lat], xmax = regionInfoLookup[i, ur.lon], ymax = regionInfoLookup[i, ur.lat])
      
      # pa0 <- protectedAreas0[protectedAreas0$PARENT_ISO == iso3Code,]
      # pa1 <- protectedAreas1[protectedAreas1$PARENT_ISO == iso3Code,]
      # pa2 <- protectedAreas2[protectedAreas2$PARENT_ISO == iso3Code,]
      #     world_region <- world[world$sov_a3 == iso3Code,]
      
      world_region <- createRegionSpatialData("world", regionBox)
      lakes_region <- createRegionSpatialData("lakes", regionBox)
      rivers_region <- createRegionSpatialData("rivers", regionBox)
      roads_region <- createRegionSpatialData("roads", regionBox)
      # cities_region <- createRegionSpatialData("cities", regionBox)
      print(system.time(adminRegions_region <- st_crop(adminRegions, regionBox)))
      populatedAreas_region <- createRegionSpatialData("populatedAreas", regionBox)
      # urbanAreas_region <- createRegionSpatialData("urbanAreas", regionBox)
      
      # code to get min and max values across all periods
      meanData <- c()
      meanData <- rast(paste0("data/cmip6/monthlyMean/monthlyMean_", j, "_observed_2001_2010.tif"))
      for (q in startyearChoices_ensemble) {
        yearSpan <- paste0(q, "_", q + yearRange)
        meanData <- c(meanData, rast(paste0("data/cmip6/monthlyMean/ensemblemonthlyMean_", j,  "_",  yearSpan, "_", k, ".tif")))
      }
      
      climVar_crop <- crop(meanData, regionExt)
      climVar_crop[is.nan(climVar_crop)] <- NA
      climVarMin <- min(global(climVar_crop, fun = "min", na.rm = TRUE))
      climVarMax <- max(global(climVar_crop, fun = "max", na.rm = TRUE))
      climVarMax <- ceiling(climVarMax)
      climVarMin <- floor(climVarMin)
      nbins <- 7
      custom_bins <- round(seq.int(from = climVarMin, to = climVarMax, length = nbins))
      
      legendTitle <- varNamesInfo[variableShortName %in% j, units]
      unit_category <- "metric" # for the scale bar
      if (iso3Code == "USA") { 
        unit_category <- "imperial" # for the scale bar
        if (j %in% c("tasmax", "tasmin", "tave")) {
          legendTitle <- "°F"
          custom_bins <- round(custom_bins * 9/5 + 32)
          climVarMax <- max(custom_bins)
          climVarMin <- min(custom_bins)
        }
        if (j %in% "pr") {
          legendTitle <- "in"
          custom_bins <- round(custom_bins * 0.039370)
          climVarMax <- max(custom_bins)
          climVarMin <- min(custom_bins)
        }
      }
      
      for (l in startyearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        if (l %in% startyearChoices[1]) {
          r_climVar <- paste0("data/cmip6/monthlyMean/monthlyMean_", j, "_observed_2001_2010.tif")
        } else {
          r_climVar <- paste0(fileLoc_monthlyMean, "ensembleMonthlyMean_", j, "_", yearSpan,"_", k, ".tif")
        }
        r_climVar <- rast(r_climVar)
        names(r_climVar) <- month.abb
        
        #        relief <- crop(shadedRelief, regionExt)
        print(system.time(r_climVar_region <- crop(r_climVar, regionExt)))
        r_climVar_region <- as.data.frame(r_climVar_region, xy = TRUE)
        r_climVar_region[month.abb] <- round(r_climVar_region[all_of(month.abb)], 2) 
        r_climVar_region$yearSpan <- yearSpan
        r_climVar_region$climVar <- j
        colorder_df <- c("climVar", "yearSpan", "x", "y", month.abb) # maybe should be all_of(month.abb)
        r_climVar_region <- r_climVar_region[colorder_df]
        names(r_climVar_region)[names(r_climVar_region) == "x"] <- "longitude"
        names(r_climVar_region)[names(r_climVar_region) == "y"] <- "latitude"
        r_climVar_region_long <- tidyr::pivot_longer(r_climVar_region, all_of(month.abb), names_to = "month", values_to = "value")
        r_climVar_region_long$month = factor(r_climVar_region_long$month, levels = all_of(month.abb))
        
        if (iso3Code == "USA") { 
          if (j %in% c("tasmax", "tasmin", "tave")) {
            r_climVar_region_long$value <- r_climVar_region_long$value * 9/5 + 32
          }
          if (j %in% "pr") {
            r_climVar_region_long$value <- r_climVar_region_long$value * 0.039370
            print(head(r_climVar_region_long))
          }
        }
        
        # if (l %in% startyearChoices[1]) {
        #   dataHolder <- as.data.table(r_climVar_region)
        #   setnames(dataHolder, old = c("x", "y", new = c("longitude", "latitude")))
        # } else {
        #   newColName <- paste0(j, " ", l)
        #   dataHolder[, c(newColName) := r_climVar_region$mean]
        # }
        # 
        
        titleText <- paste0("Monthly mean ", varNamesInfo[variableShortName %in% j, variableLongName], ", ", gsub("_", "-", yearSpan), ", ",  "scenario ", k, ", ", regionInfoLookup[i, ctyRegion])
        print(titleText)
        gc()
        colorList <- rev(RColorBrewer::brewer.pal(nbins, "RdYlBu"))
        g <- ggplot(data = adminRegions_region) +
          labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
          labs(x = "", y = "") +
          geom_raster(data = r_climVar_region_long, aes(longitude, latitude, fill = value)) + 
          # scale_fill_gradientn(colors = p, # name = legendText,
          #                      na.value = "grey50",
          #                      guide = "colorbar") +
          scale_fill_gradientn(colors = colorList, na.value = "transparent",
                               breaks=custom_bins,labels = custom_bins,
                               limits=c(climVarMin, climVarMax)) +
          geom_sf(fill = NA, color = "gray", lwd = 0.3) + 
          
          theme(axis.text.x=element_text(size=rel(0.6))) + #, angle=90
          theme(axis.text.y=element_text(size=rel(0.6)))
        
        # geom_sf(data = roads_region, color = "red", lwd = 0.3, fill = NA)
        if (nrow(populatedAreas_region) > 0) { 
          g <- g + # geom_sf(data = populatedAreas_region, color = "black", lwd = 0.5, fill = NA) +
            geom_sf_text(data = populatedAreas_region, aes(label = NAME), size = 1.5)
        }
        if (nrow(rivers_region) > 0) { 
          g <- g + geom_sf(data = rivers_region, aes(colour = "River"), show.legend = "line") +
            scale_color_manual(values = c("River" = "blue"), 
                               labels = c("Rivers"), name = "") 
          
        }
        g <- g + 
          # geom_point(aes(x = locLong, y= locLat), color="blue", size = .2, fill = NA) +
          # annotate("text", label = region, x = locLong + .35 , y= locLat, size = 2) +
          # geom_label(x = locLong + .35 , y= locLat, label = region, color = "black",
          #            fill = NA) 
          
          annotation_scale(location = "bl", width_hint = 0.5, height = unit(.3, "mm"), text_cex = 0.5, unit_category = unit_category) +
          # annotation_north_arrow(location = "bl", which_north = "true", 
          #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
          #                        style = north_arrow_minimal) +
          facet_wrap(vars(month))
        system.time(print(g))
        #      dev.off()
        outFilename <- paste0("graphics/cmip6/regionInfo/", j,"MonthlyAve_", k, "_", yearSpan, "_", regionInfoLookup[i, region], ".png")
        ggsave(outFilename, plot = g, device = "png", width = 6, height = 6)
        print(paste0("writing out ", outFilename))
        # ggsave("map_web.png", width = 6, height = 6, dpi = "screen")
        outFilename_csv <- paste0("data/regionResults/monthlyFacet_", j, "_", regionInfoLookup[i, region], "_", k, "_", yearSpan, ".csv")
        print(paste0("writing out ", outFilename_csv))
        write.csv(r_climVar_region, file = outFilename_csv, row.names = FALSE)  
      }
    }
  }
}

# region data with facets, say for months
source("R/globallyUsed.R")
library(rnaturalearthhires)
library(ggspatial)
library(sf)
library(dplyr)

#test vars
j <- "pr"
k <- "ssp585"
l <- 2001
i <- 1

locOfCMIP6ncFiles <- "data-raw/ISIMIP/cmip6/unitsCorrected/"
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startyearChoices_ensemble <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
yearRange <- 9
pal <- colorRampPalette(c("green","red"))
extentRange <- 2 # a value of 2 means 2 of the units of the raster; if it is 1/2 degree cells, this would be 1 degree
climateVars <- c("pr", "hurs", "tasmax", "tasmin", "tas")
fileLoc_monthlyMean <- "data/cmip6/monthlyMean/"

regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookup.xlsx", range = "A1:k7"))
#regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/CSVs2016_geocoordinates.xlsx", range = "F7:J47"))
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
  for (k in sspChoices) {
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
      regionBox <- c(xmin = regionInfoLookup[i, ll.lon], ymin = regionInfoLookup[i, ll.lat], xmax = regionInfoLookup[i, ur.lon], ymax = regionInfoLookup[i, ur.lat])
      
      pa0 <- protectedAreas0[protectedAreas0$PARENT_ISO == iso3Code,]
      pa1 <- protectedAreas1[protectedAreas1$PARENT_ISO == iso3Code,]
      pa2 <- protectedAreas2[protectedAreas2$PARENT_ISO == iso3Code,]
      #     world_region <- world[world$sov_a3 == iso3Code,]
      world_region <- createRegionSpatialData("world", regionBox)
      lakes_region <- createRegionSpatialData("lakes", regionBox)
      rivers_region <- createRegionSpatialData("rivers", regionBox)
      roads_region <- createRegionSpatialData("roads", regionBox)
      # cities_region <- createRegionSpatialData("cities", regionBox)
      populatedAreas_region <- createRegionSpatialData("populatedAreas", regionBox)
      # urbanAreas_region <- createRegionSpatialData("urbanAreas", regionBox)
      
      # code to get min and max values across all periods
      meanData <- c()
      meanData <- rast(paste0("data/cmip6/monthlyMean/monthlyMean_", j, "_observed_2001_2010.tif"))
      for (l in startyearChoices_ensemble) {
        yearSpan <- paste0(l, "_", l + yearRange)
        meanData <- c(meanData, rast(paste0("data/cmip6/monthlyMean/ensemblemonthlyMean_", j,  "_",  yearSpan, "_", k, ".tif")))
      }
      
      climVar_crop <- crop(meanData, regionExt)
      climVarMin <- min(global(climVar_crop, fun = "min", na.rm = TRUE))
      climVarMax <- max(global(climVar_crop, fun = "max", na.rm = TRUE))
      climVarMax <- ceiling(climVarMax)
      climVarMin <- floor(climVarMin)
      
      custom_bins <- round(seq.int(from = climVarMin, to = climVarMax, length = 6))
      
      # from https://stackoverflow.com/questions/37376398/how-to-create-an-empty-datatable-with-columns-names-and-then-append-datatables-t
      #      dataHolder <- data.table(1)[,`:=`(c(j, "yearSpan", "x", "y", month.abb),NA)][,V1:=NULL][.0]
      for (l in startyearChoices) { 
        yearSpan <- paste0(l, "_", l + yearRange)
        if (l == 2001) {
          r_climVar <- paste0("data/cmip6/monthlyMean/monthlyMean_", j, "_observed_2001_2010.tif")
        } else {
          r_climVar <- paste0(fileLoc_monthlyMean, "ensembleMonthlyMean_", j, "_", yearSpan,"_", k, ".tif")
        }
        r_climVar <- rast(r_climVar)
        
        #        relief <- crop(shadedRelief, regionExt)
        r_climVar_region <- crop(r_climVar, regionExt)
        r_climVar_region <- as.data.frame(r_climVar_region, xy = TRUE)
        r_climVar_region[month.abb] <- round(r_climVar_region[month.abb], 2)
        r_climVar_region$yearSpan <- yearSpan
        r_climVar_region$climVar <- j
        colorder_df <- c("climVar", "yearSpan", "x", "y", month.abb)
        r_climVar_region <- r_climVar_region[colorder_df]
        names(r_climVar_region)[names(r_climVar_region) == "x"] <- "longitude"
        names(r_climVar_region)[names(r_climVar_region) == "y"] <- "latitude"
        
        gatherCols <- tidyselect::all_of(month.abb)
        valueCol <- "value"
        keyCol <- j
        r_climVar_region_long <- tidyr::gather(r_climVar_region, valueCol, keyCol, gatherCols, factor_key=TRUE)
        
        
        # if (l %in% startyearChoices[1]) {
        #   dataHolder <- as.data.table(r_climVar_region)
        #   setnames(dataHolder, old = c("x", "y", new = c("longitude", "latitude")))
        # } else {
        #   newColName <- paste0(j, " ", l)
        #   dataHolder[, c(newColName) := r_climVar_region$mean]
        # }
        # 
        
        titleText <- paste0("Monthly mean ", varNamesInfo[variableShortName %in% j, variableLongName], ", ", gsub("_", "-", yearSpan), ", ",  "scenario ", k, ", ", regionInfoLookup[i, ctyRegion])
        legendTitle <- varNamesInfo[variableShortName %in% j, units]
        print(titleText)
        
        g <- ggplot(data = world_region) +
          labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
          labs(x = "", y = "") +
          geom_raster(data = r_climVar_region_long, aes(longitude, latitude, fill = keyCol)) + 
          # scale_fill_gradientn(colors = p, # name = legendText,
          #                      na.value = "grey50",
          #                      guide = "colorbar") +
          scale_fill_gradientn(colours = topo.colors(7), na.value = "transparent",
                               breaks=custom_bins,labels = custom_bins,
                               limits=c(climVarMin, climVarMax)) +
 #         geom_sf(fill = NA, color = "gray") + 
          theme(axis.text.x=element_text(size=rel(0.6))) + #, angle=90
          theme(axis.text.y=element_text(size=rel(0.6))) +
          
          geom_sf(data = roads_region, color = "red", lwd = 0.3, fill = NA)
        # if (nrow(populatedAreas_region) > 0) { 
        #   g <- g + geom_sf(data = populatedAreas_region, color = "black", lwd = 0.5, fill = NA) +
        #     geom_sf_label(data = populatedAreas_region, aes(label = NAME), size = 2, fill = NA)
        # }
        # if (nrow(rivers_region) > 0) { 
        #   g <- g + geom_sf(data = rivers_region, color = "blue", lwd = 0.5, fill = NA) 
        # }
        g <- g + geom_point(aes(x = locLong, y= locLat), colour="blue", size = .2, fill = NA) +
          annotate("text", label = region, x = locLong + .35 , y= locLat, size = 3) +
          # geom_label(x = locLong + .35 , y= locLat, label = region, color = "black",
          #            fill = NA) 
          
          annotation_scale(location = "bl", width_hint = 0.5, height = unit(.2, "mm"), text_cex = 0.3,) +
          # annotation_north_arrow(location = "bl", which_north = "true", 
          #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
          #                        style = north_arrow_minimal) +
          facet_wrap(vars(valueCol))
        print(g)
        dev.off()
        outFilename <- paste0("graphics/cmip6/regionInfo/", j,"MonthlyAve_", k, "_", yearSpan, "_", regionInfoLookup[i, region], ".png")
        unlink(outFilename)
        ggsave(outFilename, plot = last_plot(), device = "png", width = 6, height = 6, overwrite = TRUE)
        # ggsave("map_web.png", width = 6, height = 6, dpi = "screen")
        unlink(outFilename_csv)
        outFilename_csv <- paste0("data/regionResults/monthlyFacet_", j, "_", regionInfoLookup[i, region], "_", k, "_", yearSpan, ".csv")
        print(paste0("writing out ", outFilename_csv))
        write.csv(r_climVar_region, file = outFilename_csv, row.names = FALSE)  
      }
    }
  }
}

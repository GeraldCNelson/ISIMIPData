# calculate 1-0 values for locations where chill portions are enough, by fruit
{library(terra)
  library(sf)
  library("crayon")
  library(ggplot2)
  fileLoc <- "data/cmip6/chillPortions/chill_portions/"
  speciesChoice <- c("cherry", "almond", "winegrape", "apple") #, "olive", "berries") 
  fruitCPs <- readxl::read_excel("data-raw/crops/fruitCPs.xlsx")
  sspChoices <- c("ssp126", "ssp585") 
  modelChoices <- c( "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM6A-LR") #, "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
  modelChoices_lower <- tolower(modelChoices)
  startYearChoices <-  c(2041, 2081) 
  hemisphere <- c("NH", "SH")
  extent_NH <- c( -180, 180, 0, 90)
  extent_SH <-c( -180, 180, -60, 0) #-60 gets rid of Antarctica
  
  yearRange <- 19
  woptList <- list(gdal=c("COMPRESS=LZW"))
  woptList <- list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL = 6"))
  
  coastline <- st_read("data-raw/regionInformation/ne_50m_coastline/ne_50m_coastline.shp")
  
  #function to get rid of Antarctica
  crop_custom <- function(poly.sf) {
    poly.sp <- as(poly.sf, "Spatial")
    extR <- raster::extent(c(-180, 180, -60, 90))
    poly.sp.crop <- crop(poly.sp, extR)
    st_as_sf(poly.sp.crop)
  }
  coastline <- crop_custom(coastline)
  
  RobinsonProj <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  crsRob <- RobinsonProj
  coastline <- st_transform(coastline, crsRob)
  
  #test values
  i <- "UKESM1-0-LL"
  k <- "ssp585"
  l <- 2041
  m <- "NH"
  n <- "cherry"
  
  readRast_ensemble <- function(i) {
    if (m == "NH") hem <- "north"
    if (m == "SH") hem <- "south"
    fileName_in <- paste0("data/cmip6/chillPortions/chill_portions/", k,"/", i, "/", k, "_", i, "_", midYear, "_chill_portions_", hem, ".tif")
    print(paste0("fileName in: ", fileName_in))
    r <- rast(fileName_in)
    system.time(chillPortion <- app(r, fun = quantile, probs=0.1, na.rm = TRUE)) # means not adequate chill portions probs percent of the time
    print(chillPortion)
    chillPortion <- crop(chillPortion, get(paste0("extent_", m)))
  }
  
  chillPortionsFun <- function() {
    ext_hem <- get(paste0("extent_", m))
    print(paste("hemisphere: ", m, " extent: ", ext_hem))
    system.time(x <- lapply(modelChoices_lower, readRast_ensemble))
    SWC <- rast(x)
    SWC
    # now do ensemble
    for (n in speciesChoice) {
      print(paste0("working on ssp: ", k, ", start year ", l, ", hemisphere ", m, ", crop ", n))
      cplimit <- fruitCPs$chillRequirement[fruitCPs$crop %in% n]
      r <- SWC
      
      maxVal <- round(max(minmax(r)), 2)
      minVal <- round(min(minmax(r)), 2)
      #     print(r)
      fileName_out <- paste0(fileLoc, "ensemble_chill_cutoff_", n, "_", k, "_", m, "_", yearSpan, ".tif")
      print(system.time(r.mean <- app(r, fun = "mean", na.rm = TRUE)))
      r.mean[r.mean < cplimit] <- 0
      r.mean[r.mean > cplimit] <- 1
      r.mean <- crop(r.mean, ext_hem)
      print(system.time(writeRaster(r.mean, filename = fileName_out, overwrite = TRUE, woptList = woptList)))
      cat(paste0(red("species: ", n, ", ensemble ssp: ", k, ", start year: ", l, ", minVal ", minVal,  ", maxVal ", maxVal, ", fileName out: ", fileName_out), "\n\n"))
      print(paste0("extent: ", r.mean))
      r.mean
    }
  }
}
# chill portions -----
for (k in sspChoices) {
  #  k = "ssp585"
  for (l in startYearChoices) {
    # l <- 2041
    midYear <- l + 9
    yearSpan <- paste0(l, "_", l + yearRange)
    for (m in hemisphere) {
      chillPortionsFun()
    }
  }
}

# chill portions, historical -----
k <- "historical"
l = 1991
midYear <- l + 9
yearSpan <- paste0(l, "_", l + yearRange)
for (m in hemisphere) {
  chillPortionsFun()
}
# graphics -----
ext_noAntarctica <- ext(-180, 180, -60, 90)

chillPortionsGraphFun <- function() {
  for (n in speciesChoice) {
    fileName_in_NH <- paste0(fileLoc, "ensemble_chill_cutoff_", n, "_", k, "_", "NH", "_", yearSpan, ".tif")
    fileName_in_SH <- paste0(fileLoc, "ensemble_chill_cutoff_", n, "_", k, "_", "SH", "_", yearSpan, ".tif")
    r_NH <- rast(fileName_in_NH)
    r_SH <- rast(fileName_in_SH)
    r <- merge(r_NH, r_SH)
    r <- crop(r, ext_noAntarctica)
    r <- project(r, crsRob)
    
    r_df <- as.data.frame(r, xy = TRUE)
    names(r_df) <- c("x", "y", "value")
    titleText <- paste0("species: ", n, ", scenario:  ", k,  ", year span: ", gsub("_", "-", yearSpan))
    legendTitle <- "Adequate chill portions"
    #        colorList <- (RColorBrewer::brewer.pal(2, "YlOrRd"))
    colorList <- c("white", "green")
    #    custom_bins = c(0, 1)
    g <- ggplot(data = coastline) +
      labs(title = titleText, fill = legendTitle) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
      labs(x = "", y = "") +
      geom_tile(data = r_df, aes(x, y, fill = value), show.legend = FALSE) +
      scale_fill_gradientn(colours=c("white","green")) +
      geom_sf(color = "ghostwhite", lwd = 0.2) +
      theme(panel.background = element_rect(fill = "aliceblue"))
    #          theme(legend.text.align = 1) +
    #   theme(legend.position = "none")
    fileName_out <- paste0("graphics/cmip6/chillPortions/adeqChillPortions_", n, "_", k, "_", yearSpan, ".png")
    
    ggsave(filename = fileName_out, plot = g, width = 6, height = 6, units = "in", dpi = 300)
    knitr::plot_crop(fileName_out)
    print(paste0("out file name: ", fileName_out))
    g <- NULL
  }
}

for (k in sspChoices) {
  #  k = "ssp585"
  for (l in startYearChoices) {
    # l <- 2041
    midYear <- l + 9
    yearSpan <- paste0(l, "_", l + yearRange)
    chillPortionsGraphFun()
  }
}

k = "historical"
l <- 1991
midYear <- l + 9
yearSpan <- paste0(l, "_", l + yearRange)
chillPortionsGraphFun()

# chillPortions ppt -----
library(officer)
library(flextable)
library(magrittr)

defaultWidth <- 10
defaultHeight <- 5
 defaultLeft <- 0
defaultTop <- 1
# defaultTopSH <- 4

chillportionsPptFun <- function(fruit) {
  fileNameStart <- paste0("adeqChillPortions_")
  fileName_in <- paste0("graphics/cmip6/chillPortions/", fileNameStart, fruit, "_", k, "_", yearSpan, ".png")
  extImg_cp <- external_img(src = fileName_in, width = defaultWidth, height = defaultHeight)
  my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
  my_pres <- ph_with(x = my_pres, value = extImg_cp, location = ph_location(left = defaultLeft, top = defaultTop, width = defaultWidth, height = defaultHeight - 0.5) )
  return(my_pres)
}

library(readxl)
fileName_fruitCPs <- paste0("data-raw/crops/", "fruitCPs.xlsx")
CPs <- read_excel(fileName_fruitCPs)

# presentation intro -----
titleString <- paste0("Adequate Chill Portions by Fruit, Time Period, and Scenario")
contentString <- paste0("Powerpoint produced on ", Sys.Date())

introText1 <- "These slides display locations where chill portions are adequate 90% of the time of various perennial fruits. "
introText2 <- "The table below shows the crops we’re considering and the chill portion requirements used in the following graphs. "

#dataText5 <- "Crop area is based on the SAGE cropping calendar data set, based in the early 2000s. Areas that are not cropped have an NA value and are displayed in white. Areas in gray have chilling less than the minimum requirements. Areas in yellow have chilling hours between the lower and upper range of the requirements."
dataText <- c(dataText1, dataText2, dataText3, dataText4) #, dataText5)

fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
fp_2 <- fp_text(bold = FALSE, font.size = 12)
fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)

blIntro <- block_list(
  fpar(
    ftext(introText1, fp_2),
    ftext(introText2, fp_2)
  ))

my_pres <- read_pptx()
my_pres <- add_slide(x = my_pres, layout = 'Title Slide', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = titleString, location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(x = my_pres, value = contentString, location = ph_location_type(type = "subTitle"))

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, head(CPs), location = ph_location(left = 2.5, top = 2.5, width = 4, height = 3) )

my_pres <- ph_with(x = my_pres, value = blIntro, location = ph_location_type(type = "body") )

# presentation for loop -----
#browser()
for (fruit in speciesChoice) {
  ensembleTitle <- paste("Adequate Chill Portions for ", fruit)
  my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
  my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
  
  # do historical first, then ssps and future periods
  k <- "historical"
  l <- 1991
  yearSpan <- paste0(l, "_", l + yearRange)
  chillportionsPptFun(fruit)
  
  for (k in sspChoices) {
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      chillportionsPptFun(fruit)
    }
  }
}

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Data Source", location = ph_location_type(type = "title"))
my_pres <- f_addDataSlide()

print(my_pres, target = "presentations/cmip6/perennials/adequateChillPortions.pptx") %>% browseURL()

#print(my_pres, target = paste0("presentations/cmip6/chillingHours/summerHeatandSpringFrost.pptx"))



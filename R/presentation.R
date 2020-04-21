# script to create powerpoints
source("R/globallyUsed.R")
library(officer)
library(magrittr)
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9

# pdfDimensions used to size the images to a particular page size for the nutrient modeling paper. Probably not needed here.
# pdfDimensions <- getNewestVersion("pdfDimensions", fileloc("gDir"))
# pdfDimensions[, width := as.numeric(width)]
# pdfDimensions[, height := as.numeric(height)]
# 
# pdfDimensions[, fileName := gsub(imageFilePrefix, "", fileName)]
# imageList <- paste0("imageList_", gdxChoice, "")
# pdfDimensions <- pdfDimensions[fileName %in% get(imageList)]
# 
# pdfDimensions[, newOrder := match(fileName, get(imageList))]
# setorder(pdfDimensions, newOrder)
# pdfDimensions[, newOrder := NULL]
# 
# # add path back onto filename
# pdfDimensions[, fileName := paste0(imageFilePrefix, fileName)]

IPCC_WG2_Ch5_crop_temperature_table <- read_excel("/Volumes/Extreme SSD/crops/Crop_temperature_table_summary_07042020.xlsx")


# big loop across all crops and time periods
for (m in 1:nrow(IPCC_WG2_Ch5_crop_temperature_table)) {
  cropName <- as.character(IPCC_WG2_Ch5_crop_temperature_table[m, "crop"])
  tdamage_mean <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "tdamage mean"])
  
  titleString <- paste0("Mapping locations with damaging temperatures for ", cropName)
  contentString <- paste0("Average number of days by month with maximum temperature above damage level, powerpoint produced ", Sys.Date())
  my_pres <- read_pptx() %>% 
    add_slide(layout = 'Title Slide', master = 'Office Theme')  %>% 
    ph_with(value = titleString, location = ph_location_type(type = "ctrTitle")) %>% 
    ph_with(value = contentString, location = ph_location_type(type = "subTitle"))
  
  for (i in modelChoices) {
    print(i)
    modelTitle <- gsub("_", " ", i)
    add_slide(my_pres, layout = 'Section Header', master = 'Office Theme')  %>% 
      ph_with(value = modelTitle, location = ph_location_type(type = "body"))
    
    for (k in sspChoices) {
      for (l in startyearChoices) {
        print(paste0(cropName, ", damage temp: ", tdamage_mean, ", start year: ", l))
        
        modelName.lower <- tolower(i)
        yearSpan <- paste0(l, "_", l + yearRange)
        filler <- fixFiller(i)
        plotFileName <- paste0("graphics/cmip6/damageTemp/tdamage_ensembleMean_masked_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".jpg")
        if (l == 2001) {
          plotFileName <- paste0("graphics/cmip6/damageTemp/tdamage_mean_", cropName, "_", tdamage_mean, "C", "_observed_", yearSpan, ".jpg")
        }
        print(plotFileName)
        extImg <- external_img(src = plotFileName, width = 8, height = 8)
        #        my_pres <- add_slide(my_pres, layout = 'Two Content', master = 'Office Theme')
        #       cropInfoString <- paste0(Crop)
        #        my_pres <- ph_with(my_pres, value = cropInfoString, location = ph_location_type(type = "ctrTitle"))
        
        add_slide(my_pres, layout = 'Comparison', master = 'Office Theme') %>% 
          ph_with(value = extImg, location = ph_location_fullsize(),  use_loc_size = FALSE )
      }
    }
  }
}

# do ppt for ensemble means and SDs
titleString <- paste0("Ensemble Means and SDs for 13 crops")
contentString <- paste0("Ensemble means and standard deviations: Average number of days by month with maximum temperature above damage level, powerpoint produced on ", Sys.Date())
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data
my_pres <- read_pptx() %>% 
  add_slide(layout = 'Title Slide', master = 'Office Theme')  %>% 
  ph_with(value = titleString, location = ph_location_type(type = "ctrTitle")) %>% 
  ph_with(value = contentString, location = ph_location_type(type = "subTitle"))

IntroText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data." 
IntroText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/)."
IntroText3 <- "It includes data from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three scenarios (ssp126, ssp370 and ssp585). In this powerpoint, only results using ssp585 are presented." 
IntroText4 <- "The data from a 10 year period for the individual models are averaged for each month and a standard deviation across the 5 models is calculated."

IntroText <- c(IntroText1, IntroText2, IntroText3, IntroText4)

def_text <- fp_text(color = "black", italic = FALSE, font.size = 15)
TITLE = fpar(ftext(paste0(IntroText, collapse = " "), prop = def_text))
TITLE <- update(TITLE, fp_p = fp_par(text.align = "left"))

lastPhId <- function(presentation) {
  index = presentation$cursor
  x <- slide_summary(presentation, index = index)
  x <- x[x$type == "body", ]
  max(as.numeric(x$id))
}

my_pres %<>% add_slide(layout = "Title and Content", master = "Office Theme")
my_pres %<>% ph_with(value = "Introduction", location = ph_location_type(type = "title"))
my_pres %<>% ph_empty_at(left = 1, top = 2, height = 1, width = 8, bg = "white")
my_pres %<>% ph_add_fpar(value = TITLE, id_chr = lastPhId(my_pres), par_default = FALSE)

add_slide(my_pres, layout = 'Title and Content', master = 'Office Theme')  %>% 
  ph_with(value = "Introduction", location = ph_location_type(type = "title")) %>% 
  ph_with(value = paste0(IntroText, collapse = " "), location = ph_location_type(type = "body"))

for (m in 1:nrow(IPCC_WG2_Ch5_crop_temperature_table)) {
  cropName <- as.character(IPCC_WG2_Ch5_crop_temperature_table[m, "crop"])
  tdamage_mean <- as.numeric(IPCC_WG2_Ch5_crop_temperature_table[m, "tdamage mean"])
  for (k in sspChoices) {
    
    ensembleTitle <- paste("Ensemble Mean and Standard Deviation for", cropName)
    add_slide(my_pres, layout = 'Section Header', master = 'Office Theme')  %>% 
      ph_with(value = ensembleTitle, location = ph_location_type(type = "body"))

    fileNameObserved <- paste0("graphics/cmip6/damageTemp/tdamage_mean_masked_", cropName, "_", tdamage_mean, "C_observed_",  "2001_2010", ".jpg")
    print(paste0("fileNameObserved: ", fileNameObserved))
    extImgObs <- external_img(src = fileNameObserved, width = 5, height = 8)
    add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
      ph_with(value = extImgObs, location = ph_location(left = 0, top = 0, width = 5, height = 8) )
    
    for (l in startyearChoices_ensemble) {
      yearSpan <- paste0(l, "_", l + yearRange)
      
      fileNameMean <- paste0("graphics/cmip6/damageTemp/tdamage_ensembleMean_masked_", cropName, "_", tdamage_mean, "C_", k, "_", yearSpan, ".jpg")
      fileNameSD <- paste0("graphics/cmip6/damageTemp/tdamage_ensembleSD_masked_", cropName, "_", tdamage_mean, "C_",  k, "_", yearSpan, ".jpg")
      print(paste0("fileNameMean: ", fileNameMean))
      print(paste0("fileNameSD: ", fileNameSD))
      
      extImgMean <- external_img(src = fileNameMean, width = 5, height = 8)
      extImgSD <- external_img(src = fileNameSD, width = 5, height = 8)
      
      #   add_slide(my_pres, layout = 'Comparison', master = 'Office Theme') %>% 
      #     ph_with(value = extImgMean, location = ph_location_left(),  use_loc_size = FALSE ) %>%
      # #  add_slide(my_pres, layout = 'Comparison', master = 'Office Theme') %>% 
      #     ph_with(value = extImgSD, location = ph_location_right(),  use_loc_size = FALSE )
      
      
      add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
        ph_with(value = extImgMean, location = ph_location(left = 0, top = 0, width = 5, height = 8) ) %>%
        ph_with(value = extImgSD, location = ph_location(left = 5, top = 0, width = 5, height = 8) )
      print(paste0("m: ", m))
    }
  }
}

print(my_pres, target = "damageTemp_Ensemble.pptx") %>% browseURL()

print(my_pres, target = paste0("presentations/cmip6/damageTemp_Ensemble", ".pptx"))




# script to create powerpoints for THI results
source("R/globallyUsed.R")
library(officer)
library(magrittr)
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

yearRange <- 9
modelChoices.lower <- tolower(modelChoices)
#test values
k <- "ssp585"
l <- 2021

thiList <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.swine")

# do ppt for ensemble means and CVs
thiListReduced <- thiList[!thiList %in% c("thi.yak", "thi.broiler", "thi.layer")]

titleString <- paste0("Global Effects to 2100 of Temperature and Humidity on Productivity of ", length(thiListReduced), " Animal Species")
contentString <- paste0("Preliminary Results: Monthly ensemble means and coefficients of variation by species for four time periods to 2100. Powerpoint produced on ", Sys.Date())
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

my_pres <- read_pptx() %>% 
  add_slide(layout = 'Title Slide', master = 'Office Theme')  %>% 
  ph_with(value = titleString, location = ph_location_type(type = "ctrTitle")) %>% 
  ph_with(value = contentString, location = ph_location_type(type = "subTitle"))

IntroText0 <- "Animal productivity is affected by exposure to combined high levels of temperature and humidity. The THI (temperature and humidity index) is a species-specific measure of those effects with thresholds for low, medium and high negative productivity effects.  "
IntroText0.5 <- "In the following figures 4 colors represent areas of different levels of stress to a particular species. Areas with one of these colors is where the animals were grown in the early 2000s."
IntroText1 <- "The climate data set used in these graphics of average monthly THI values was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
IntroText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/). "
IntroText3 <- "It includes data from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three scenarios (ssp126, ssp370 and ssp585). "
IntroText3.5 <- "In this powerpoint, only results using ssp585 are presented." 
IntroText4 <- "The THI values are for 10 year periods (2001-2010, 2021-2030, 2051-2060, and 2091-2100) with results for the individual models averaged for each month and a coefficient of variation across the 5 models is calculated."
IntroText5 <- "This powerpoint presents work in progress and should not be circulated without permission."
fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
fp_2 <- fp_text(bold = FALSE, font.size = 12)
fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)

bl <- block_list(
  fpar(
    ftext(IntroText0, fp_2),
    ftext(IntroText0.5, fp_2)),
  fpar(),
  fpar(
    ftext(IntroText1, fp_2),
    ftext(IntroText2, fp_2),
    ftext(IntroText3, fp_2)),
  fpar(),
  fpar(
    ftext(IntroText3.5, fp_2)),
  fpar(
    ftext(IntroText4, fp_2)),
  fpar(),
  fpar(
    ftext(IntroText5, fp_2))
)


# def_text <- fp_text(color = "black", italic = FALSE, font.size = 15)
# TITLE = fpar(ftext(paste0(IntroText, collapse = " "), prop = def_text))
# TITLE <- update(TITLE, fp_p = fp_par(text.align = "left"))
# 
# lastPhId <- function(presentation) {
#   index = presentation$cursor
#   x <- slide_summary(presentation, index = index)
#   x <- x[x$type == "body", ]
#   max(as.numeric(x$id))
# }

# my_pres %<>% add_slide(layout = "Title and Content", master = "Office Theme")
# my_pres %<>% ph_with(value = "Introduction", location = ph_location_type(type = "title"))
# #my_pres %<>% ph_empty_at(left = 1, top = 2, height = 1, width = 8, bg = "white")
# my_pres %<>% ph_add_fpar(value = TITLE, id_chr = lastPhId(my_pres), par_default = FALSE)

# add_slide(my_pres, layout = 'Title and Content', master = 'Office Theme')  %>% 
#   ph_with(value = "Introduction", location = ph_location_type(type = "title")) %>% 
#   ph_with(value = paste0(IntroText, collapse = " "), location = ph_location_type(type = "body"))

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = bl, location = ph_location_type(type = "body") )

for (k in sspChoices) {
  yearSpan <- paste0(l, "_", l + yearRange)
  print(paste0("ssp choice: ", k, ", start year: ", l))
  for (j in 1:length(thiListReduced)) {
    speciesName <- gsub("thi.", "", thiListReduced[j])
    ensembleTitle <- paste("Ensemble Mean and Coefficient of Variation for", speciesName)
    add_slide(my_pres, layout = 'Section Header', master = 'Office Theme')  %>% 
      ph_with(value = ensembleTitle, location = ph_location_type(type = "body"))
    
    fileNameCts <- paste0("graphics/cmip6/THI/counts_", speciesName, ".jpg")
    extImgObs <- external_img(src = fileNameCts, width = 5, height = 8)
    
    add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
      ph_with(value = extImgObs, location = ph_location(left = 2, top = 0, width = 5, height = 8) )
    
    fileNameObserved <- paste0("graphics/cmip6/THI/masked_", speciesName, "_observed_",  "2001_2010", ".jpg")
    
    extImgObs <- external_img(src = fileNameObserved, width = 5, height = 8)
    add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
      ph_with(value = extImgObs, location = ph_location(left = 0, top = 0, width = 5, height = 8) )
    
    for (l in startyearChoices_ensemble) {
      yearSpan <- paste0(l, "_", l + yearRange)
      fileNameCV <- paste0("graphics/cmip6/THI/THI_ensembleCV_masked_",   speciesName, "_",  yearSpan, "_", k, ".jpg")
      fileNameMean <- paste0("graphics/cmip6/THI/THI_ensembleMean_masked_",  speciesName, "_",  yearSpan, "_", k, ".jpg")
      
      extImgMean <- external_img(src = fileNameMean, width = 5, height = 8)
      extImgCV <- external_img(src = fileNameCV, width = 5, height = 8)
      
      #   add_slide(my_pres, layout = 'Comparison', master = 'Office Theme') %>% 
      #     ph_with(value = extImgMean, location = ph_location_left(),  use_loc_size = FALSE ) %>%
      # #  add_slide(my_pres, layout = 'Comparison', master = 'Office Theme') %>% 
      #     ph_with(value = extImgCV, location = ph_location_right(),  use_loc_size = FALSE )
      
      
      add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
        ph_with(value = extImgMean, location = ph_location(left = 0, top = 0, width = 5, height = 8) ) %>%
        ph_with(value = extImgCV, location = ph_location(left = 5, top = 0, width = 5, height = 8) )
    }
  }
}

print(my_pres, target = "presentations/cmip6/THI/damageTemp_Ensemble.pptx") %>% browseURL()

#print(my_pres, target = paste0("presentations/cmip6/THI/damageTemp_Ensemble", ".pptx"))

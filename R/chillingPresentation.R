# script to create powerpoints for chilling hours results
source("R/globallyUsed.R")
library(officer)
library(magrittr)
sspChoices <- c("ssp585") #"ssp126", 
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startyearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)

chillRequirements <- as.data.table(read_excel("data-raw/crops/chillRequirements.xlsx", range = "a1:e16"))

defaultWidth = 5
defaultHeight = 4
defaultLeft = 0

yearRange <- 9
modelChoices.lower <- tolower(modelChoices)
#test values
k <- "ssp585"
l <- 2021
j = 1

# do ppt for ensemble means and CVs
titleString <- paste0("Ensemble Means and CVs for chilling hours for ", length(fruits), " perennial fruits")
contentString <- paste0("Ensemble means and coefficients of variation: Average chilling hours, powerpoint produced on ", Sys.Date())
startyearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

my_pres <- read_pptx()
my_pres <- add_slide(x = my_pres, layout = 'Title Slide', master = 'Office Theme')
my_pres <- ph_with(x = my_pres, value = titleString, location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(x = my_pres, value = contentString, location = ph_location_type(type = "subTitle"))

IntroText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
IntroText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/). "
IntroText3 <- "It includes data from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three scenarios (ssp126, ssp370 and ssp585). In this powerpoint, only results using ssp585 are presented. " 
IntroText4 <- "The data from a 10 year period for the individual models are averaged for each month and a coefficient of variation across the 5 models is calculated. "
IntroText5 <- "Crop area is based on the SAGE cropping calendar data set, based in the early 2000s. Areas that are not cropped have an NA value and are displayed in white. Areas in gray have chilling less than the minimum requirements. Areas in yellow have chilling hours between the lower and upper range of the requirements."
IntroText <- c(IntroText1, IntroText2, IntroText3, IntroText4, IntroText5)

fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
fp_2 <- fp_text(bold = FALSE, font.size = 12)
fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)

bl <- block_list(
  #  fpar(ftext("hello world", fp_1)),
  fpar(
    ftext(IntroText1, fp_2),
    ftext(IntroText2, fp_2),
    ftext(IntroText3, fp_2),
    ftext(IntroText4, fp_2),
    ftext(IntroText5, fp_2)
  ))

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = bl, location = ph_location_type(type = "body") )

for (k in sspChoices) {
  print(paste0("ssp choice: ", k, ", start year: ", l))
  for (j in 1:length(fruits)) {
    
    cropName <- fruits[j]
    chillHoursRange <- chillRequirements[crop %in% cropName, common_varieties]
    minChillHours <- as.numeric(unlist(strsplit(chillHoursRange, split = "-"))[1])
    maxChillHours <- as.numeric(unlist(strsplit(chillHoursRange, split = "-"))[2])
    
    print(cropName)
    # get observed images for both southern and northern hemisphere
    fileNameObserved_SH <- paste0("graphics/cmip6/chillingHours/chillHrs_SouthernHem_masked_", cropName, "_observed_", "2001_2010", ".png")
    extImgObs_SH <- external_img(src = fileNameObserved_SH, width = defaultWidth, height = defaultHeight)
    fileNameObserved_NH <- paste0("graphics/cmip6/chillingHours/chillHrs_NorthernHem_masked_", cropName, "_observed_", "2001_2010", ".png")
    extImgObs_NH <- external_img(src = fileNameObserved_NH, width = defaultWidth, height = defaultHeight)
    
    ensembleTitle <- paste("Ensemble mean and coefficient of variation for", cropName)
    my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
    my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
    
    my_pres <- add_slide(x = my_pres, layout = 'Title and Content', master = 'Office Theme')
    titleTextObserved <- paste0("Mean chilling hours, 2001-2010, ", cropName, "; Required chilling hours - min. ", minChillHours, ", max. ", maxChillHours)
    my_pres <- ph_with( x = my_pres, value = fpar(ftext(titleTextObserved, fp_3)), location = ph_location_type(type = "title") )
   
    # my_pres <- ph_with(x = my_pres, value = extImgObs_NH, location = ph_location_type(type = "body"),
    #         use_loc_size = FALSE )
    my_pres <- ph_with( x = my_pres, value = extImgObs_NH, location = ph_location(left = defaultLeft, top = 1.2, width = 5, height = 4 ))
    my_pres <- ph_with( x = my_pres, value = extImgObs_SH, location = ph_location(left = defaultLeft, top = 4.2, width = 5, height = 4 ))
    
    for (l in startyearChoices_ensemble) {
      yearSpan <- paste0(l, "_", l + yearRange)
      fileNameCV_NH <- paste0("graphics/cmip6/chillingHours/chillHrs_NorthernHem_ensembleCV_masked_", cropName, "_", yearSpan, "_", k, ".png")
      fileNameMean_NH <-  paste0("graphics/cmip6/chillingHours/chillHrs_NorthernHem_ensembleMean_masked_", cropName, "_", yearSpan, "_", k, ".png")
      extImgMean_NH <- external_img(src = fileNameMean_NH, width = defaultWidth, height = defaultHeight)
      extImgCV_NH <-   external_img(src = fileNameCV_NH,   width = defaultWidth, height = defaultHeight)
      fileNameCV_SH <- paste0("graphics/cmip6/chillingHours/chillHrs_SouthernHem_ensembleCV_masked_", cropName, "_", yearSpan, "_", k, ".png")
      fileNameMean_SH <- paste0("graphics/cmip6/chillingHours/chillHrs_SouthernHem_ensembleMean_masked_", cropName, "_", yearSpan, "_", k, ".png")
      extImgMean_SH <- external_img(src = fileNameMean_SH, width = defaultWidth, height = defaultHeight)
      extImgCV_SH <-   external_img(src = fileNameCV_SH, width = defaultWidth, height = defaultHeight)
      
      my_pres <- add_slide(x = my_pres, layout = 'Title Only', master = 'Office Theme')
      titleTextFuture <- paste0("Ensemble mean and CoV chilling hours, ", k, ", ", yearSpan, ", ", cropName , "; Required chilling hours - min. ", minChillHours, ", max. ", maxChillHours)
      
      my_pres <- ph_with( x = my_pres, value = fpar(ftext(titleTextFuture, fp_3)), location = ph_location_type(type = "title") )
      my_pres <- ph_with(x = my_pres, value = extImgMean_NH, location = ph_location(left = defaultLeft, top = 1.2, width = 5, height = 4) )
      my_pres <- ph_with(x = my_pres, value = extImgMean_SH, location = ph_location(left = defaultLeft, top = 4.2, width = 5, height = 4) )
      my_pres <- ph_with(x = my_pres, value = extImgCV_NH, location = ph_location(left = 5, top = 1.2, width = 5, height = 4))
      my_pres <- ph_with(x = my_pres, value = extImgCV_SH, location = ph_location(left = 5, top = 4.2, width = 5, height = 4))
      
    }
  }
}

print(my_pres, target = "presentations/cmip6/chillingHours/chillHours_Ensemble.pptx") %>% browseURL()

print(my_pres, target = paste0("presentations/cmip6/chillingHours/chillHours_Ensemble", ".pptx"))




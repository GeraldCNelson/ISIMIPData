# script to create powerpoints for THI results
source("R/globallyUsed.R")
library(officer)
library(magrittr)
sspChoices <- c("ssp585") #"ssp126", 
modelChoices <- c( "GFDL-ESM4", "UKESM1-0-LL", "MPI-ESM1-2-HR", "MRI-ESM2-0", "IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
#modelChoices <- c("IPSL-CM6A-LR") # "GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL", "IPSL-CM5A-LR"
startYearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
climateVars <- c( "tasmin", "tasmax", "tas",  "pr", "hurs") 
varNamesInfo <- as.data.table(read_excel("data-raw/varNamesLookup.xlsx"))
globalMeanHolder <- as.data.table( read.csv("data/cmip6/annualMean/globalMeans.csv"))
globalMeanHolder[, yearSpan := gsub("_", "-", yearSpan)]
globalMeanHolder[, globalMeanValue := round(globalMeanValue, 1)]
globalMeanHolder <- dcast(globalMeanHolder, climVar + scenario ~ yearSpan, value.var = "globalMeanValue")
setorder(globalMeanHolder,  -climVar)
globalMeanHolder[varNamesInfo, climVar := variableLongName, on = c(climVar = "variableShortName")]
setnames(globalMeanHolder, old = c("climVar", "scenario"), new = c("Climate variable", "Scenario"))
yearRange <- 9
#test values
k <- "ssp585"
l <- 2091
j <- "tasmax"

titleString <- paste0("Annual Land-based Means of Minimum, Average and Maximum Temperature, Relative Humidity and Precipitation")
contentString <- paste0("Ensemble means and coefficients of key climate variables for four time periods to 2100. Powerpoint produced ", Sys.Date())
startYearChoices_ensemble <-  c(2021, 2051, 2091) # no multimodel results for observed data

my_pres <- read_pptx() %>% 
  add_slide(layout = 'Title Slide', master = 'Office Theme')  %>% 
  ph_with(value = titleString, location = ph_location_type(type = "ctrTitle")) %>% 
  ph_with(value = contentString, location = ph_location_type(type = "subTitle"))

IntroText0 <- "Daily minimum, average and maximum temperature (tmin or tasmin, tave and tmax or tasmax), relative humidity (rh or hurs), and precipitation (pr) are the key climate variables for agricultural activities.  "
IntroText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
IntroText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/). "
IntroText3 <- "It includes modeling results from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three GHG emission scenarios (ssp126, ssp370 and ssp585). "
IntroText3.5 <- "In this powerpoint, only results using ssp585 are presented." 
IntroText4 <- "The values are average means for observed data for 2001-2010 and  ensemble means and coefficients of variation for three 10 year periods (2021-2030, 2051-2060, and 2091-2100)."
IntroText5 <- "This powerpoint presents work in progress and should not be circulated without permission."
IntroText6 <- "The next slide presents the 10-year mean across all land-based locations for these key climate variables for each of the four 10 year periods."
fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
fp_2 <- fp_text(bold = FALSE, font.size = 12)
fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)

bl <- block_list(
  fpar(
    ftext(IntroText0, fp_2)),
  #    ftext(IntroText0.5, fp_2)),
  fpar(),
  fpar(
    ftext(IntroText1, fp_2),
    ftext(IntroText2, fp_2),
    ftext(IntroText3, fp_2)),
  fpar(),
  fpar(
    ftext(IntroText3.5, fp_2)),
  fpar(),
  fpar(
    ftext(IntroText4, fp_2)),
  fpar(),
  fpar(
    ftext(IntroText6, fp_2)),
  fpar(),
  fpar(
    ftext(IntroText5, fp_2))
  
)

library(flextable)
myft <- flextable(globalMeanHolder)
myft <- fontsize(myft, part = "header", size = 12)
myft <- fontsize(myft, part = "body", size = 10)
myft <- padding( myft, padding = 15, part = "all" )
myft <- theme_vanilla(myft)
myft <- width(myft, width = 1.5)
myft


my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
my_pres <- ph_with(x = my_pres, value = bl, location = ph_location_type(type = "body") )

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Ensemble Land-based Annual Mean Values", location = ph_location_type(type = "title")) %>%
  ph_with(value = myft, location = ph_location_type(type = "body")) #ph_location(left = .5, top = 1.5, width = 10, height = 4)) #location = ph_location_type(type = "body"))

for (k in sspChoices) {
  yearSpan <- paste0(l, "_", l + yearRange)
  print(paste0("ssp choice: ", k, ", start year: ", l))
  for (j in climateVars) {
    varName <- j
    varNameLong <- as.character(varNamesInfo[variableShortName %in% varName, variableLongName])
    varNameLongUnits <- as.character(varNamesInfo[variableShortName %in% varName, units])
    
    ensembleTitle <- paste("Ensemble Mean and Coefficient of Variation for", varNameLong)
    #    add_slide(my_pres, layout = 'Section Header', master = 'Office Theme')  %>% 
    #   ph_with(value = ensembleTitle, location = ph_location_type(type = "body"))
    # 
    # fileNameCts <- paste0(lofOfGraphicsFiles, "annualMean_", varName, ".jpg")
    # extImgObs <- external_img(src = fileNameCts, width = 5, height = 8)
    # 
    # add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
    # ph_with(value = extImgObs, location = ph_location(left = 2, top = 0, width = 5, height = 8) )
    # 
    fileNameObserved <- paste0(lofOfGraphicsFiles, "annualMean/annualMean_",  varName, "_observed_",  "2001_2010", ".jpg")
    
    extImgObs <- external_img(src = fileNameObserved, width = 5, height = 8)
    add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
      ph_with(value = extImgObs, location = ph_location(left = 0, top = 0, width = 5, height = 8) )
    
    for (l in startYearChoices_ensemble) {
      yearSpan <- paste0(l, "_", l + yearRange)
      fileNameCV <- paste0(lofOfGraphicsFiles, "annualMean/ensembleAnnualCV_",   varName, "_",  yearSpan, "_", k, ".jpg")
      fileNameMean <- paste0(lofOfGraphicsFiles, "annualMean/ensembleannualMean_",  varName, "_",  yearSpan, "_", k, ".jpg")
      
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

print(my_pres, target = "presentations/cmip6/annualMean/annualMeansCVs.pptx") %>% browseURL()

#print(my_pres, target = paste0("presentations/cmip6/THI/damageTemp_Ensemble", ".pptx"))

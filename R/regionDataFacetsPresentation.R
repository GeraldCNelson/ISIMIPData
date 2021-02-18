# script to create powerpoints for THI results
source("R/globallyUsed.R")
library(officer)
library(magrittr)
library(flextable)

sspChoices <- c("ssp126", "ssp585")
startYearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices_ensemble <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
climateVars <- c("tas", "tasmax", "tasmin", "pr", "hurs") 

#climateVars <- "tas"
varNamesInfo <- as.data.table(read_excel("data-raw/varNamesLookup.xlsx"))

yearRange <- 9
#test values
k <- "ssp585"
l <- 2091
j <- "tas"

regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookup.xlsx", range = "A1:k7"))
regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookupCSVs.xlsx", range = "F7:P43"))
regionInfoLookup[, ctyRegion := paste0("\n", region, ", " , country)]

for (i in 1:(nrow(regionInfoLookup))) {
  region <- regionInfoLookup[i, region]
  country <- regionInfoLookup[i, country]
  
  # regionMeanHolder[, yearSpan := gsub("_", "-", yearSpan)]
  # regionMeanHolder[, globalMeanValue := round(globalMeanValue, 1)]
  # regionMeanHolder <- dcast(regionMeanHolder, climVar + scenario ~ yearSpan, value.var = "globalMeanValue")
  # setorder(regionMeanHolder,  -climVar)
  # regionMeanHolder[varNamesInfo, climVar := variableLongName, on = c(climVar = "variableShortName")]
  # setnames(regionMeanHolder, old = c("climVar", "scenario"), new = c("Climate variable", "Scenario"))
  titleString <- paste0("Ensemble mean annual climate variable averages for ", region, ", ", country)
  contentString <- paste0("Ensemble means for four time periods to 2100. Powerpoint produced ", Sys.Date())
  my_pres <- read_pptx() %>% 
    add_slide(layout = 'Title Slide', master = 'Office Theme')  %>% 
    ph_with(value = titleString, location = ph_location_type(type = "ctrTitle")) %>% 
    ph_with(value = contentString, location = ph_location_type(type = "subTitle"))
  
  IntroText0 <- "Daily minimum, average and maximum temperature (tmin or tasmin, tave and tmax or tasmax), relative humidity (rh or hurs), and precipitation (pr) are the key climate variables for agricultural activities.  "
  IntroText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
  IntroText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/). "
  IntroText3 <- "It includes modeling results from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three GHG emission scenarios (ssp126, ssp370 and ssp585). "
  IntroText3.5 <- "In this powerpoint, only results using ssp585 are presented." 
  # IntroText4 <- "The values are average means for observed data for 2001-2010 and  ensemble means and coefficients of variation for four 10 year periods (2002-2010, 2021-2030, 2051-2060, and 2091-2100)."
  IntroText5 <- "This powerpoint presents work in progress and should not be circulated without permission."
#  IntroText6 <- "The next slide presents the 10-year annual mean temperature for all 1/2 degree pixels included in this region for each of the 10 year periods."
  
  format_page_title <- fp_text( font.size=24)
  
  fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
  fp_2 <- fp_text(bold = FALSE, font.size = 12)
  fp_3 <- fp_text(italic = TRUE, color = "black", font.size = 14)
  
  bl <- block_list(
    fpar(ftext(IntroText0, fp_2)),
    #    ftext(IntroText0.5, fp_2)),
    fpar(),
    fpar(ftext(IntroText1, fp_2),
         ftext(IntroText2, fp_2),
         ftext(IntroText3, fp_2)),
    fpar(),
    fpar(ftext(IntroText3.5, fp_2)),
    # fpar(),
    # fpar(
    #   ftext(IntroText4, fp_2)),
    # fpar(),
    # fpar(ftext(IntroText6, fp_2)),
    fpar(),
    fpar(ftext(IntroText5, fp_2))
  )
  
  my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
  my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
  my_pres <- ph_with(x = my_pres, value = bl, location = ph_location_type(type = "body") )
  
  for (k in sspChoices) {
    yearSpan <- paste0(l, "_", l + yearRange)
    for (j in climateVars) {
      varName <- j
      varNameLong <- as.character(varNamesInfo[variableShortName %in% varName, variableLongName])
      varNameLongUnits <- as.character(varNamesInfo[variableShortName %in% varName, units])
      annualMeanDataFile = paste0("data/regionResults/", j, "_", region, "_",k, ".csv")
      print(annualMeanDataFile)
      regionMeanHolder <- as.data.table( read.csv(annualMeanDataFile))
      
      myft <- flextable(regionMeanHolder, col_keys = names(regionMeanHolder))
      myft <- fontsize(myft, part = "header", size = 12)
      myft <- fontsize(myft, part = "body", size = 8)
      myft <- padding( myft, padding = 10, part = "all" )
      myft <- theme_vanilla(myft)
      myft <- width(myft, width = 1.5)
      print(myft)
      
      
      ensembleTitle <- paste0("Ensemble mean for average annual ", varNameLong)
      add_slide(my_pres, layout = 'Section Header', master = 'Office Theme')  %>% 
        ph_with(value = ensembleTitle, location = ph_location_type(type = "body"))
      
      titleText <- paste0("Ensemble-based annual mean values, ", varNameLong)
      my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme") %>%
        ph_with(block_list(fpar(ftext(titleText, prop = fp_text(font.size = 28, color = "black")))),
                location = ph_location_type(type = "title") ) %>%
 #       ph_with(value = paste0("Ensemble-based annual mean values, ", varNameLong), location = ph_location_type(type = "title")) %>%
        ph_with(value = myft, location = ph_location_type(type = "body")) #ph_location(left = .5, top = 1.5, width = 10, height = 4)) #location = ph_location_types(type = "body"))
      
      # 
      # fileNameCts <- paste0("graphics/cmip6/annualMean_", varName, ".jpg")
      # extImgObs <- external_img(src = fileNameCts, width = 5, height = 8)
      # 
      # add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
      # ph_with(value = extImgObs, location = ph_location(left = 2, top = 0, width = 5, height = 8) )
      # 
      # fileNameObserved <- paste0("graphics/cmip6/annualMean/annualMean_",  varName, "_observed_",  "2001_2010", ".jpg")
      # 
      # extImgObs <- external_img(src = fileNameObserved, width = 5, height = 8)
      # add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
      #   ph_with(value = extImgObs, location = ph_location(left = 0, top = 0, width = 5, height = 8) )
      # 
      for (l in startYearChoices) {
        yearSpan <- paste0(l, "_", l + yearRange)
        #      fileNameCV <- paste0("graphics/cmip6/annualMean/ensembleAnnualCV_",   varName, "_",  yearSpan, "_", k, ".jpg")
        fileNameMean <- paste0("graphics/cmip6/regionInfo/", j, "_", k, "_",  yearSpan, "_", region, ".png")
        extImgMean <- external_img(src = fileNameMean, width = 5, height = 5)
        #     extImgCV <- external_img(src = fileNameCV, width = 5, height = 8)
        
        #   add_slide(my_pres, layout = 'Comparison', master = 'Office Theme') %>% 
        #     ph_with(value = extImgMean, location = ph_location_left(),  use_loc_size = FALSE ) %>%
        # #  add_slide(my_pres, layout = 'Comparison', master = 'Office Theme') %>% 
        #     ph_with(value = extImgCV, location = ph_location_right(),  use_loc_size = FALSE )
        
        
        add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
          ph_with(value = extImgMean, location = ph_location(left = 1.5, top = .5, width = 7, height = 7) ) # %>%
        #      ph_with(value = extImgCV, location = ph_location(left = 5, top = 0, width = 5, height = 8) )
      }
    }
  }
  outFilename <- paste0("presentations/cmip6/regionInfo/annualMeans", "_", region, ", ", country, ".pptx")
  print(outFilename)
  print(my_pres, target = outFilename) # %>% browseURL()
}
#print(my_pres, target = paste0("presentations/cmip6/THI/damageTemp_Ensemble", ".pptx"))

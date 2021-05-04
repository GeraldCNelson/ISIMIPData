# script to create powerpoints for THI results
source("R/globallyUsed.R")
library(officer)
library(magrittr)
library(flextable)

sspChoices <- c("ssp126", "ssp585") #"ssp126", 
startYearChoices <-  c(2001, 2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
startYearChoices_ensemble <-  c(2021, 2051, 2091) #2011, 2041, 2051, 2081) # c(2091) # c(2006) #, 2041, 2051, 2081)
thiList <- c("thi.cattle", "thi.sheep", "thi.goat", "thi.yak", "thi.broiler", "thi.layer", "thi.chicken", "thi.pigs")
thiListReduced <- thiList[!thiList %in% c("thi.yak", "thi.broiler", "thi.layer")]

varNamesInfo <- as.data.table(read_excel("data-raw/varNamesLookup.xlsx"))

yearRange <- 9
#test values
k <- "ssp585"
l <- 2091
j <- "thi.cattle"

regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookup.xlsx", range = "A1:k7"))
#regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/wg2ch5Locations.xlsx", range = "a1:k16")) # Ch5 author locations
#regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookupCSVs.xlsx",range = "F7:P43")) #perennial crop author locations
#regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/PerennialCrops.xlsx",range = "A1:K6")) #perennial crop author locations

regionInfoLookup[, ctyRegion := paste0("\n", region, ", " , country)]

for (i in 3) {# 1:(nrow(regionInfoLookup))) {
  region <- regionInfoLookup[i, region]
  country <- regionInfoLookup[i, country]
  
  # regionMeanHolder[, yearSpan := gsub("_", "-", yearSpan)]
  # regionMeanHolder[, globalMeanValue := round(globalMeanValue, 1)]
  # regionMeanHolder <- dcast(regionMeanHolder, climVar + scenario ~ yearSpan, value.var = "globalMeanValue")
  # setorder(regionMeanHolder,  -climVar)
  # regionMeanHolder[varNamesInfo, climVar := variableLongName, on = c(climVar = "variableShortName")]
  # setnames(regionMeanHolder, old = c("climVar", "scenario"), new = c("Climate variable", "Scenario"))
  titleString <- paste0("Ensemble mean monthly livestock stress levels for ", region, ", ", country)
  contentString <- paste0("Ensemble means for four time periods to 2100. Powerpoint produced ", Sys.Date())
  my_pres <- read_pptx() %>% 
    add_slide(layout = 'Title Slide', master = 'Office Theme')  %>% 
    ph_with(value = titleString, location = ph_location_type(type = "ctrTitle")) %>% 
    ph_with(value = contentString, location = ph_location_type(type = "subTitle"))
  
  IntroText0 <- "Some text about livestock"  
  IntroText1 <- "The climate data set used in these graphics was prepared initially by the ISIMIP project (www.isimip.org) using CMIP6 data. " 
  IntroText2 <- "This analysis uses the ISIMIP3b output data sets (https://www.isimip.org/news/isimip3ab-protocol-released/). "
  IntroText3 <- "It includes modeling results from 5 earth system models (GFDL-ESM4, UKESM1-0-LL, MPI-ESM1-2-HR, MRI-ESM2-0, and IPSL-CM6A-LR) and three GHG emission scenarios (ssp126, ssp370 and ssp585). "
  IntroText3.5 <- "SSP126 has the lowest GHG emissions of the scenarios planned for use in the IPCC Sixth Assessment Report; SSP585 the highest. Note that the differences between SSP126 and SSP585 for temperature results become larger over time."
  IntroText4 <- "The ISIMIP data are available in 1/2 degree cells. The graphics presented here show these data in a 4 degree by 4 degree region around a center point of interest."
  IntroText5 <- "This powerpoint presents work in progress and should not be circulated without permission."
  #  IntroText6 <- "The next slide presents the 10-year monthly mean temperature for all 1/2 degree pixels included in this region for each of the 10 year periods."
  
  format_page_title <- fp_text( font.size=20)
  
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
    fpar(),
    fpar(
      ftext(IntroText4, fp_2)),
    #fpar(),
    # fpar(ftext(IntroText6, fp_2)),
    fpar(),
    fpar(ftext(IntroText5, fp_2))
  )
  
  my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
  my_pres <-  ph_with(x = my_pres, value = "Introduction", location = ph_location_type(type = "title"))
  my_pres <- ph_with(x = my_pres, value = bl, location = ph_location_type(type = "body"))
  
  for (j in thiListReduced) {
    varName <- j
    varNameLong <- as.character(varNamesInfo[variableShortName %in% varName, variableLongName])
    varNameLongUnits <- as.character(varNamesInfo[variableShortName %in% varName, units])
    # monthlyMeanDataFile = paste0("data/regionResults/", j, "_", region, "_",k, ".csv")
    # print(monthlyMeanDataFile)
    # regionMeanHolder <- as.data.table( read.csv(monthlyMeanDataFile))
    # 
    # myft <- flextable(regionMeanHolder, col_keys = names(regionMeanHolder))
    # myft <- fontsize(myft, part = "header", size = 12)
    # myft <- fontsize(myft, part = "body", size = 8)
    # myft <- padding( myft, padding = 10, part = "all" )
    # myft <- theme_vanilla(myft)
    # myft <- width(myft, width = 1.5)
    # print(myft)
    
    ensembleTitle <- paste0("Ensemble mean for average monthly stress for ", varNameLong)
    my_pres <- add_slide(x = my_pres, layout = 'Section Header', master = 'Office Theme')
    my_pres <- ph_with(x = my_pres, value = ensembleTitle, location = ph_location_type(type = "title"))
    # my_pres <- ph_with(x = my_pres, value = ensembleText, location = ph_location_type(type = "body"))
    
    #      titleText <- paste0("Ensemble-based monthly mean values, ", varNameLong)
    #      my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme") %>%
    #        ph_with(block_list(fpar(ftext(titleText, prop = fp_text(font.size = 28, color = "black")))),
    #                location = ph_location_type(type = "title") ) %>%
    # #       ph_with(value = paste0("Ensemble-based monthly mean values, ", varNameLong), location = ph_location_type(type = "title")) %>%
    #        ph_with(value = myft, location = ph_location_type(type = "body")) #ph_location(left = .5, top = 1.5, width = 10, height = 4)) #location = ph_location_types(type = "body"))
    #      
    # 
    # fileNameCts <- paste0("graphics/cmip6/monthlyMean_", varName, ".jpg")
    # extImgObs <- external_img(src = fileNameCts, width = 5, height = 8)
    # 
    # add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
    # ph_with(value = extImgObs, location = ph_location(left = 2, top = 0, width = 5, height = 8) )
    # 
    # fileNameObserved <- paste0("graphics/cmip6/monthlyMean/monthlyMean_",  varName, "_observed_",  "2001_2010", ".jpg")
    # 
    # extImgObs <- external_img(src = fileNameObserved, width = 5, height = 8)
    # add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
    #   ph_with(value = extImgObs, location = ph_location(left = 0, top = 0, width = 5, height = 8) )
    # 
    for (l in startYearChoices) {
      yearSpan <- paste0(l, "_", l + yearRange)
      if (l %in% startYearChoices[1]) { 
        fileNameMean <- paste0("graphics/cmip6/regionInfo/THI/", j, "MonthlyAve_",  yearSpan, "_", region, ".png")
        extImgMean <- external_img(src = fileNameMean, width = 5, height = 5)
        add_slide(my_pres, layout = 'Title Only', master = 'Office Theme') %>% 
          ph_with(value = extImgMean, location = ph_location(left = 1.5, top = .5, width = 7, height = 7) )
      }
      
      if (!l %in% startYearChoices[1]) {
        for (k in sspChoices) {
          
          #      fileNameCV <- paste0("graphics/cmip6/monthlyMean/ensemblemonthlyCV_",   varName, "_",  yearSpan, "_", k, ".jpg")
          fileNameMean <- paste0("graphics/cmip6/regionInfo/THI/", j, "MonthlyAve_", k, "_",  yearSpan, "_", region, ".png")
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
  }
  
  outFilename <- paste0("presentations/cmip6/regionInfo/THI/THImonthlyMeans", "_", region, ", ", country, ".pptx")
  unlink(outFilename)
  print(outFilename)
  print(my_pres, target = outFilename) # %>% browseURL()
}
#print(my_pres, target = paste0("presentations/cmip6/THI/damageTemp_Ensemble", ".pptx"))

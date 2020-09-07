# manipulate GADM data
library(sf)
library(data.table)
library(readxl)
# gadmIn <- st_read("/Volumes/ExtremeSSD/GADM/gadm36.gpkg") #GID_0 is the same as ISO 3 letter code
# regionsToRemove <- c("Akrotiri and Dhekelia", "Åland", " American Samoa", "Anguilla", "Antarctica", "Bonaire", 
#                      "Sint Eustatius and Saba", "Bouvet Island", "Indian Ocean Territory", "Caspian Sea", "Christmas Island",
#                      "Clipperton Island", "Cocos Islands", "Heard Island and McDonald Islands", "Isle of Man", 
#                      "Montenegro", "Montserrat", " Norfolk Island", "Northern Mariana Islands", "Reunion", "Saint Kitts and Nevis", 
#                      "Saint Lucia", "Saint Pierre and Miquelon", "Saint Vincent and the Grenadines", "Saint-Barthélemy", "Saint-Martin", 
#                      "South Georgia and the South Sandwich Islands", "São Tomé and Príncipe", "Sint Maarten", "Tokelau", "Wallis and Futuna")
# 
# # cols to keep for level 1 admin units
# colsToKeep <- c("GID_0", "NAME_0", "GID_1", "NAME_1", "NL_NAME_1", "GID_2", "NAME_2", "VARNAME_2", "NL_NAME_2", "TYPE_2", "ENGTYPE_2", "CC_2", "HASC_2")
# colsToKeepFewer <- c("GID_0", "NAME_0", "GID_1", "NAME_1", "NL_NAME_1")
# gadmSmall <- gadmIn[, colsToKeep]
# gadmSmaller <- gadmIn[, colsToKeepFewer]
# gadmSmall <- unique(gadmSmall)
# gadmSmall <- st_make_valid(gadmSmall)
# gadmSmaller <- unique(gadmSmaller)
# gadmSmaller <- st_make_valid(gadmSmaller)
# #temp <- gadmIn[regionsToRemove, colsToKeep]
# saveRDS(gadmSmall, file = "/Volumes/ExtremeSSD/GADM/gadmSmall.RDS")
# saveRDS(gadmSmaller, file = "/Volumes/ExtremeSSD/GADM/gadmSmaller.RDS")

ISO3sWNoLevel2 <- "TTO"
ISO3List <- character()
regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookup.xlsx", range = "A1:k7"))
ISO3List <- c(ISO3List, regionInfoLookup$ISO3)
regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/wg2ch5Locations.xlsx", range = "a1:k17")) # climate smart villages
ISO3List <- c(ISO3List, regionInfoLookup$ISO3)
regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/regionInfoLookupCSVs.xlsx",range = "F7:P43")) #perennial crop author locations
ISO3List <- c(ISO3List, regionInfoLookup$ISO3)
regionInfoLookup <- as.data.table(read_excel("data-raw/regionInformation/PerennialCrops.xlsx",range = "A1:K6")) #perennial crop author locations
ISO3List <- c(ISO3List, regionInfoLookup$ISO3)
ISO3List <- unique(ISO3List)
for (i in ISO3List)  {
  f1 <- getData("GADM", level = 1, country = i)
  saveRDS(f1, paste0("data-raw/regionInformation/regionsISO3/ISO3_", i, "1.RDS"))
  if (!i %in% ISO3sWNoLevel2) {
    f2 <- getData("GADM", level = 2, country = i)
    saveRDS(f2, paste0("data-raw/regionInformation/regionsISO3/ISO3_", i, "2.RDS"))
  }
}

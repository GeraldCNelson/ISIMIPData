# manipulate GADM data
library(sf)
gadamIn <- st_read("/Volumes/ExtremeSSD/GADM/gadm36.gpkg") #GID_0 is the same as ISO 3 letter code
# cols to keep for level 1 admin units
colsToKeep <- c("GID_0", "NAME_0", "GID_1", "NAME_1", "NL_NAME_1", "GID_2", "NAME_2", "VARNAME_2", "NL_NAME_2", "TYPE_2", "ENGTYPE_2", "CC_2", "HASC_2")
gdamSmall <- gadamIn[, colsToKeep]
saveRDS(gdamSmall, file = "/Volumes/ExtremeSSD/GADM/gdamSmall.RDS")
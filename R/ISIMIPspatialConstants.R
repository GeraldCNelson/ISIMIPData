# ISIMIP project spatial constants
library(sf) # only needed for doing some spatial stuff
RobinsonProj <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
GoodeHomolosineProj <- "+proj=goode" # see https://proj.org/operations/projections/goode.html
crsRob <- RobinsonProj
crsGoode <- GoodeHomolosineProj
crslatlong <- "+proj=longlat +datum=WGS84 +no_defs"
coastline <- st_read("data-raw/regionInformation/ne_50m_coastline/ne_50m_coastline.shp")
coastline_cropped <- st_crop(coastline, c(xmin = -180, xmax = 180, ymin = -60, ymax = 90))
coastline_cropped <- st_transform(coastline_cropped, crsRob)

#function to get rid of Antarctica, used only on the coastline sf file; commenting out because probably not needed but leaving here in case that is wrong
# f_crop_custom <- function(poly.sf) {
#   poly.sp <- as(poly.sf, "Spatial")
#   extR <- raster::extent(c(-180, 180, -60, 90))
#   poly.sp.crop <- crop(poly.sp, extR)
#   st_as_sf(poly.sp.crop)
# }
# coastline <- f_crop_custom(coastline)


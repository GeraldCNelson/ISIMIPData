# ISIMIP project spatial constants
require(terra)
source("R/ISIMIPconstants.R")
RobinsonProj <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
GoodeHomolosineProj <- "+proj=goode +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # see https://proj.org/operations/projections/goode.html
crsRob <- RobinsonProj
crsGoode <- GoodeHomolosineProj
crslatlong <- "+proj=longlat +datum=WGS84 +no_defs"
coastline <- vect("data-raw/regionInformation/ne_50m_coastline/ne_50m_coastline.shp")
coastline_cropped <- crop(coastline, extent_noAntarctica )

coastline_cropped_Rob <- project(coastline_cropped, RobinsonProj)
coastline_cropped_igh <- project(coastline_cropped, GoodeHomolosineProj)

landOnlyMask <- rast(paste0(locOfRawDataFiles, "landseamask.nc"))
landOnlyMaskNoAntarctica <- rast(paste0(locOfRawDataFiles, "landseamask_no_antarctica.nc"))

# examples of adding a coastline and removing x and y axis labels and legend
# plot(r_suitable_globe, main = titleText, legend = FALSE, xlab = FALSE, axes=FALSE)
# plot(coastline_cropped_spvect, add = TRUE)


# testing interrupted good homolosine

crsGoode <- "+proj=igh +ellps=sphere +towgs84=0,0,0 +lon_0=100w +x_0=-11119487.43"

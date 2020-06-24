ibrary(terra)
r <- rast(nl=12)
values(r) <- 1:12
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
project(r, crs)
#class       : SpatRaster 
#dimensions  : 278, 547, 12  (nrow, ncol, nlyr)
#resolution  : 62123.91, 61900.45  (x, y)
#extent      : -17005833, 16975948, -8583170, 8625155  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
#data source : memory 
#names       : lyr1, lyr2, lyr3, lyr4, lyr5, ... 
#min values  :    1,    1,    1,    1,    1, ... 
#max values  :   12,   12,   12,   12,   12, ... 


And to make it pretty (removing the filled corners that should not be in the Robinson proj)

xy1 <- cbind(xFromCol(r, 1), yFromRow(r, 1:nrow(r)))
xy2 <- cbind(xFromCol(r, ncol(r)), yFromRow(r, nrow(r):1))
v <- vect(rbind(xy1, xy2), type="polygons", crs=crs(r))
v <- project(v, crs)

x <- project(r, crs)
x <- mask(x, v)
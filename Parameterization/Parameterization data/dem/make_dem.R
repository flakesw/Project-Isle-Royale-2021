# create mosaic DEM
# This script mosaics a set of DEM tiles downloaded from earthexplorer
# and then crops them to a study area polygon
library("raster")
library("sf")

setwd("C:/Users/Sam/Documents/Research/Isle Royale/calibration_data/dem")

#dem layers are 1-acrsecond SRTM layers, available at https://earthexplorer.usgs.gov/

rast1 <- raster('USGS_1_n48w090.tif')
rast2 <- raster('USGS_1_n49w090.tif')
rast3 <- raster('USGS_1_n49w089.tif')
rast4 <- raster('USGS_1_n48w089.tif')

dem_combined <- mosaic(rast1, rast2, rast3, rast4, fun = mean)

ir_boundary <- sf::st_read("../ir_polygon/ir_polygon2.shp") %>%
  sf::st_transform(crs = st_crs(dem_combined))

dem_ir <- crop(dem_combined, extent(ir_boundary)) %>%
  mask(ir_boundary) %>%
  projectRaster(crs = "EPSG:4269")
plot(dem_ir)

writeRaster(dem_ir, "dem_ir.tif", format = "GTiff", overwrite = TRUE)


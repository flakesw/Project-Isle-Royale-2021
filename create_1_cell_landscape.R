#create test landscape with just one cell
#
library("raster")
ecoregions <- raster("./LANDIS inputs/input rasters/ecoregions.tif")
values(ecoregions) <- 0
ecoregions[1513, 1098] <- 1 #choose which cell
writeRaster(ecoregions, "./LANDIS inputs/input rasters/ecoregions_test.tif", datatype = "INT2S", overwrite = TRUE)


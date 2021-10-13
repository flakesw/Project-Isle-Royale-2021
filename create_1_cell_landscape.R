#create test landscape with just one cell
#
library("raster")
ecoregions <- raster("./LANDIS inputs/input rasters/ecoregions.tif")
values(ecoregions) <- 0
ecoregions[690, 2877] <- 1
writeRaster(ecoregions, "./LANDIS inputs/input rasters/ecoregions_test.tif", datatype = "INT2S", overwrite = TRUE)

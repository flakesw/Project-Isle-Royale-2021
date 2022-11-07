#create test landscape with just one cell
#
library("raster")
ecoregions <- raster("./Models/LANDIS inputs/input rasters/ecoregions.tif")
values(ecoregions) <- 0
ecoregions[792, 518] <- 1 #choose which cell
writeRaster(ecoregions, "./Models/LANDIS inputs/input rasters/ecoregions_one_cell_lowland.tif", datatype = "INT2S", overwrite = TRUE)


#create test landscape with just one cell
#
library("raster")
ecoregions <- raster("./Models/LANDIS inputs/input rasters/ecoregions_inv.tif")
values(ecoregions) <- 0
# ecoregions[145, 139] <- 1 #choose which cell
ecoregions[1564051] <- 1
writeRaster(ecoregions, "./Models/LANDIS inputs/input rasters/ecoregions_one_cell.tif", datatype = "INT2S", overwrite = TRUE)
table(values(ecoregions))
 
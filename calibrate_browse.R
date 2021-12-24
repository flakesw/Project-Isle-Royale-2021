# calibrate browse

#create browse regions
old_region <- raster("./LANDIS inputs/browse/pop_zone_052318.img")

#ecoregion
ecoregion <- raster("./LANDIS inputs/input rasters/ecoregions_test.tif")

#need to update NECN to make max biomass available
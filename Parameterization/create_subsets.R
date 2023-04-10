# subset the rasters for a reduced study area
library("terra")

setwd("C:/Users/Sam/Documents/Research/Isle Royale")

#directory of original rasters
raster_list  <- list.files(path = "./Models/LANDIS inputs/input rasters", pattern='.tif$', all.files=TRUE, full.names=FALSE)


large_mask <- rast("./Models/LANDIS inputs/input rasters/ecoregions.tif")

shapes <- sf::st_read("./Parameterization/Parameterization data/browse/loc_shape.shp")

subset_mask <- crop(large_mask, vect(shapes)) %>%
  mask(vect(shapes))


for(i in 1:length(raster_list)){
  #import original raster
  raster1 <- rast(paste0("./Models/LANDIS inputs/input rasters/", raster_list[i]))
  data_type  <- ifelse(is.int(raster1), "INT2S", "FLT4S")
  # crs(raster1) <- "EPSG:2163"
  
    #clip and mask raster to subset mask
  raster1_clip <- terra::crop(raster1, subset_mask) %>%
    terra::mask(subset_mask, maskvalues = NA, updatevalue = 0)
  values(raster1_clip)[is.na(values(raster1_clip))] <- 0
  if(is.int(raster1)) values(raster1_clip) <- as.integer(values(raster1_clip))

  #write raster
  terra::writeRaster(raster1_clip, 
                      paste0("./Models/LANDIS inputs/input rasters hodgson/",
                             substr(raster_list[i], 1, 
                                    nchar(raster_list[i])-4),
                      ".tif"),
                      datatype = data_type,
                      overwrite = TRUE)
}


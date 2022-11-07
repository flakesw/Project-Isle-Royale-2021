# subset the rasters for a reduced study area
library("raster")

setwd("C:/Users/Sam/Documents/Research/Isle Royale")

#directory of original rasters
raster_list  <- list.files(path = "./Models/LANDIS inputs/input rasters", pattern='.tif$', all.files=TRUE, full.names=FALSE)


large_mask <- raster("./Models/LANDIS inputs/input rasters/ecoregions.tif")

e <- as(extent(-69000, -66000, 5354000, 5357000), 'SpatialPolygons')
crs(e) <- crs(large_mask)
subset_mask <- crop(large_mask, e)


for(i in 1:length(raster_list)){
  #import original raster
  raster1 <- raster(paste0("./Models/LANDIS inputs/input rasters/", raster_list[i]))
  data_type <- dataType(raster1)
  # crs(raster1) <- "EPSG:2163"
  
    #clip and mask raster to subset mask
  raster1_clip <- raster::crop(raster1, extent(subset_mask))
  raster1_clip <- mask(raster1_clip, subset_mask, maskvalue = 0, updatevalue = 0)
  values(raster1_clip)[is.na(values(raster1_clip))] <- 0

  #write raster
  raster::writeRaster(raster1_clip, 
                      paste0("./Models/LANDIS inputs/input rasters subset east/",
                             substr(raster_list[i], 1, 
                                    nchar(raster_list[i])-4),
                             "_subset.tif"),
                      datatype = data_type,
                      NAvalue = 0,
                      overwrite = TRUE)
}

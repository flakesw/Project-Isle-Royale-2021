#Create ignitions maps for SCRPPLE
# From Short et al. data and a boundary box of the study area

#This isn't actually that helpful for ISRO because it's such a small area
#with few ignitions

library("terra")
library("sf")
library("spatstat")
sf::sf_use_s2(TRUE)

# raster to get bounding box from
mask <- terra::rast("./Models/LANDIS inputs/input rasters/ecoregions_inv.tif")
crs(mask)
poly_bound <- sf::st_read("./Parameterization/Parameterization data/isle_royale_boundary_buffer/isle_royale_boundary_buffer.shp")%>%
  sf::st_union()%>%
  sf::st_transform(crs = crs(mask))

#geomac data are in EPSG:4269 or EPSG:4326
region <- sf::st_read("D:/Data/epa_ecoregions/us_eco_l3/us_eco_l3.shp") %>%
  filter(US_L3CODE %in% c(50,51)) %>%
  sf::st_union()%>%
  sf::st_transform(crs = crs(mask))


#-------------------------------------------------------------------------------
#import Short data

# sf::st_layers("./Parameterization/Parameterization data/short/FPA_FOD_20210617.gdb")
# short <- sf::st_read("./Parameterization/Parameterization data/short/FPA_FOD_20210617.gdb", layer = "Fires")
# st_geometry_type(short)
# st_crs(short)
# short_full <- st_transform(short, crs(mask))
# 
# short_subset <- sf::st_intersection(short_full, region)
# sf::st_write(short_subset, "./Parameterization/Parameterization data/short/short_region.gpkg")

short_subset <- sf::st_read("./Parameterization/Parameterization data/short/short_region.gpkg")
short_subset <- sf::st_intersection(short_full, poly_bound)



lightning_ignitions <- short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Natural", ]
head(lightning_ignitions)
nrow(lightning_ignitions) #22 -- very few on island!
table(lightning_ignitions$FIRE_NAME)
plot(region)
plot(lightning_ignitions, add = TRUE)

acc_ignitions <- short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Human", ]
head(acc_ignitions)
nrow(acc_ignitions) #10 -- very few on island!
plot(region)
plot(acc_ignitions, add = TRUE)

#-------------------------------------------------------------------------------
# Do a density kernel to estimate intensity
# these are made oversized to avoid edge effects, and then later aligned and
# clipped to the mask of the study area

lightning_coords <- as.data.frame(st_coordinates(lightning_ignitions))
win <- owin(xrange = range(lightning_coords$X), yrange = range(lightning_coords$Y))
lightning.ppp <- ppp(lightning_coords$X, lightning_coords$Y, window = win)
lightning_raster <- density.ppp(lightning.ppp, adjust = 0.2, kernel = "gaussian", eps = 180)
lightning_raster <- rast(lightning_raster) 
terra::set.crs(lightning_raster, crs(short_subset))
lightning_raster <- terra::project(lightning_raster, crs(mask))
plot(lightning_raster)

plot(mask)
plot(lightning_raster, add = TRUE)

hist(lightning_raster)
lightning_raster <- lightning_raster * 1e7

lightning_raster_constant <- mask
terra::writeRaster(lightning_raster_constant, 
                   filename = "./Models/LANDIS inputs/scrpple/lightning_ignitions_constant.tiff", 
                   filetype = "GTiff", 
                   overwrite = TRUE)

acc_coords <- as.data.frame(st_coordinates(acc_ignitions))
win <- owin(xrange = range(acc_coords$X), yrange = range(acc_coords$Y))
acc.ppp <- ppp(acc_coords$X, acc_coords$Y, window = win)
acc_raster <- density.ppp(acc.ppp, adjust = 0.2, kernel = "gaussian", eps = 1000)
acc_raster <- rast(acc_raster) 
terra::set.crs(acc_raster, crs(short_subset))
acc_raster <- terra::project(acc_raster, crs(mask))

plot(acc_raster)
hist(acc_raster)
acc_raster <- acc_raster * 1e6

acc_raster_constant <- mask
terra::writeRaster(acc_raster_constant, 
                   filename = "./Models/LANDIS inputs/scrpple/accidental_ignitions_constant.tiff", 
                   filetype = "GTiff", 
                   overwrite = TRUE)



#clip to study area
#this doesn't really work for ISRO since it's so small
lightning_raster <- rast(lightning_raster)
lightning_clip <- terra::crop(lightning_raster, mask)
lightning_clip <- terra::resample(lightning_raster, mask, method = "bilinear")
lightning_masked <- raster::mask(lightning_clip, mask, maskvalue = 0, updatevalue = 0)
writeRaster(lightning_masked, "./Models/LANDIS inputs/scrpple/lightning_ignitions.tif", "GTiff", overwrite = TRUE)

acc_raster <- raster(acc_raster)
acc_clip <- raster::crop(acc_raster, extent(mask))
acc_clip <- raster::resample(acc_raster, mask, method = "bilinear")
acc_masked <- raster::mask(acc_clip, mask, maskvalue = 0, updatevalue = 0)
writeRaster(acc_masked, "./Models/LANDIS inputs/scrpple/accidental_ignitions.tif", "GTiff", overwrite = TRUE)


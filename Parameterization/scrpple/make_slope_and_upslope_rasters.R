#get slope steepness and upslope azimuth maps

library("rgrass7")
library("sf")
library("openSTARS")
library("terra")

mask <- terra::rast("C:/Users/Sam/Documents/Research/Isle Royale/Models/LANDIS inputs/input rasters/initial_communities_inv.tif")

dem <- terra::rast('C:/Users/Sam/Documents/Research/Isle Royale/Parameterization/Parameterization data/dem/dem_ir_60m.tif')

plot(dem)

slope <- terra::terrain(dem, "slope")
writeRaster(slope, "./Models/LANDIS inputs/scrpple/slope.tif")
aspect <- terra::terrain(dem, "aspect")
upslope <- aspect-180
upslope[] <- ifelse(upslope[]<0, upslope[]+360, upslope[])
writeRaster(upslope, "./Models/LANDIS inputs/scrpple/upslope.tif")

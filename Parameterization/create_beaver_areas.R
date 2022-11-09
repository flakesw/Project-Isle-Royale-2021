# Make management area maps
library("sf")
library("openSTARS")
library("terra")
library("rgrass7")

setwd("C:/Users/Sam/Documents/Research/Isle Royale/")

#load mask
#load stream map
#load lake map
#combine stream map and lake map
#buffer stream map to create foraging zone

#digitize beaver dam locations for potential impoundments

#set this to your grass directory
grass_program_path <- "C:/Program Files/GRASS GIS 7.8"

#mask of study area
mask <- terra::rast("C:/Users/Sam/Documents/Research/Isle Royale/Models/LANDIS inputs/input rasters/initial_communities.tif")

#import and project dem to mask
rname <- "C:/Users/Sam/Documents/Research/Isle Royale/Parameterization/Parameterization data/dem/dem_ir.tif"
r <- terra::rast(rname) %>%
  terra::project(mask)
#write the projected dem
new_rname <- 'C:/Users/Sam/Documents/Research/Isle Royale/Parameterization/Parameterization data/dem/dem_ir_60m.tif'
terra::writeRaster(r, new_rname, overwrite = TRUE)


#-----------------
# 
initGRASS(gisBase= grass_program_path, #where the GRASS program lives; folder with bin and lib subfolders
          gisDbase='C:/Users/Sam/Documents/Research/Isle Royale/Parameterization/Parameterization data/',  #where to open GRASS
          location = 'dem', #subdirectory -- working directory. Not sure what the difference is between this, home, and gisDbase
          mapset='PERMANENT',
          SG = r,
          override = TRUE)

# unlink_.gislock() # unlock to be able to open testR database in GRASS

#change CRS to match DEM raster (I think there's a better way to do this by initializing GRASS with the right DEM?)
execGRASS("g.proj", flags = "c", epsg = as.numeric(crs(r, describe = TRUE)$code))

## initialize new mapset inheriting projection info
# I don't know what this is doing
execGRASS("g.mapset", flags = "c", mapset = "new_mapset")
execGRASS("g.mapset", flags = "c", mapset = "PERMANENT")
execGRASS("g.mapset", flags = "l")

# Import raster to GRASS and set region
execGRASS("r.in.gdal", flags="o", parameters=list(input=new_rname, output="elev_rast")) #import DEM raster located at "input", and add as "output"
execGRASS("g.region", parameters=list(raster="elev_rast") ) #set region to match DEM
execGRASS("g.region", flags = "p")

# create subwatershed: 
# input DEM is strm30_utm (stored in attached Grass database), 30 meter resolution
# output stream raster is up_stream
execGRASS('r.watershed', flags='overwrite', 
          parameters = list(elevation='elev_rast', 
                            threshold=2000, 
                            stream='up_stream', 
                            basin='r_basin',
                            tci = "topographic_index"))

#read the topographic convergence raster in from GRASS to R
tci <- read_RAST(vname = "topographic_index", cat=NULL, NODATA=NULL, ignore.stderr=get.ignore.stderrOption(),
                 return_format="terra", close_OK=TRUE, flags=NULL)
plot(tci)
str(tci) #read in as a terra SpatRaster; not sure how to change this or if it's determined by rGRASS

# r.thin thins non-NULL pixels so that each line is only 1 pixel wide, required before converting to vector
# this uses the upstream raster created by r.watershed
execGRASS('r.thin', flags='overwrite',parameters =  list(input='up_stream', output='r_strm_thin'))

# convert raster (r_strm_thin) to vector (v_stream)
execGRASS('r.to.vect', flags='overwrite', 
          parameters = list(input='r_strm_thin', output='v_stream', type = "line"))

# clean so that each line segment is at least 65 meters long 
execGRASS('v.clean', flags=c('overwrite'), 
          parameters =list(input='v_stream', output='v_stream_clean', type='line',tool='snap',threshold=65))


# export stream vector to shapefile
# execGRASS('v.out.ogr', parameters=list(input='v_stream',type="line", dsn="/Users/junf/Documents/testR/",format="ESRI_Shapefile"))

streams <- read_VECT('v_stream_clean')
str(streams) #it's brought in as a terra SpatVector, which I've never really used
plot(streams)

streams <- sf::st_as_sf(streams)

#TODO set distances from literature
mgt1 <- st_buffer(streams, dist = 50) #inundation zone
plot(st_geometry(mgt1))

mgt2 <- st_buffer(streams, dist = 200) %>% #beaver foraging zone, should be "toroid"
 st_difference(mgt1)
plot(st_geometry(mgt2))

#make a quick figure
plot(r)
plot(sf::st_geometry(mgt2), add = TRUE, col = "red", border = FALSE)
plot(sf::st_geometry(mgt1), add = TRUE, col = "darkgreen", border = FALSE)

# in ggplot with stars
library("ggplot2")

ggplot() + 
  stars::geom_stars(data = stars::st_as_stars(r)) + 
  geom_sf(data = mgt2, fill = "red", colour = NA) +
  geom_sf(data = mgt1, fill = "darkgreen", colour = NA)



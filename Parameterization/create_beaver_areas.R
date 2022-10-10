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


grass_program_path <- "C:/Program Files/GRASS GIS 7.8"

# setwd("D:/TMP") # Working directory 

mask <- terra::rast("C:/Users/Sam/Documents/Research/Isle Royale/Models/LANDIS inputs/input rasters/initial_communities.tif")

# Set on-disk raster variable
rname <- "C:/Users/Sam/Documents/Research/Isle Royale/Parameterization/Parameterization data/dem/dem_ir.tif"
r <- terra::rast(rname) %>%
  terra::project(mask)
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
# execGRASS("g.mapset", flags = "c", mapset = "new_mapset")
# execGRASS("g.mapset", flags = "c", mapset = "PERMANENT")
# execGRASS("g.mapset", flags = "l")

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


tci <- read_RAST(vname = "topographic_index", cat=NULL, NODATA=NULL, ignore.stderr=get.ignore.stderrOption(),
                 return_format="terra", close_OK=TRUE, flags=NULL)
plot(tci)

tci
# r.thin thins non-NULL pixels so that each line is only 1 pixel wide, required before converting to vector
execGRASS('r.thin', flags='overwrite',parameters =  list(input='up_stream', output='r_strm_thin'))

# convert raster (r_strm_thin) to vector (v_stream)
execGRASS('r.to.vect', flags='overwrite',parameters = list(input='r_strm_thin', output='v_stream'))

# clean so that each line segment is at least 65 meters long 
execGRASS('v.clean', flags=c('overwrite'),parameters =list(input='v_stream', output='v_stream_clean', type='line',tool='snap',thresh=65))

# display output vector
system("d.mon x0")
system("d.rast r_basin")
system("d.vect v_stream_clean")

# export stream vector to shapefile
execGRASS('v.out.ogr', parameters=list(input='v_stream',type="line", dsn="/Users/junf/Documents/testR/",format="ESRI_Shapefile"))



#study area map
ir_raster <- raster::raster("./calibration_data/ecoregions_051718.img")
crs(ir_raster)

#make layer of streams



#streams from EDNA -- TODO create our own map from DEM, this one is garbage
streams_us <- sf::st_read("./calibration_data/us_streams/streams.shp")
plot(st_geometry(streams_us))

ir_boundary <- sf::st_read("./calibration_data/isle_royale_boundary_buffer/isle_royale_boundary_buffer.shp") %>%
  sf::st_transform(crs = st_crs(streams_us))
streams_ir <- sf::st_intersection(streams_us, ir_boundary) %>%
  sf::st_transform(crs = st_crs(ir_raster))
sf::st_write(streams_ir, "streams_ir.shp")


plot(st_geometry(streams_ir))
plot(ir_raster)

#TODO set distances from literature
mgt1 <- st_buffer(streams_ir, dist = 50) #inundation zone
plot(st_geometry(mgt1))
st_write(mgt1, "mgt1.shp")

mgt2 <- st_buffer(streams_ir, dist = 100) %>% #beaver foraging close, should be "toroid"
 st_difference(mgt1)
plot(st_geometry(mgt2))
st_write(mgt2, "mgt2.shp", overwrite = TRUE)

mgt3 <- st_buffer(mgt2, dist = 200)

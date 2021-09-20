# Make management area maps
library("sf")
library("openSTARS")
library("raster")
library("rgrass7")

setwd("C:/Users/Sam/Documents/Research/Isle Royale/")

#load mask
#load stream map
#load lake map
#combine stream map and lake map
#buffer stream map to create foraging zone

#digitize beaver dam locations for potential impoundments
dem_path <- "C:/Users/Sam/Documents/Research/Isle Royale/calibration_data/dem/dem_ir.tif"

grass_program_path <- "C:/Program Files/GRASS GIS 7.8"

# setwd("D:/TMP") # Working directory 

# Set on-disk raster variable
rname <- paste(getwd(), "./calibration_data/dem/dem_ir.tif", sep="/")

# Set GRASS environment and database location 
loc <- initGRASS(gisBase = grass_program_path, 
                 home=tempdir(),
                 location = 'C:\\Users\\Sam\\Documents\\grassdata\\ir',
                 mapset = "PERMANENT",
                 gisDbase="GRASS_TEMP", override=TRUE )

execGRASS("g.proj", flags = "c", epsg = 4269)

## initialize new mapset inheriting projection info
execGRASS("g.mapset", flags = "c", mapset = "new_mapset")


# Import raster to GRASS and set region
execGRASS("r.in.gdal", flags="o", parameters=list(input=rname, output="tmprast"))
execGRASS("g.region", parameters=list(raster="tmprast") ) 

# Calculate 9x9 focal mean 
execGRASS("r.neighbors", flags="overwrite", parameters=list(input="tmprast", output="xxfm", 
                                                            method="average", size=as.integer(9)) )

r <- readRAST("xxfm")
spplot(r)

#-----------------

initGRASS(gisBase='C:/Program Files/GRASS GIS 7.8', 
          gisDbase='C:/Users/Sam/Documents/Research/Isle Royale/calibration_data',
          location = 'dem',
          mapset='PERMANENT', override = TRUE)

unlink_.gislock() # unlock to be able to open testR database in GRASS

# create subwatershed: 
# input DEM is strm30_utm (stored in attached Grass database), 30 meter resolution
# output stream raster is up_stream
execGRASS('r.watershed', flags='overwrite',parameters = list(elevation='dem_ir.tif', threshold=2000, stream='up_stream', basin='r_basin'))

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

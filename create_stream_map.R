# make a stream map from a DEM using GRASS and rgrass7
# and derive topographic data needed for 
library("rgrass7")
library("openSTARS")
library("link2GI")
library("raster")
library("tidyverse")
library("sf")
library("sp")

use_sp()

#follow tutorial here: https://www.bookstack.cn/read/Robinlovelace-geocompr/spilt.5.11.md

# GRASS commands reference here: https://grass.osgeo.org/grass78/manuals/raster.html

dem <- raster("./calibration_data/dem/dem_ir.tif") %>%
  projectRaster(crs = "EPSG:26916") #reproject to NAD83 UTM Zone 16

new_dem <- raster(extent(dem))
res(new_dem) <- 30

dem <- resample(dem, new_dem, method = "bilinear")


link =findGRASS()

library(rgrass7)
# find a GRASS 7 installation, and use the first one
ind =grep("7", link$version)[2]
# next line of code only necessary if we want to use GRASS as installed by 
# OSGeo4W. Among others, this adds some paths to PATH, which are also needed
# for running GRASS.
link2GI::paramGRASSw(link[ind,])

grass_path = "C:\\Program Files\\GRASS GIS 7.8"

initGRASS(gisBase = grass_path,
          gisDbase ="C:/Users/Sam/Documents/Research/Isle Royale/calibration_data/grass_db",
          location ="isro",
          mapset ="PERMANENT",override=TRUE)

execGRASS("g.proj",flags =c("c","quiet"),
          proj4 = crs(dem)@projargs)

b_box =st_bbox(dem)

execGRASS("g.region", flags =c("quiet"),
          n =as.character(b_box["ymax"]),s =as.character(b_box["ymin"]),
          e =as.character(b_box["xmax"]),w =as.character(b_box["xmin"]),
          res = "30")

rgrass7::writeRAST(as(dem, "SpatialGridDataFrame"), "isro_dem", flags = "overwrite")

# derive_streams(
#   burn = 0,
#   accum_threshold = 700,
#   condition = TRUE,
#   min_stream_length = 0,
#   dem_name = NULL,
#   clean = TRUE,
#   mem = FALSE
# )

execGRASS(cmd = "r.terraflow", flags = c("s", "overwrite"), 
          elevation = "isro_dem", accumulation = "acc",
          tci = "topo_conv_ind")

execGRASS(cmd = "r.terraflow", flags = c("s", "overwrite"), 
          elevation = "isro_dem", accumulation = "acc8",
          tci = "topo_conv_ind8")


# acc_flow <- rgrass7::readRAST("acc")

tci_s <- rgrass7::readRAST("topo_conv_ind")
tci_8 <- rgrass7::readRAST("topo_conv_ind8")




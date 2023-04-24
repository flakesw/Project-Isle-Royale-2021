#use SSURGO and DEM data to create various input rasters
# heavily borrowed from work by Zachary Robbins, Kate Jones, and Melissa Lucash
# see https://github.com/LANDIS-II-Foundation/Project-Southern-Appalachians-2018/tree/master/Parameterizing/Soils


# Data sources;
# gSSURGO from July 2020 update
# shapefile of study area provided by NPS (and other geospatial information) available here https://www.nps.gov/im/vmi-isro.htm
# additional geology data: https://irma.nps.gov/DataStore/Reference/Profile/2165823
# soil carbon and nitrogen layers form soilgrids.org


# > To create the NECN soil inputs we combined SSURGO data, deadwood data from the US Forest Inventory and Analysis Program , baseflow and storm flow approximations, and data on soil carbon and nitrogen. 
# This process can be seen at https://github.com/LANDIS-II-Foundation/Project-Southern-Appalachians-2018/tree/master/Parameterizing/Soils. Maps for depth, drainage, flood frequency sand, and clay percentage, field capacity and wilting point were derived from the USGS gssurgo dataset, subset to the study extent. These soils maps were aggregated from there an original resolution of (10m) were aggregated to the study resolution of 250m. Total carbon was calculated using the CONUS level carbon maps scale to the resolution of the study area (West, 2014). The guidelines in the Century
# the manual was used to divide the SOM into the fast, slow, and passive pools and then calculate the
# total N in each pool (Parton 2013). Deadwood and roots were calculated by interpolating between FIA sites for dead wood and assuming dead roots made up 1/3 or dead wood values. 
# 
# 
# * Parton, W. 2013. CENTURY Soil Organic Matter Model Environment. Technical
# Documentation. Agroecosystem Version 3.0. USDA-ARS, Forest Collins, CO. 
# * West, T.O. 2014. Soil Carbon Estimates in 20-cm Layers to 1-m Depth for the Conterminous US, 1970-1993. Data set. Available on-line [http://daac.ornl.gov] from Oak Ridge National Laboratory Distributed Active Archive Center, Oak Ridge, Tennessee, USA. http://dx.doi.org/10.3334/ORNLDAAC/1238
# * Soil Survey Staff, Natural Resources Conservation Service, United States Department of Agriculture. Soil Survey Geographic (SSURGO) Database for [TN,GA,NC,SC]. Available online. Accessed [10/17/2019].
#  
# 
# This is a methodology for creating the soil and hydrology maps necessary to Run LANDIS-II NECN. These methods were provided by Melissa Lucash and I want to thank her for sharing them. 
# 
# 
# The Maps needed to run LANDIS-II are 
# 
# * Soil Depth ^1^
#   * Soil Drain ^1^
#   * Field Capacity ^1^
#   * Wilting Point ^1^
#   * Percent Sand  ^1^
#   * Percent Clay  ^1^
#   * Base Flow to Streams ^4^
#   * Storm Flow to Streams ^4^
#   * Soil Maps representing 4 layers of carbon pools ^2^
#   * Soil Maps representing 4 layers of nitrogen pools ^2^
#   * Dead Wood on the Surface ^3^
#   * Dead Wood of Coarse Roots ^3^
#   
#   All of the Maps 1 are derived from the USGS ggsurgo database. The Maps 2 are derived from total soil carbon maps and estimations of each pool. The Maps 3 is interpolated from FIA data.
#   Baseflow and stormflow are approximated from soil texture

#load libraries
library("raster")
library("sf")
library("tidyverse")
library("spdep")
library("soiltexture")
library("ggplot2")

#clean up some garbage and remove loaded files. This script needs a lot of RAM
# rm(list = ls())
gc()

# get our boundary for the study area, to clip soil map to
# the CRS is EPSG:3857, WGS 84 Pseudo-Mercator
# we'll reproject it to match the SSURGO data
isro_boundary <- sf::st_read("./Parameterization/Parameterization data/isle_royale_boundary_buffer/isro_buffer.shp") %>%
  sf::st_transform(crs = "EPSG:5070")

# import SSURGO data and do some wrangling to allow us to extract soil data
# this is the polygon with all the map units, which we'll need to access component-
# and horizon-level data

# the CRS is ESRI:102039, equivalent to EPSG:5070. Conus Albers projection
# with some adjustments

#see what layers we've got
# sf::st_layers("./Parameterization/Parameterization data/ssurgo/gSSURGO_MI/gSSURGO_MI.gdb")

# the data structure is a little complicated. What is mapped are map units, 
# found in the "MUPOLYGON" layer (a polygon shape layer). Each map unit has
# components, which are different types of soils. Each soil type has several 
# horizons (e.g., O, A, B, etc.). So for many of the data, we'll have to 
# figure out what horizons are in what components and then combine them
# to get map unit level data (which we need to make a raster with).

# Some data are available "mapping-ready," in the "Valu1" layer. These data
# have already been aggregated to the map unit level and can be merged and 
# mapped directly. Sadly not many of them are very useful for us, so we'll have 
# to do it the hard way.

# gSSURGO does have a raster of the mapunits, which you can extract using ArcGIS,
# but for now the OpenFileGDB driver does not allow us to access the raster from R.
# Instead, we'll just use a polygon layer and then rasterize it later. This also
# has the advantage of being able to easily reproject the vector data to match 
# our project CRS (unless you're already using the USGS CRS EPSG:5070) without
# causing weird raster reprojection problems.

# data that comes from SSURGO includes:
# * Draiange = component:drainagecl
# * Wiltpoint= chorizon:wfifteenbar:r 
# * Feild capacity= chorizon:wthirdbar:r
# * Sand Percentage= Chorizon:sandtotal_R:RV 
# * Clay Percentage= Chorizon:claytotal_R:RV 
# * Soil depth = corestriction:resdept_r 


# functions --------------------------------------------------------------------
get_stormflow <- function(soil_type, slope_deg){
  if(is.na(soil_type) | is.na(slope_deg)){
    return(NA)
  } else{
    #see WetSpa manual
    slope_perc <- tan(slope_deg * (2*pi/360)) * 100
    slope_bins <- c(-1, 0.5, 5, 10, 1000)
    s_bin <- cut(slope_perc, slope_bins, labels = FALSE)
    
    soil_runoff_table <- data.frame(
      soil_type = c("Sand", "Loamy sand", "Sandy loam", "Loam", "Silt loam", "Silt", "Sandy clay loam", "Clay loam", "Silty clay loam", "Sandy clay", "Silty clay", "Clay"),
      s0 = c(0.68, 0.65, 0.62, 0.59, 0.56, 0.53, 0.5, 0.47, 0.44, 0.41, 0.38, 0.35)
    )
    
    soil_slope_table <- data.frame(
      soil_type = c("Sand", "Loamy sand", "Sandy loam", "Loam", "Silt loam", "Silt", "Sandy clay loam", "Clay loam", "Silty clay loam", "Sandy clay", "Silty clay", "Clay"),
      slope1 = c(0.03, 0.07, 0.10, 0.13, 0.17, 0.20, 0.23, 0.27, 0.30, 0.33, 0.37, 0.40),
      slope2 = c(0.07, 0.11, 0.14, 0.17, 0.21, 0.24, 0.27, 0.31, 0.34, 0.37, 0.41, 0.44),
      slope3 = c(0.13, 0.17, 0.20, 0.23, 0.27, 0.30, 0.33, 0.37, 0.40, 0.43, 0.47, 0.50),
      slope4 = c(0.25, 0.29, 0.32, 0.35, 0.39, 0.42, 0.45, 0.49, 0.52, 0.55, 0.59, 0.62)
    )
    
    s0 = soil_runoff_table[soil_runoff_table$soil_type == soil_type, "s0"]
    c0 = soil_slope_table[soil_slope_table$soil_type == soil_type, 2]
    c = soil_slope_table[soil_slope_table$soil_type == soil_type, s_bin]
    stormflow = stormflow <- c0 + (1 - c0)*(s / (s + s0))
    return(stormflow)
  }
}
#-------------------------------------------------------------------------------

#this is the grid we'll match everything to in the end
template_raster <- terra::rast("./Models/LANDIS inputs/input rasters/initial_communities_inv.tif")
crs(template_raster) #check that we're in NAD 83 Zone 17N

# to extract this data, we need to import the mapunit polygon (or raster),
# mapunits, the corestriction layer that has depth to bedrock or hardpan (or other
# restriction), and the chorizon layer that has horizon-level data
# Extracting this takes a while, so once you've done it once, save it for later.

if(!file.exists("./Parameterization/Parameterization data/ssurgo/mapunits.shp")){
  mapunits <- sf::st_read(dsn = "./Parameterization/Parameterization data/ssurgo/gSSURGO_MI/gSSURGO_MI.gdb",
                                layer = "MUPOLYGON") %>%
    sf::st_make_valid() %>% #there are some self-intersections or other invalid geometry
    sf::st_intersection(isro_boundary) #clip state data to our study area
  mapunits$OID <- 1:nrow(mapunits)
  
  sf::write_sf(mapunits, "./Parameterization/Parameterization data/ssurgo/mapunits.shp")
}

#after we've made the subset mapunits file, we can just read it in.
mapunits <- sf::read_sf("./Parameterization/Parameterization data/ssurgo/mapunits.shp")
mapunits <- sf::st_transform(mapunits, crs(template_raster))
plot(sf::st_geometry(mapunits)) #looks good!

#this has the component data; each map unit has several soil components/
# We only need the cokey and mukey, to connect to the horizon data,
# and comppct_r, which tells us what percentage of the map unit is comprised
# of what component. 
# We also need the "drainagecl" column which tells us the drainage type
# we need slope_r for later, to calculate stormflow

# some extra info we don't need
component_all <- sf::st_read(dsn = "./Parameterization/Parameterization data/ssurgo/gSSURGO_MI/gSSURGO_MI.gdb", 
                              layer = "component",
                              quiet = TRUE) %>%
  dplyr::filter(mukey %in% mapunits$MUKEY)

# select columns to make life easier
component <- component_all %>% dplyr::select(comppct_r, cokey, mukey, drainagecl, slope_r) 

#this has all the horizon data we need; each component (cokey) has several horizons
hori <- sf::st_read(dsn = "./Parameterization/Parameterization data/ssurgo/gSSURGO_MI/gSSURGO_MI.gdb", 
                layer = "chorizon",
                quiet = TRUE) %>%
  dplyr::filter(cokey %in% component$cokey)

valu1 <- sf::st_read(dsn = "./Parameterization/Parameterization data/ssurgo/gSSURGO_MI/gSSURGO_MI.gdb",
                     layer = "Valu1",
                     quiet = TRUE) %>%
  dplyr::filter(mukey %in% mapunits$MUKEY)

# the general idea here is to add new columns to the "component" dataframe,
# and then merge those columns with the mapunit layer. I'll go through in order,
# but there's probably a more elegant solution that has a list of variables, 
# and then extracts them all at the same time.The different sources of data
# make this sort of difficult, and probably not worth doing for this one-off.

#-------------------------------------------------------------------------------
# soil depth
# for soil depth, we want to use the component-level data provided in the 
# corestriction layer. This is the only data we need from this layer.
# Some components have more than one restriction: we only want to keep 
# the shallowest restriction.
rest <- sf::st_read(dsn = "./Parameterization/Parameterization data/ssurgo/gSSURGO_MI/gSSURGO_MI.gdb", 
                    layer = "corestrictions",
                    quiet = TRUE) %>%
  dplyr::filter(cokey %in% component$cokey) %>%
  dplyr::group_by(cokey) %>%
  dplyr::summarise(soilDepth = min(resdept_r), .groups = "keep")
  
# TODO refactor this in a tidier way
component <- dplyr::left_join(component, rest, by = "cokey")
component[is.na(component$soilDepth), "soilDepth"] <- 200 #if no restrictive layer, set soil depth to 2 m

# soil drainage
# the soil drainage map is used to modify rates of denitrification and soil carbon 
# respiration and leaching (it is used to calculate the "anaerobic effect"). 
# It is a proxy for more proximate variables like water-filled pore
# space, incorporating the general tendency for poorly-drained soils to accumulate
# carbon and lose nitrogen.
# Here, I use SSURGO data layers describing how well-drained soils are, and 
# use a lookup table to assign polygons a value of SoilDrain. SSURGO gives the 
# full range of drainage values as Excessively drained to Very poorly drained,
# which I map onto the range from 1 to 0 (with 0 being poorly drained). 

drain_lookup <- read.csv("./Parameterization/Parameterization data/necn_parameterize/soil drainage/drain_coef_lookup_2021-09-16.csv")
component$soilDrain <- drain_lookup$drain_coef[base::match(component$drainagecl, drain_lookup$Drainage_class)]

# For the rest of the SSURGO data, we need to extract the proper data from each horizon, aggregate to the 
# component, and then aggregate to the map unit. Component -> mapunit we aggregate
# using the component percent (comppct_r is the "representative" value of comppct),
# for horizon -> component we have to be more creative depending on the variable of interest

# field capacity
#found here: hori$wthirdbar_r

# wilting point
#found here: hori$wfifteenbar_r

# percent sand
#found here: hori$sandtotal_r

# percent clay
#found here: hori$claytotal_r

# base flow
# base flow represents the proportion of water that percolates to deep horizons, 
# or percentage of excess water (above field capacity) in Henne equations.

#take the weighted average of variables by the horizon thickness for each component, join with components
component <- hori %>% 
  dplyr::mutate(hzthk_r = ifelse(!is.na(hzthk_r), hzthk_r, hzdepb_r - hzdept_r)) %>%  #there are a few NAs for thickness; replace with the depth to top minus depth to bottom
  dplyr::select(cokey, hzthk_r, wthirdbar_r, wfifteenbar_r, sandtotal_r, claytotal_r, ksat_r) %>%
  dplyr::group_by(cokey) %>%
  dplyr::summarise(across(where(is.numeric), 
                   ~stats::weighted.mean(., w = hzthk_r, na.rm = TRUE))) %>%
  dplyr::mutate(across(wthirdbar_r:claytotal_r, ~replace_na(.x, replace = 1.0))) %>% #replace NAs where any are left -- just on rocky outcrops and beaches. Replace with 1 instead of 0 so that the sites can still be active
  dplyr::mutate(across(c(wthirdbar_r:claytotal_r), .fns = ~ `*`(.x, 0.01))) %>% #multiply some columns by 0.01 to convert from percent to proportion
  dplyr::right_join(component, by = "cokey") %>%
  dplyr::mutate(across(where(is.double), ~replace_na(.x, 0.001))) %>% #TODO check out what's weird with the last three entries -- no horizon data?
  dplyr::mutate(across(where(is.integer), ~as.double(replace_na(.x, 1)))) %>% #this is dumb -- dealing with new tidyr issue
  dplyr::mutate(wthirdbar_r = ifelse(wthirdbar_r <= wfifteenbar_r, wfifteenbar_r + 0.02, wthirdbar_r)) #fix a few sites where field capacity is less than wilt point for some reason
  
#add our extracted data to the mapunit layer
mapunits_data <- component %>%
  dplyr::group_by(mukey) %>%
  dplyr::summarise(across(c(wthirdbar_r, wfifteenbar_r, sandtotal_r, claytotal_r, ksat_r,
                            soilDepth, soilDrain, slope_r), 
                          ~stats::weighted.mean(., w = comppct_r, na.rm = TRUE))) %>%
  dplyr::right_join(mapunits, by = c("mukey" = "MUKEY")) %>%
  dplyr::rename(MUKEY = mukey) %>%
  sf::st_sf() #we lost the geometry at some point, so this function fixes it. Seems clunky though

# SOM
# this is one variable available from the Valu1 table!
# it's in units of g m-2, already the scale we need
mapunits_data <- left_join(mapunits_data, 
                           dplyr::select(valu1, c("mukey", "soc0_999", "soc0_150")), 
                           by = c("MUKEY" = "mukey")) %>%
  dplyr::mutate(mapunits_data, soc0_999 = ifelse(soc0_999 <= 0 | is.na(soc0_999), 100, soc0_999),
                mapunits_data, soc0_150 = ifelse(soc0_150 <= 0 | is.na(soc0_150), 100, soc0_150)) %>%
  sf::st_sf()

#use relationships from Zachary Robbins to divvy up the SOM to different pools
mapunits_data$SOM1surfC <- 0.01 * mapunits_data$soc0_150
mapunits_data$SOM1soilC <- 0.02 * mapunits_data$soc0_150
mapunits_data$SOM2C <- 0.59 * mapunits_data$soc0_150
mapunits_data$SOM3C <- 0.38 * mapunits_data$soc0_150

#wetland soils have more recalcitrant carbon 
mapunits_data[mapunits_data$soilDrain < 0.1, ]$SOM2C <- 0.4 * mapunits_data[mapunits_data$soilDrain < 0.1, ]$soc0_150
mapunits_data[mapunits_data$soilDrain < 0.1, ]$SOM3C <- 0.57 * mapunits_data[mapunits_data$soilDrain < 0.1, ]$soc0_150

# calculate baseFlow
# if using the original (not Henne version) water balance model, then baseflow
# is important for reducing soil moisture. It's not a physical parameter that is
# typically measured; the term is typically applied to hydrographs for streams,
# rather than to individual plots of soil. In NECN, it's comparable to percolation
# of water to horizons deeper than the rooting zone, groundwater recharge, or
# some lateral flow (though some lateral flow would also come into the cell, cancelling
# out some of the flow out of the cell). Because NECN does not have any lateral flow,
# I used baseflow to simulate the propensity for upland soils to lose water and 
# bottomlands to gain water. Wetlands in particular will have a very low baseflow
# to prevent them from drying out. If baseflow is 0, then the only drying process is
# evapotranspiration, so bottomlands in a wet area might stay permanently wet. 

# I'm taking a page from the TOPMODEL approach, and using the area upslope of each
# cell and the slope steepness to calculate a topographic index, which is more or
# less proportional to soil moisture (Riihimaki et al. 2021). 

# tci = ln(a/tan(s)), where a = upslope area and s = slope steepness
# Riihimäki, H., J. Kemppinen, M. Kopecký, and M. Luoto. 2021. Topographic Wetness Index as a Proxy for Soil Moisture: The Importance of Flow-Routing Algorithm and Grid Resolution. Water Resources Research 57:e2021WR029871.

library("rgrass7")

baseflow_max <- 0.5
baseflow_min <- 0

library("sf")
library("openSTARS")
library("terra")

grass_program_path <- "C:/Program Files/GRASS GIS 7.8"

mask <- terra::rast("C:/Users/Sam/Documents/Research/Isle Royale/Models/LANDIS inputs/input rasters/initial_communities_inv.tif")

rname <- 'C:/Users/Sam/Documents/Research/Isle Royale/Parameterization/Parameterization data/dem/dem_ir_60m.tif'
r <- terra::rast(rname)

initGRASS(gisBase= grass_program_path, #where the GRASS program lives; folder with bin and lib subfolders
          gisDbase='C:/Users/Sam/Documents/Research/Isle Royale/Parameterization/Parameterization data/',  #where to open GRASS
          location = 'dem', #subdirectory -- working directory. Not sure what the difference is between this, home, and gisDbase
          mapset='PERMANENT',
          SG = r,
          override = TRUE)

#change CRS to match DEM raster (I think there's a better way to do this by initializing GRASS with the right DEM?)
execGRASS("g.proj", flags = "c", epsg = as.numeric(crs(r, describe = TRUE)$code))

# Import raster to GRASS and set region
execGRASS("r.in.gdal", flags="o", parameters=list(input=rname, output="elev_rast")) #import DEM raster located at "input", and add as "output"
execGRASS("g.region", parameters=list(raster="elev_rast") ) #set region to match DEM
execGRASS("g.region", flags = "p") #check on the CRS

# calculate watershed statistics, including topographic index
execGRASS('r.watershed', flags='overwrite', 
          parameters = list(elevation='elev_rast', #name of elevation raster in GRASS
                            threshold=2000,
                            tci = "topographic_index",
                            slope_steepness = "slope_steepness")) #name of tci output in GRASS

#import from GRASS to R as a terra raster
tci <- read_RAST(vname = "topographic_index", cat=NULL, NODATA=NULL, ignore.stderr=get.ignore.stderrOption(),
                 return_format="terra", close_OK=TRUE, flags=NULL)
plot(tci)

hist(values(tci))

#scale tci to baseflow min and max, assuming that everything over 15 is a wetland
baseflow <- ifelse(values(tci) > 15, baseflow_min,
                   (1 - values(tci)/15) * (baseflow_max - baseflow_min))
hist(baseflow)                   

#higher TCI -> higher water content -> less baseflow

baseflow_rast <- tci
values(baseflow_rast) <- baseflow
values(baseflow_rast)[values(baseflow_rast) < 0.01] <- 0.01
values(baseflow_rast)[values(baseflow_rast) > 1] <- 1

# calculate slope steepness
#We'll use this for stormflow later on
execGRASS('r.slope.aspect', flags='overwrite', 
          parameters = list(elevation='elev_rast', #name of elevation raster in GRASS
                            slope = "slope_steepness")) #name of tci output in GRASS

#import from GRASS to R as a terra raster
slope <- read_RAST(vname = "slope_steepness", cat=NULL, NODATA=NULL, ignore.stderr=get.ignore.stderrOption(),
                 return_format="terra", close_OK=TRUE, flags=NULL)
plot(slope)
hist(values(slope))
crs(slope)

#-------------------------------
# storm flow
# storm flow represents the proportion of water that runs off when soil water exceeds
# field capacity. In original equations, stormflow happens to excess water while 
# base flow happens only to all water; in Henne equations, both base and storm flow
# happen only to excess water.

# For stormflow (runoff), use Table 3.4 and eq. 3.2 in https://www.vub.be/WetSpa/downloads/WetSpa_manual.pdf
soil_s0_table <- data.frame(
  soil_type = c("Sand", "Loamy sand", "Sandy loam", "Loam", "Silt loam", "Silt", "Sandy clay loam", "Clay loam", "Silty clay loam", "Sandy clay", "Silty clay", "Clay"),
  code = 1:12,
  s0 = c(0.68, 0.65, 0.62, 0.59, 0.56, 0.53, 0.5, 0.47, 0.44, 0.41, 0.38, 0.35)
)

soil_slope_table <- data.frame(
  soil_type = c("Sand", "Loamy sand", "Sandy loam", "Loam", "Silt loam", "Silt", "Sandy clay loam", "Clay loam", "Silty clay loam", "Sandy clay", "Silty clay", "Clay"),
  soil_type_short = c("Sa", "LoSa", "SaLo", "Lo", "SiLo", "Si", "SaClLo", "ClLo", "SiClLo", "SaCl", "SiCl", "Cl"),
  code = 1:12,
  slope1 = c(0.03, 0.07, 0.10, 0.13, 0.17, 0.20, 0.23, 0.27, 0.30, 0.33, 0.37, 0.40),
  slope2 = c(0.07, 0.11, 0.14, 0.17, 0.21, 0.24, 0.27, 0.31, 0.34, 0.37, 0.41, 0.44),
  slope3 = c(0.13, 0.17, 0.20, 0.23, 0.27, 0.30, 0.33, 0.37, 0.40, 0.43, 0.47, 0.50),
  slope4 = c(0.25, 0.29, 0.32, 0.35, 0.39, 0.42, 0.45, 0.49, 0.52, 0.55, 0.59, 0.62)
)


#classify soil texture
mapunits_data$OID <- 1:nrow(mapunits_data)

soil_mat <- data.frame(OID = mapunits_data$OID,
                       SAND = mapunits_data$sandtotal_r*100,
                       CLAY = mapunits_data$claytotal_r*100,
                       SILT = 100 - mapunits_data$sandtotal_r*100 - mapunits_data$claytotal_r*100) %>%
  na.omit()

#classify soils by texture
texture <- data.frame(soiltexture::TT.points.in.classes(tri.data = soil_mat, class.sys = "USDA.TT"))

names(texture)  <- soil_slope_table[match(names(texture), soil_slope_table$soil_type_short), "code"]

#get the integer code for the soil class
for(i in 1:nrow(texture)){
  soil_mat$class[i] <- as.numeric(names(texture)[which(texture[i, ] %in% c(1,2,3))])
}
#join data
mapunits_data <- left_join(mapunits_data, soil_mat[c("OID", "class")], by = c("OID"))
mapunits_data$class <- as.numeric(mapunits_data$class)

#rasterize the soil class data
class_rast <- terra::rasterize(terra::vect(mapunits_data), rast(template_raster), field = "class")

s0_rast <- terra::classify(class_rast, rcl = soil_s0_table[, c(2,3)])

slope_percent <- terra::app(slope, function(x) tan(x * (2*pi/360)) * 100)

#bin into groups 1:4
slope_bin <- terra::classify(slope_percent, c(-1, 0.5, 5, 10, 1000)) + 1

s_rast <- slope_percent
c0_rast <- slope_percent
# this is annoying and surely the wrong way to do this, but we need to get a single value for s depending on
# the slope and soil type. Probably can be done with terra::app and the right function, but I couldn't figure it out
for(i in 1:4){
  values(s_rast)[which(values(slope_bin) == i)] <- soil_slope_table[values(class_rast)[which(values(slope_bin) == i)], i+3]
  values(c0_rast)[which(values(slope_bin) == i)] <- soil_slope_table[values(class_rast)[which(values(slope_bin) == i)], 4]
}

stormflow_rast <- c0_rast + (1-c0_rast) * (s_rast / (s_rast + s0_rast))
values(stormflow_rast)[values(stormflow_rast) < 0.01] <- 0.01
values(stormflow_rast)[values(stormflow_rast) > 1] <- 1
plot(stormflow_rast)
hist(stormflow_rast)
#-------------------------------------------------------------------------------
# Soil Nitrogen
#-------------------------------------------------------------------------------
# nitrogen pools
#at the moment, none of this fancy stuff works. Instead, I'm just using 
# a relationship using C:N ratios in each horizon, from Melissa Lucash



#-------------------------------------------------------------------------------
#create the soil N maps using soil C and C:N ratio

# Carbon is apportioned like so: 
# SOM1surfC = 0.01
# SOM1soilC = 0.02 
# SOM2C = 0.59 
# SOM3C = 0.38 

# Nitrogen as a proportion of carbon, from Zachary or Melissa TODO find where!
# SOM1surfN=.1
# SOM1soilN=.1
# SOM2N=.04
# SOM3N=.118
# instead, let's divvy up N using the same proportions, but using soil N from soilgrids

# From a previous model run on this landscape, N was a somewhat lower proportion on average
# than those numbers above:
mapunits_data$SOM1surfN <- mapunits_data$SOM1surfC * 0.015 * 0.75
mapunits_data$SOM1soilN <- mapunits_data$SOM1soilC * 0.099 * 0.75
mapunits_data$SOM2N <- mapunits_data$SOM2C * 0.052 * 0.75
mapunits_data$SOM3N <- mapunits_data$SOM3C * 0.073 * 0.75

#We can also use a regression from Ross et al. 2011 for surface C:N
# mapunits_data$SOM1surfN <- mapunits_data$SOM1surfC * 0.03 + 6.9
# mapunits_data$SOM1surfN <- ifelse(mapunits_data$SOM1surfC/mapunits_data$SOM1surfN < 10, 
#                                   mapunits_data$SOM1surfC * (1/10),
#                                   mapunits_data$SOM1surfN)
# mapunits_data$SOM1soilN <- mapunits_data$SOM1soilC * 0.03 + 6.9
# mapunits_data$SOM1soilN <- ifelse(mapunits_data$SOM1soilC/mapunits_data$SOM1soilN < 10, 
#                                   mapunits_data$SOM1soilC * (1/10),
#                                   mapunits_data$SOM1soilN)
#------------------



mapunits_data <- sf::st_transform(mapunits_data, "EPSG:26917") #reproject to NAD83 Zone 17N

#check CN ratios
hist(mapunits_data$SOM1surfC/mapunits_data$SOM1surfN)
hist(mapunits_data$SOM1soilC/mapunits_data$SOM1soilN)
hist(mapunits_data$SOM2C/mapunits_data$SOM2N)
hist(mapunits_data$SOM3C/mapunits_data$SOM3N)


#fix field capacity for some poorly drained sites
mapunits_data[mapunits_data$soilDrain < 0.1 & mapunits_data$wthirdbar_r < 0.4, 
              "wthirdbar_r"] <- 0.4
mapunits_data[mapunits_data$soilDrain < 0.1 & mapunits_data$wfifteenbar_r < 0.05,
              "wfifteenbar_r"] <- 0.05


#-------------------------------------------------------------------------------
# write rasters!
# this part might take a few hours depending on your study area, raster resolution, etc.
# TODO: rasterize once with mapunit, then match values to cells from mapunit ID. Should be waaaay faster, we'll just lose
# the ability to average mapunit values within a cell (i.e. just one mapunit ID per cell). But we have small cells
# so it shouldn't be much of a problem.

template_raster <- terra::rast("./Models/LANDIS inputs/input rasters/initial_communities_inv.tif")
crs(template_raster) #check that we're in NAD 83 Zone 17N

#TODO make into a loop to extract everything, since it all comes from the same dataframe
field_capacity <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "wthirdbar_r", fun="mean")
values(field_capacity) <- ifelse(is.na(values(field_capacity)), 0, values(field_capacity))
terra::writeRaster(field_capacity, "./Models/LANDIS inputs/input rasters/field_capacity.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

wilt_point <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "wfifteenbar_r", fun="mean")
values(wilt_point) <- ifelse(is.na(values(wilt_point)), 0, values(wilt_point))
terra::writeRaster(wilt_point, "./Models/LANDIS inputs/input rasters/wilt_point.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

sand <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "sandtotal_r", fun="mean")
values(sand) <- ifelse(is.na(values(sand)), 0, values(sand))
values(sand) <- ifelse(values(sand) > 1, 1, values(sand))
terra::writeRaster(sand, "./Models/LANDIS inputs/input rasters/sand.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

clay <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "claytotal_r", fun="mean")
values(clay) <- ifelse(is.na(values(clay)), 0, values(clay))
terra::writeRaster(clay, "./Models/LANDIS inputs/input rasters/clay.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

soilDrain <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "soilDrain", fun="mean")
values(soilDrain) <- ifelse(is.na(values(soilDrain)), 0, values(soilDrain))
terra::writeRaster(soilDrain, "./Models/LANDIS inputs/input rasters/soil_drain.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

soilDepth <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "soilDepth", fun="mean")
values(soilDepth) <- ifelse(is.na(values(soilDepth)), 0, values(soilDepth))
terra::writeRaster(soilDepth, "./Models/LANDIS inputs/input rasters/soil_depth.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

# baseflow <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "baseFlow", fun="mean") 
# values(baseflow) <- ifelse(is.na(values(baseflow)), 0, values(baseflow))
baseflow_rast <- terra::project(baseflow_rast, template_raster)
values(baseflow_rast) <- ifelse(is.na(values(baseflow_rast)), 0, values(baseflow_rast))
terra::writeRaster(baseflow_rast, "./Models/LANDIS inputs/input rasters/baseflow.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

# stormflow <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "runoff", fun="mean") 
# values(stormflow) <- ifelse(is.na(values(stormflow)), 0, values(stormflow))
stormflow_rast <- terra::project(stormflow_rast, template_raster)
values(stormflow_rast) <- ifelse(is.na(values(stormflow_rast)), 0, values(stormflow_rast))
terra::writeRaster(stormflow_rast, "./Models/LANDIS inputs/input rasters/stormflow.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM1surfC <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM1surfC", fun="mean") 
values(SOM1surfC) <- ifelse(is.na(values(SOM1surfC)), 0, values(SOM1surfC))
terra::writeRaster(SOM1surfC, "./Models/LANDIS inputs/input rasters/SOM1surfC.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM1soilC <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM1soilC", fun="mean") 
values(SOM1soilC) <- ifelse(is.na(values(SOM1soilC)), 0, values(SOM1soilC))
terra::writeRaster(SOM1soilC, "./Models/LANDIS inputs/input rasters/SOM1soilC.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM2C <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM2C", fun="mean") 
values(SOM2C) <- ifelse(is.na(values(SOM2C)), 0, values(SOM2C))
values(SOM2C) <- ifelse(values(SOM2C) >= 25000, 24499, values(SOM2C)) #TODO fix this
terra::writeRaster(SOM2C, "./Models/LANDIS inputs/input rasters/SOM2C.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM3C <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM3C", fun="mean")
values(SOM3C) <- ifelse(is.na(values(SOM3C)), 0, values(SOM3C))
values(SOM3C) <- ifelse(values(SOM3C) >= 20000, 19999, values(SOM3C)) #TODO fix this
terra::writeRaster(SOM3C, "./Models/LANDIS inputs/input rasters/SOM3C.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM1surfN <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM1surfN", fun="mean") %>%
  terra::mask(template_raster, inverse = FALSE, maskvalues = c(NA, 0), updatevalue = 0)
terra::NAflag(SOM1surfN) <- 0
# values(SOM1surfN) <- ifelse(is.na(values(SOM1surfN)), 100, values(SOM1surfN))
values(SOM1surfN) <- ifelse(values(SOM1surfN) >= 500, 499, values(SOM1surfN)) #TODO fix this
terra::writeRaster(SOM1surfN, "./Models/LANDIS inputs/input rasters/SOM1surfN.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM1soilN <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM1soilN", fun="mean") %>%
  terra::mask(template_raster, inverse = FALSE, maskvalues = c(NA, 0), updatevalue = 0)
terra::NAflag(SOM1surfN) <- 0
# values(SOM1soilN) <- ifelse(is.na(values(SOM1soilN)), 0, values(SOM1soilN))
values(SOM1soilN) <- ifelse(values(SOM1soilN) >= 500, 499, values(SOM1soilN)) #TODO fix this
terra::writeRaster(SOM1soilN, "./Models/LANDIS inputs/input rasters/SOM1soilN.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM2N <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM2N", fun="mean") %>%
  terra::mask(template_raster, inverse = FALSE, maskvalues = c(NA, 0), updatevalue = 0)
terra::NAflag(SOM2N) <- 0
# values(SOM1soilN) <- ifelse(is.na(values(SOM1soilN)), 0, values(SOM1soilN))
values(SOM2N) <- ifelse(values(SOM2N) >= 1000, 999, values(SOM2N)) #TODO fix this
terra::writeRaster(SOM2N, "./Models/LANDIS inputs/input rasters/SOM2N.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM3N <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM3N", fun="mean") %>%
  terra::mask(template_raster, inverse = FALSE, maskvalues = c(NA, 0), updatevalue = 0)
terra::NAflag(SOM3N) <- 0
# values(SOM1soilN) <- ifelse(is.na(values(SOM1soilN)), 0, values(SOM1soilN))
values(SOM3N) <- ifelse(values(SOM3N) >= 1000, 999, values(SOM3N)) #TODO fix this
terra::writeRaster(SOM3N, "./Models/LANDIS inputs/input rasters/SOM3N.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)



#-------------------------------------------------------------------------------
# Another attempt at getting the N layers:

# # for N, we will use nitrogen data and carbon data from soilgrids.org to get the C:N ratio
# # and translate our soil C data from SSURGO to soil N
# # these need to be changed to g m-2
# 
# # these use a weird projection, Homolosine, EPSG:7030
# 
# #units are in dg/kg or cg/kg -- that is, they are concentrations, not per m2
# 
# c_rasts <- c("c0-5.tif", "c5-15.tif", "c15-30.tif", "c30-60.tif", "c60-100.tif", "c100-200.tif") %>%
#   paste0("./Parameterization/Parameterization data/soilgrids/", .) #carbon in dg/kg 
# n_rasts <- c("n0-5.tif", "n5-15.tif", "n15-30.tif", "n30-60.tif", "n60-100.tif", "n100-200.tif") %>%
#   paste0("./Parameterization/Parameterization data/soilgrids/", .) #nitrogen in cg/kg
# bulk_dens_rasts <- c("bd0-5.tif", "bd5-15.tif", "bd15-30.tif", "bd30-60.tif", "bd60-100.tif", "bd100-200.tif") %>%
#   paste0("./Parameterization/Parameterization data/soilgrids/", .) #bulk density in cg/cm3
# 
# #TODO refactor with tidyverse
# c_stack <- raster::stack(c_rasts) / 10 /1000 #convert from dg C per kg soil to g N per g soil
# c_rast <- sum(c_stack) #total carbon
# 
# n_stack <- raster::stack(n_rasts) / 100 / 1000 #convert from cg N per kg soil to g N per g soil
# n_rast <- sum(n_stack) #total nitrogen
# 
# bd_stack <- raster::stack(bulk_dens_rasts) /100 # convert cg/cm3 to g/cm3
# bd_rast <- mean(bd_stack) #average bulk density
# 
# #convert from per mass to per volume basis
# c_bd_stack <- c_stack * bd_stack
# n_bd_stack <- n_stack * bd_stack
# 
# # because we have calculated C and N bulk densities (i.e. per cm3), we need
# # to translate to density (per cm2) by multiplying by soil depth (in cm).
# 
# # To know how many layers we want for each variable, 
# # we need to rasterize the soil depth polygons we created before
# # template_raster <- raster::raster("./Models/LANDIS inputs/input rasters/initial_communities.tif")
# # NAvalue(template_raster) <- 0
# # crs(template_raster) #check that we're in NAD 83 Zone 17N
# # mapunits2 <- sf::st_transform(mapunits_data, "EPSG:26917") #reproject to NAD83 Zone 17N, same as the initial communities
# # soilDepth <- fasterize::fasterize(mapunits2, template_raster, field = "soilDepth", fun="max")
# # soilDepth <- raster::mask(soilDepth, template_raster) #remove the water/nonforested cells
# # 
# # plot(soilDepth)
# # rm(mapunits2)
# 
# # coarsen and reproject to match soilgrids data
# # soilDepth_coarse  <- raster::projectRaster(soilDepth, c_stack, method = "ngb")
# 
# #set mapunits to match the soil rasters
# mapunits <- sf::st_transform(mapunits, crs(c_stack)) 
# 
# c_extract <- raster::extract(c_bd_stack, mapunits, fun = mean, weights = TRUE)
# n_extract <- raster::extract(n_bd_stack, mapunits, fun = mean, weights = TRUE)
# 
# # We have multiple depths for each variable, but many are deeper than soil depths
# # From SSURGO. Maybe representing bedrock? In any case, they aren't likely to be
# # relevant to our model. 
# n_layers <- as.numeric(cut(mapunits_data$soilDepth, breaks = c(0, 15, 30, 60, 100, 200)))
# weights <- c(15, 15, 30, 40, 50) #weight each layer by its depth. Reduce last layer weight to 50 for 150 cm depth
# 
# mapunits$c_total <- NA
# mapunits$n_total <- NA
# mapunits$cn_ratio <- NA
# 
# for(i in 1:nrow(mapunits)){
#   #for each mapunit, take the weighted mean of the layers that are relevant. Weight by depth of each layer
#   mapunits$c_total[i] <- weighted.mean(c_extract[i, n_layers[i]], weights = weights[1:nlayers[i]]) %>%
#     `*`(mapunits_data$soilDepth[i]) %>% #multiple by depth to convert cm3 to cm2
#     `*`(10000) #convert from cm2 to m2
#   mapunits$n_total[i] <- weighted.mean(n_extract[i, n_layers[i]], weights = weights[1:nlayers[i]]) %>%
#     `*`(mapunits_data$soilDepth[i]) %>% #multiple by depth to convert cm3 to cm2
#     `*`(10000) #convert from cm2 to m2
# }
# 
# mapunits$cn_ratio <- mapunits$c_total/mapunits$n_total
# 
# #get average values for the mapunits, from soilgrids data:
# mapunit_n <- mapunits[,"n_total"] %>%
#   sf::st_set_geometry(NULL) %>%
#   stats::aggregate(by = list(mapunits$MUKEY), FUN = function(x){mean(x, na.rm = TRUE)}) %>%
#   dplyr::rename("MUKEY" = "Group.1")
# mapunit_c <- mapunits[,"c_total"] %>%
#   sf::st_set_geometry(NULL) %>%
#   stats::aggregate(by = list(mapunits$MUKEY), FUN = function(x){mean(x, na.rm = TRUE)}) %>%
#   dplyr::rename("MUKEY" = "Group.1")
# mapunit_cn <- mapunits[,"cn_ratio"] %>%
#   sf::st_set_geometry(NULL) %>%
#   stats::aggregate(by = list(mapunits$MUKEY), FUN = function(x){mean(x, na.rm = TRUE)}) %>%
#   dplyr::rename("MUKEY" = "Group.1")
# 
# 
# #compare soil C from SSURGO vs from soilgrids
# #soilgrids
# ggplot() + 
#   geom_sf(mapping = aes(colour = c_total, fill = c_total), data = mapunits)
# #ssurgo
# ggplot() + 
#   geom_sf(mapping = aes(colour = soc0_999, fill = soc0_999), data = mapunits_data)
# ggplot() + 
#   geom_sf(mapping = aes(colour = soc0_150, fill = soc0_150), data = mapunits_data)
# ggplot() + 
#   geom_sf(mapping = aes(colour = soilDrain, fill = soilDrain), data = mapunits_data)
# 
# 
# #compare soilgrids and ssurgo
# #from soilgrids; looks pretty reasonable:
# hist(mapunits$cn_ratio)
# 
# #calculate cn from ssurgo
# mapunits$cn_ratio_ssurgo <- mapunits_data$soc0_150 / mapunits$n_total
# hist(mapunits$cn_ratio_ssurgo) #some crazy high values here -- what's with the soc0_999 value?
# 
# #wetlands have crazy high CN ratios; more than plausible
# mapunits$cn_ratio_ssurgo[mapunits$cn_ratio_ssurgo > 60] <- 60
# # some ratios are way too low; set to a more reasonable number. See Ross et al. 2010
# mapunits$cn_ratio_ssurgo[mapunits$cn_ratio_ssurgo < 11.6] <- 11.6
# 
# # Fill in NA values
# 
# # #for NA values of cn_ratio_ssurgo, add the average of the other mapunits with the same MUKEY
# mapunits[is.na(mapunits$cn_ratio_ssurgo), "cn_ratio_ssurgo"] <- 
#   mapunit_cn[match(mapunits$MUKEY, mapunit_cn$MUKEY), "cn_ratio"] %>%
#   subset(is.na(mapunits$cn_ratio_ssurgo))
# 
# 
# # for some other NAs, the whole mapunit is NA -- for these, average the values for the neighboring
# # polygons and set the cn equal to that
# neighbors <- spdep::poly2nb(mapunits) #get neighbors for each mapunit
# neighbor_list <- neighbors
# neighbor_cn <- NA
# for(i in 1:nrow(mapunits)){
#   #average the neighbors' CN for each shape
#   neighbor_cn[i] <- mean(mapunits$cn_ratio_ssurgo[neighbor_list[[i]]], na.rm = TRUE)
# }
# 
# #assign values from neighbors
# mapunits$cn_ratio_ssurgo <- ifelse(is.na(mapunits$cn_ratio_ssurgo), neighbor_n, mapunits$cn_ratio_ssurgo)
# 
# #for the remaining 4 polygons, assign a reasonable value from Ross et al. 2011
# mapunits[is.na(mapunits$cn_ratio_ssurgo), "cn_ratio_ssurgo"] <- 18
# mapunits[mapunits$cn_ratio_ssurgo == 0, "cn_ratio_ssurgo"] <- 18
# 
# 
# # re-calculate n from SSURGO soil carbon and soilgrids N
# mapunits$n_adjusted <- mapunits_data$soc0_150 / mapunits$cn_ratio_ssurgo
# plot(mapunits$n_adjusted ~ mapunits$n_total)
# 
# mapunits[is.na(mapunits$n_adjusted), ]
# 
# ggplot() + 
#   geom_sf(mapping = aes(colour = n_adjusted, fill = n_adjusted), data = mapunits)
# ggplot() + 
#   geom_sf(mapping = aes(colour = cn_ratio_ssurgo, fill = cn_ratio_ssurgo), data = mapunits)
# 
# hist(mapunits$cn_ratio_ssurgo)

# # Using N directly from soilgrids, then distributing N between layers
# 
# mapunits_data$SOM1surfN <- mapunits$n_adjusted * (0.015 * 0.01) / 0.06055
# mapunits_data$SOM1soilN <- mapunits$n_adjusted * (0.099 * 0.02)/ 0.06055
# mapunits_data$SOM2N <- mapunits$n_adjusted * (0.052 * 0.59)/ 0.06055
# mapunits_data$SOM3N <- mapunits$n_adjusted * (0.073 * 0.38)/ 0.06055
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
# BaseFlow and Storm Flow are treated as stationary variables in this simulation. 

#load libraries
library("raster")
library("sf")
library("tidyverse")
library("spdep")
library("soiltexture")
library("ggplot2")
# library("fasterize") #fasterize doesn't have "mean" as a built-in function -- try to figure this out?

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
# the soil drainage map is used to modify rates of denitrification and carbon 
# respiration. It is a proxy for more proximate variables like water-filled pore
# space, incorporating the general tendency for poorly-drained soils to accumulate
# carbon and lose nitrogen.
# Here, I use SSURGO data layers describing how well-drained soils are, and 
# use a lookup table to assign polygons a value of SoilDrain. SSURGO gives the 
# full range of drainage values as Excessively drained to Very poorly drained,
# which I map onto the range from 1 to 0 (with 0 being poorly drained). 
#TODO validate this

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
# use saturated hydraulic conductivity from SSURGO
# head(hori$ksat_r)

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

# calculate baseFlow
# assume a 2 inch rain event = 5 cm; how much of it would be absorbed in an hour?
# I'll take this value to be the baseflow rate
# TODO validate this, I guess

# ksat is in units of um per second
# 5 cm * (1 second / x um) = seconds
# 3600 seconds in 1 hour

# ksat/10000 #convert to cm

mapunits_data$baseFlow <- (mapunits_data$ksat_r/10000) * 3600  #how much water can be absorbed in an hour?
mapunits_data$baseFlow <- mapunits_data$baseFlow / 5 #What proportion of the 5cm rainfall can be absorbed?
mapunits_data$baseFlow <- ifelse(mapunits_data$baseFlow > 1, 1, mapunits_data$baseFlow) #if more than 100%, set to 100%

# storm flow
# storm flow represents the proportion of water that runs off when soil water exceeds
# field capacity. In original equations, stormflow happens to excess water while 
# base flow happens only to all water; in Henne equations, both base and storm flow
# happen only to excess water.


# For stormflow (runoff), use Table 3.4 and eq. 3.2 in https://www.vub.be/WetSpa/downloads/WetSpa_manual.pdf

mapunits_data$OID <- 1:nrow(mapunits_data)

soil_runoff_table <- data.frame(
  soil_type = c("Sand", "Loamy sand", "Sandy loam", "Loam", "Silt loam", "Silt", "Sandy clay loam", "Clay loam", "Silty clay loam", "Sandy clay", "Silty clay", "Clay"),
  c0 = c(0.03, 0.07, 0.1, 0.13, 0.17, 0.20, 0.23, 0.27, 0.3, 0.33, 0.37, 0.4),
  s0 = c(0.68, 0.65, 0.62, 0.59, 0.56, 0.53, 0.5, 0.47, 0.44, 0.41, 0.38, 0.35)
  )

soil_mat <- data.frame(OID = mapunits_data$OID,
                       SAND = mapunits_data$sandtotal_r,
                       CLAY = mapunits_data$claytotal_r,
                       SILT = 100 - mapunits_data$sandtotal_r - mapunits_data$claytotal_r) %>%
  na.omit()

#classify soils by texture
texture <- soiltexture::TT.points.in.classes(soil_mat, class.sys = "USDA.TT")

names(texture) <- c("Clay", "Silty clay", "Sandy clay", "Clay loam", "Silty clay loam", "Sandy clay loam", "Loam", "Silt loam", "Sandy loam", "Silt", "Loamy sand", "Sand")

# soil_mat$class <- unlist(apply(texture, 1, function(x) unlist(names(texture)[which(x == 1)])))

for(i in 1:nrow(texture)){
  soil_mat$class[i] <- names(texture)[which(texture[i, ] %in% c(1,2,3))]
}

mapunits_data <- left_join(mapunits_data, soil_mat[c("OID", "class")], by = c("OID"))

# mapunits_data$slope <- component_all$slope_r # TODO update with slope from DEM

c0 <- soil_runoff_table[base::match(mapunits_data$class, soil_runoff_table$soil_type), "c0"]
s0 <- soil_runoff_table[base::match(mapunits_data$class, soil_runoff_table$soil_type), "s0"]
s <- mapunits_data$slope_r
mapunits_data$runoff <- c0 + (1 - c0)*(s / (s + s0))

#-------------------------------------------------------------------------------
# Soil Nitrogen
#-------------------------------------------------------------------------------
# nitrogen pools
#at the moment, none of this fancy stuff works. Instead, I'm just using 
# a relationship using C:N ratios in each horizon, from Melissa Lucash

# for N, we will use nitrogen data and carbon data from soilgrids.org to get the C:N ratio
# and translate our soil C data from SSURGO to soil N
# these need to be changed to g m-2

# these use a weird projection, Homolosine, EPSG:7030

#units are in dg/kg or cg/kg -- that is, they are concentrations, not per m2

c_rasts <- c("c0-5.tif", "c5-15.tif", "c15-30.tif", "c30-60.tif", "c60-100.tif", "c100-200.tif") %>%
  paste0("./Parameterization/Parameterization data/soilgrids/", .) #carbon in dg/kg 
n_rasts <- c("n0-5.tif", "n5-15.tif", "n15-30.tif", "n30-60.tif", "n60-100.tif", "n100-200.tif") %>%
  paste0("./Parameterization/Parameterization data/soilgrids/", .) #nitrogen in cg/kg
bulk_dens_rasts <- c("bd0-5.tif", "bd5-15.tif", "bd15-30.tif", "bd30-60.tif", "bd60-100.tif", "bd100-200.tif") %>%
  paste0("./Parameterization/Parameterization data/soilgrids/", .) #bulk density in cg/cm3

#TODO refactor with tidyverse
c_stack <- raster::stack(c_rasts) / 10 /1000 #convert from dg C per kg soil to g N per g soil
c_rast <- sum(c_stack) #total carbon

n_stack <- raster::stack(n_rasts) / 100 / 1000 #convert from cg N per kg soil to g N per g soil
n_rast <- sum(n_stack) #total nitrogen

bd_stack <- raster::stack(bulk_dens_rasts) /100 # convert cg/cm3 to g/cm3
bd_rast <- mean(bd_stack) #average bulk density

#convert from per mass to per volume basis
c_bd_stack <- c_stack * bd_stack
n_bd_stack <- n_stack * bd_stack

# because we have calculated C and N bulk densities (i.e. per cm3), we need
# to translate to density (per cm2) by multiplying by soil depth (in cm).

# To know how many layers we want for each variable, 
# we need to rasterize the soil depth polygons we created before
# template_raster <- raster::raster("./Models/LANDIS inputs/input rasters/initial_communities.tif")
# NAvalue(template_raster) <- 0
# crs(template_raster) #check that we're in NAD 83 Zone 17N
# mapunits2 <- sf::st_transform(mapunits_data, "EPSG:26917") #reproject to NAD83 Zone 17N, same as the initial communities
# soilDepth <- fasterize::fasterize(mapunits2, template_raster, field = "soilDepth", fun="max")
# soilDepth <- raster::mask(soilDepth, template_raster) #remove the water/nonforested cells
# 
# plot(soilDepth)
# rm(mapunits2)

# coarsen and reproject to match soilgrids data
# soilDepth_coarse  <- raster::projectRaster(soilDepth, c_stack, method = "ngb")

#set mapunits to match the soil rasters
mapunits <- sf::st_transform(mapunits, crs(c_stack)) 

c_extract <- raster::extract(c_bd_stack, mapunits, fun = mean, weights = TRUE)
n_extract <- raster::extract(n_bd_stack, mapunits, fun = mean, weights = TRUE)

# We have multiple depths for each variable, but many are deeper than soil depths
# From SSURGO. Maybe representing bedrock? In any case, they aren't likely to be
# relevant to our model. 
n_layers <- as.numeric(cut(mapunits_data$soilDepth, breaks = c(0, 15, 30, 60, 100, 200)))
weights <- c(15, 15, 30, 40, 50) #weight each layer by its depth. Reduce last layer weight to 50 for 150 cm depth

mapunits$c_total <- NA
mapunits$n_total <- NA
mapunits$cn_ratio <- NA

for(i in 1:nrow(mapunits)){
  #for each mapunit, take the weighted mean of the layers that are relevant. Weight by depth of each layer
  mapunits$c_total[i] <- weighted.mean(c_extract[i, n_layers[i]], weights = weights[1:nlayers[i]]) %>%
    `*`(mapunits_data$soilDepth[i]) %>% #multiple by depth to convert cm3 to cm2
    `*`(10000) #convert from cm2 to m2
  mapunits$n_total[i] <- weighted.mean(n_extract[i, n_layers[i]], weights = weights[1:nlayers[i]]) %>%
    `*`(mapunits_data$soilDepth[i]) %>% #multiple by depth to convert cm3 to cm2
    `*`(10000) #convert from cm2 to m2
}


mapunit_n <- mapunits[,"n_total"] %>%
  sf::st_set_geometry(NULL) %>%
  stats::aggregate(by = list(mapunits$MUKEY), FUN = function(x){mean(x, na.rm = TRUE)}) %>%
  dplyr::rename("MUKEY" = "Group.1")
mapunit_c <- mapunits[,"c_total"] %>%
  sf::st_set_geometry(NULL) %>%
  stats::aggregate(by = list(mapunits$MUKEY), FUN = function(x){mean(x, na.rm = TRUE)}) %>%
  dplyr::rename("MUKEY" = "Group.1")
mapunit_cn <- mapunits[,"cn_ratio_ssurgo"] %>%
  sf::st_set_geometry(NULL) %>%
  stats::aggregate(by = list(mapunits$MUKEY), FUN = function(x){mean(x, na.rm = TRUE)}) %>%
  dplyr::rename("MUKEY" = "Group.1")


#compare soil C from SSURGO vs from soilgrids
ggplot() + 
  geom_sf(mapping = aes(colour = c_total, fill = c_total), data = mapunits)
ggplot() + 
  geom_sf(mapping = aes(colour = soc0_999, fill = soc0_999), data = mapunits_data)
ggplot() + 
  geom_sf(mapping = aes(colour = soc0_150, fill = soc0_150), data = mapunits_data)
ggplot() + 
  geom_sf(mapping = aes(colour = soilDrain, fill = soilDrain), data = mapunits_data)

#compare soilgrids and ssurgo
mapunits$cn_ratio <- mapunits$c_total/mapunits$n_total
hist(mapunits$cn_ratio)

mapunits$cn_ratio_ssurgo <- mapunits_data$soc0_150 / mapunits$n_total
hist(mapunits$cn_ratio_ssurgo) #some crazy high values here -- what's with the soc0_999 value?

#wetlands have crazy high CN ratios; more than plausible
mapunits$cn_ratio_ssurgo[mapunits$cn_ratio_ssurgo > 60] <- 60
# some ratios are way too low; set to a more reasonable number. See Ross et al. 2010
mapunits$cn_ratio_ssurgo[mapunits$cn_ratio_ssurgo < 8] <- 8



# Fill in NA values

# #for NA values of cn_ratio_ssurgo, add the average of the other mapunits with the same MUKEY
mapunits[is.na(mapunits$cn_ratio_ssurgo), "cn_ratio_ssurgo"] <- mapunit_cn[match(mapunits$MUKEY, mapunit_cn$MUKEY), "cn_ratio_ssurgo"] %>%
  subset(is.na(mapunits$cn_ratio_ssurgo))


# for some other NAs, the whole mapunit is NA -- for these, average the values for the neighboring
# polygons and set the cn equal to that
neighbors <- spdep::poly2nb(mapunits) #get neighbors for each mapunit
neighbor_list <- neighbors
neighbor_cn <- NA
for(i in 1:nrow(mapunits)){
  #average the neighbors' CN for each shape
  neighbor_cn[i] <- mean(mapunits$cn_ratio_ssurgo[neighbor_list[[i]]], na.rm = TRUE)
}

#assign values from neighbors
mapunits$cn_ratio_ssurgo <- ifelse(is.na(mapunits$cn_ratio_ssurgo), neighbor_n, mapunits$cn_ratio_ssurgo)
#for the remaining 4 polygons, just assign them a 10 I guess TODO
mapunits[is.na(mapunits$cn_ratio_ssurgo), "cn_ratio_ssurgo"] <- 10
mapunits[mapunits$cn_ratio_ssurgo == 0, "cn_ratio_ssurgo"] <- 10


# re-calculate n from SSURGO soil carbon and soilgrids N
mapunits$n_adjusted <- mapunits_data$soc0_150 / mapunits$cn_ratio_ssurgo
plot(mapunits$n_adjusted ~ mapunits$n_total)

mapunits[is.na(mapunits$n_adjusted), ]


#-------------------------------------------------------------------------------
#create the soil N maps using soil C and C:N ratio
#TODO have C:N ratios change with soil layers

# Carbon is apportioned like so: 
# SOM1surfC = 0.01
# SOM1soilC = 0.02 
# SOM2C = 0.59 
# SOM3C = 0.38 

# Nitrogen as a proportion of carbon
# SOM1surfN=.1
# SOM1soilN=.1
# SOM2N=.04
# SOM3N=.118
# instead, let's divvy up N using the same proportions, but using soil N from soilgrids

# So, N totals should be apportioned as so:
# SOM1surfN = total N * (0.1 * 0.01)
# SOM1soilN= total N * (0.1 * 0.02)
# SOM2N= total N * (0.04 * 0.59)
# SOM3N= total N * (0.118 * 0.38)

# total = total N * 0.07054

# So we need to divide each layer by 0.07054 to retrieve the original amount of N per site

#------------------
# Using N directly from soilgrids, then distributing N between layers

mapunits_data$SOM1surfN <- mapunits$n_adjusted * (0.1 * 0.01) / 0.07054
mapunits_data$SOM1soilN <- mapunits$n_adjusted * (0.1 * 0.02)/ 0.07054
mapunits_data$SOM2N <- mapunits$n_adjusted * (0.04 * 0.59)/ 0.07054
mapunits_data$SOM3N <- mapunits$n_adjusted * (0.118 * 0.38)/ 0.07054

mapunits_data <- sf::st_transform(mapunits_data, "EPSG:26917") #reproject to NAD83 Zone 17N

#check CN ratios
hist(mapunits_data$SOM1surfC/mapunits_data$SOM1surfN)
hist(mapunits_data$SOM1soilC/mapunits_data$SOM1soilN)
hist(mapunits_data$SOM2C/mapunits_data$SOM2N)
hist(mapunits_data$SOM3C/mapunits_data$SOM3N)

#-------------------------------------------------------------------------------
# write rasters!
# this part might take a few hours depending on your study area, raster resolution, etc.
# TODO: rasterize once with mapunit, then match values to cells from mapunit ID. Should be waaaay faster, we'll just lose
# the ability to average mapunit values within a cell (i.e. just one mapunit ID per cell). But we have small cells
# so it shouldn't be much of a problem.

template_raster <- terra::rast("./Models/LANDIS inputs/input rasters/initial_communities.tif")
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

baseflow <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "baseFlow", fun="mean") 
values(baseflow) <- ifelse(is.na(values(baseflow)), 0, values(baseflow))
terra::writeRaster(baseflow, "./Models/LANDIS inputs/input rasters/baseflow.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

stormflow <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "runoff", fun="mean") 
values(stormflow) <- ifelse(is.na(values(stormflow)), 0, values(stormflow))
terra::writeRaster(stormflow, "./Models/LANDIS inputs/input rasters/stormflow.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

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
terra::terra::writeRaster(SOM1surfN, "./Models/LANDIS inputs/input rasters/SOM1surfN.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM1soilN <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM1soilN", fun="mean") %>%
  terra::mask(template_raster, inverse = FALSE, maskvalues = c(NA, 0), updatevalue = 0)
terra::NAflag(SOM1surfN) <- 0
# values(SOM1soilN) <- ifelse(is.na(values(SOM1soilN)), 0, values(SOM1soilN))
values(SOM1soilN) <- ifelse(values(SOM1soilN) >= 500, 499, values(SOM1soilN)) #TODO fix this
terra::terra::writeRaster(SOM1soilN, "./Models/LANDIS inputs/input rasters/SOM1soilN.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM2N <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM2N", fun="mean") %>%
  terra::mask(template_raster, inverse = FALSE, maskvalues = c(NA, 0), updatevalue = 0)
terra::NAflag(SOM2N) <- 0
# values(SOM1soilN) <- ifelse(is.na(values(SOM1soilN)), 0, values(SOM1soilN))
values(SOM2N) <- ifelse(values(SOM2N) >= 1000, 999, values(SOM2N)) #TODO fix this
terra::terra::writeRaster(SOM2N, "./Models/LANDIS inputs/input rasters/SOM2N.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)

SOM3N <- terra::rasterize(terra::vect(mapunits_data), template_raster, field = "SOM3N", fun="mean") %>%
  terra::mask(template_raster, inverse = FALSE, maskvalues = c(NA, 0), updatevalue = 0)
terra::NAflag(SOM3N) <- 0
# values(SOM1soilN) <- ifelse(is.na(values(SOM1soilN)), 0, values(SOM1soilN))
values(SOM3N) <- ifelse(values(SOM3N) >= 1000, 999, values(SOM3N)) #TODO fix this
terra::terra::writeRaster(SOM3N, "./Models/LANDIS inputs/input rasters/SOM3N.tif", datatype = "FLT4S", NAflag = 0, overwrite = TRUE)


#create new initial communities
library("raster")
library("sf")
library("tidyverse")
library("RSQLite")

#we'll use data from FIA for the initial communities, 
# https://www.fs.usda.gov/rmrs/publications/treemap-tree-level-model-conterminous-us-forests-circa-2014-produced-imputation-fia
# Riley et al. 2021

#treemap is in a weird CRS; looks like a NAD83 UTM, EPSG:42303
treemap <- raster("./calibration_data/treemap/RDS-2019-0026_Data/Data/national_c2014_tree_list.tif")


isro_bound <- sf::st_read("./calibration_data/isle_royale_boundary_buffer/isro_buffer.shp") %>%
  sf::st_transform(crs = crs(treemap))

treemap_isro <- crop(treemap, st_bbox(isro_bound))
plot_counts <- table(values(treemap_isro)) %>% #summary of how many of each plot ID in the study area
  as.data.frame()

fia_plots <- read.csv("./calibration_data/treemap/RDS-2019-0026_Data/Data/TL_CN_Lookup.txt") %>%
  filter(tl_id %in% values(treemap_isro))

fia_trees <- read.csv("./calibration_data/treemap/RDS-2019-0026_Data/Data/Tree_table_CONUS.txt") %>%
  filter(tl_id %in% values(treemap_isro))

# download michigan FIA data and reference data from the datamart:
# https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html
# could use rFIA for this TODO

#species reference data
sp_ref <- read.csv("./calibration_data/fia/FIADB_REFERENCE/REF_SPECIES.csv")


#get the basal area by species
fia_trees_ba <- fia_trees %>%
  dplyr::group_by(tl_id, SPCD) %>%
  dplyr::summarise(plot_ba = sum(I((DIA/2)^2), na.rm = TRUE), .group = "drop") %>%
  dplyr::mutate(tl_id = as.factor(tl_id)) %>%
  dplyr::left_join(plot_counts, by = c("tl_id" = "Var1")) %>% #add column for how frequent each plot type is
  dplyr::mutate(study_area_ba = plot_ba * Freq) %>%
  dplyr::group_by(SPCD) %>%
  dplyr::summarise(species_ba_total = sum(study_area_ba)/10000) %>%
  dplyr::arrange(species_ba_total) %>%
  dplyr::left_join(select(sp_ref, c("SPCD", "SPECIES_SYMBOL", "GENUS", "SPECIES"))) %>%
  dplyr::mutate(species_ba_pct = species_ba_total / sum(species_ba_total))
  
fia_trees_reduced <- fia_trees_ba[fia_trees_ba$species_ba_pct > 0.001, ]

# we need to take diameters and heights of trees and translate to biomass
# thankfully FVS has equations to do this already
# the biomass equations we need are available here: https://www.fs.fed.us/forestmanagement/products/measurement/biomass/index.php
# they're in an SQLite database, sort of inconveniently, so let's load them up using RSQLite

biomass_db <- dbConnect(RSQLite::SQLite(), "./calibration_data/fia/BiomassEqns/BiomassEqns.db") 
dbListTables(biomass_db)

biomass_eqn_coefs <- dbGetQuery(biomass_db, 'SELECT * FROM BM_EqCoefs') %>%
  filter(species_code %in% fia_trees$SPCD)

species_defaults <- 

biomass_eqn_forms <- dbGetQuery(biomass_db, 'SELECT * FROM BM_EqForms')

# forest_codes <- dbGetQuery(biomass_db, 'SELECT * FROM temp_BM_forests') #ISRO is in region 9, and the nearest forest is Superior NF, code 909



# 
# orig_ic <- raster("./calibration_data/initial communities/init_comm_052318.img")
# 
# plot(orig_ic)
# 
# ecoreg <- raster("./calibration_data/ecoregions/ecoregions_09132021.tif")
# 
# pop_zone <- raster("./calibration_data/browse/pop_zone_052318.img")
# 
# values(orig_ic)[values(ecoreg) == 3] <- 135
# values(orig_ic)[values(ecoreg) == 6] <- 134
# values(orig_ic)[values(ecoreg) %in% c(0,1)] <- 0
# 
# writeRaster(orig_ic, filename = "./calibration_data/initial communities/ic_2021-09-16_test.tif",
#             datatype = "INT2S", overwrite = TRUE)
# 
# ecoreg_updated <- ecoreg
# values(ecoreg_updated)[values(orig_ic) == 0 & values(ecoreg_updated) != 1] <- 0
# 
# plot(ecoreg_updated - ecoreg)
# 
# writeRaster(ecoreg_updated, filename = "./calibration_data/ecoregions/ecoregions_test.tif",
#             datatype = "INT2S", overwrite = TRUE)
# 
# # fix the pop_zone raster.
# # It looks like I just made a new one? Need to figure out what I ended up doing
# 
# # pop_zone_new <- reclassify(orig_ic, c(0, 0, 0, 0, 135, 1))
# # 
# # writeRaster(pop_zone_new, "./calibration_data/browse/pop_zone_2021-09-16.tif",
# #             datatype = "INT2S", overwrite = TRUE)
# 
# 
# #To make the initial communities, we need to get cohort data with biomass for
# #each cell. We can do this with FIA data.
# 
# fia_raster <- raster("./calibration_data/treemap/RDS-2019-0026_Data/Data/national_c2014_tree_list.tif")


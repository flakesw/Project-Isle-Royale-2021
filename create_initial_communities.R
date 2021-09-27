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

#TODO check where the data are coming from -- some from, e.g., AR and ID might be pretty far off
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
  
fia_trees_reduced <- fia_trees_ba[fia_trees_ba$species_ba_pct > 0.01, ] 

fia_clean <- fia_trees %>%
  filter(SPCD %in% fia_trees_reduced$SPCD) %>%
  left_join(select(sp_ref, c("SPCD", "SPECIES_SYMBOL", "GENUS", "SPECIES")))


#estimate biomass using SilviaTerra's R package
#code borrowed from https://github.com/SilviaTerra/rpnc250
remotes::install_github("SilviaTerra/rpnc250", ref = "main")

library(rpnc250)

#clean up data, remove NAs and dead trees
plot_ba <- fia_clean %>%
  dplyr::filter(
    !is.na(DIA),
    !is.na(TPA_UNADJ),
    STATUSCD == 1
  ) %>%
  dplyr::group_by(
    CN # FIA plot IDs
  ) %>%
  dplyr::summarize(
    bapa = sum(TPA_UNADJ * 0.005454 * DIA^2),
    .groups = "drop"
  )

# filter the trees and join to plot basal area table
trees_prepped <- fia_clean %>%
  dplyr::filter(
    !is.na(DIA),
    STATUSCD == 1, # only live trees
    !is.na(TPA_UNADJ)
  ) %>%
  dplyr::left_join(
    plot_ba,
    by = "CN"
  )

#calculate biomass
trees_with_biomass <- trees_prepped %>%
  dplyr::mutate(
    biomass = estimate_biomass(
      spcd = SPCD,
      dbh = DIA,
      site_index = 65,
      stand_basal_area = bapa
    )
  )

#plot biomass estimates
trees_with_biomass %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = DIA,
      y = biomass,
      color = SPECIES_SYMBOL
    )
  ) +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "DBH (inches)",
    y = "biomass (green tons)",
    color = "species"
  )



# #alternative method using USFS NBEL database -- work in progress TODO
# # we need to take diameters and heights of trees and translate to biomass
# # thankfully FVS has equations to do this already
# # the biomass equations we need are available here: https://www.fs.fed.us/forestmanagement/products/measurement/biomass/index.php
# # they're in an SQLite database, sort of inconveniently, so let's load them up using RSQLite
# 
# biomass_db <- dbConnect(RSQLite::SQLite(), "./calibration_data/fia/BiomassEqns/BiomassEqns.db") 
# dbListTables(biomass_db)
# 
# biomass_eqn_coefs <- dbGetQuery(biomass_db, 'SELECT * FROM BM_EqCoefs') %>%
#   filter(species_code %in% fia_trees_reduced$SPCD)
# 
# test <- dbGetQuery(biomass_db, 'SELECT * FROM tblEqns') # maybe useful? Returns weird variables like "BST", "BBL", "BFT"
# test <- dbGetQuery(biomass_db, 'SELECT * FROM BM_comp') #maybe describes weird abbreviations above, but they' renot in the chart!
# test <- dbGetQuery(biomass_db, 'SELECT * FROM BM_ref_species') #some wood traits, like percent moisture, bark volume, etc
# test <- dbGetQuery(biomass_db, 'SELECT * FROM bm_eq_info') #some generalized equations for mixed communities -- same as eqn_coefs table?
# test <- dbGetQuery(biomass_db, 'SELECT * FROM speciesbyregion') #not sure what this table is doing
# test <- dbGetQuery(biomass_db, 'SELECT * FROM eqninfobyspecies') #this one looks more usable! Equations are already in extractable form. But limited species. Not sure what this table is doing really.
# test <- dbGetQuery(biomass_db, 'SELECT * FROM tblComp') #described some biomass components -- matches one from tblEqns
# test <- dbGetQuery(biomass_db, 'SELECT * FROM tblConversions') #just some conversion factors
# test <- dbGetQuery(biomass_db, 'SELECT * FROM tblEqnsMeta') #has equations already in usable form, but returns components like TblEqns
# test <- dbGetQuery(biomass_db, 'SELECT * FROM tblEqnsNew') %>%
#   filter(SPP_CODE %in% fia_trees_reduced$SPCD) #also has limited species, like eqninfobyspecies
# 
# 
# # species_defaults <- 
# 
# biomass_eqn_forms <- dbGetQuery(biomass_db, 'SELECT * FROM BM_EqForms')
# 
# # forest_codes <- dbGetQuery(biomass_db, 'SELECT * FROM temp_BM_forests') #ISRO is in region 9, and the nearest forest is Superior NF, code 909
# 
# fia_trees[40, ] %>% left_join(biomass_eqn_coefs, by = c("SPCD" = "species_code"))
# 
# biomass_eqn_forms %>% dplyr::filter(eqform_id == "4") %>%
#   select(equation)
# 
# biomass_eqn_cleaned <- 



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


#create new initial communities
library("raster")
library("sf")
library("tidyverse")
library("RSQLite")
library("rFIA")
library("terra")

# TODO add in alder, willow, herbs

# This script creates initial communities, 


#we'll use data from FIA for the initial communities, 
# https://www.fs.usda.gov/rmrs/publications/treemap-tree-level-model-conterminous-us-forests-circa-2014-produced-imputation-fia
# Riley et al. 2021

#treemap is in a weird CRS; probably a NAD83 UTM, EPSG:42303
treemap <- terra::rast("./calibration_data/treemap/RDS-2019-0026_Data/Data/national_c2014_tree_list.tif")

isro_bound <- sf::st_read("./calibration_data/isle_royale_boundary_buffer/isro_buffer.shp") %>%
  sf::st_transform(crs = terra::crs(treemap))

#TODO check where the data are coming from -- some from, e.g., AR and ID might be pretty far off
treemap_isro <- terra::crop(treemap, isro_bound) %>%
  terra::project("EPSG:26917", method = "near")

template <- terra::rast(extent = terra::ext(treemap_isro),
                        resolution = 30,
                        crs = "EPSG:26917")
treemap_isro <- terra::resample(treemap_isro, template, method = "near")
plot_counts <- table(terra::values(treemap_isro)) %>% #summary of how many of each plot ID in the study area
  as.data.frame()
treemap_isro[is.na(treemap_isro)] <- 0


rm(treemap) #free up some RAM
detach("package:terra") #get rid of masking from terra since we'll be using raster for the rest of this

treemap_isro <- raster(treemap_isro) #turn into a raster package object

# write the raster for IC 
# raster package was having trouble here for some reason, so I'm using terra
raster::writeRaster(treemap_isro, filename = "./LANDIS inputs/input rasters/initial_communities.tif", datatype = "INT4S", overwrite = TRUE, NAvalue = 0)
# 
# test <- raster("./LANDIS inputs/input rasters/initial_communities.tif")
# table(values(test))
# unique(values(test)) %in% tl_plots$tl_id
# unique(values(test)) %in% site_biomass$MapCode
# unique(site_biomass$MapCode) %in% unique(values(test))
# length(unique(site_biomass$MapCode))
# length(unique(values(test))) #10 lots get dropped TODO

tl_plots <- read.csv("./calibration_data/treemap/RDS-2019-0026_Data/Data/TL_CN_Lookup.txt") %>%
  filter(tl_id %in% values(treemap_isro))

tl_trees <- read.csv("./calibration_data/treemap/RDS-2019-0026_Data/Data/Tree_table_CONUS.txt") %>%
  filter(tl_id %in% values(treemap_isro))

#inventory data from study area, to ground-truth which species are important
isro_inv <- read.csv("./inventory_data/5_isro_spcov.csv")


#-------------------------------------------------------------------------------
#create ecoregions
ecoregions <- raster::reclassify(treemap_isro, c(0,0,0,1,99999,1)) #just two ecoregions, active and inactive
raster::writeRaster(ecoregions, "./LANDIS inputs/input rasters/ecoregions.tif", datatype = "INT2S", overwrite = TRUE)

#-------------------------------------------------------------------------------
# download michigan FIA data and reference data from the datamart:
# https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html
# could use rFIA for this

#species reference data
sp_ref <- read.csv("./calibration_data/fia/FIADB_REFERENCE/REF_SPECIES.csv")

#get the basal area by species
tl_trees_ba <- tl_trees %>%
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

#TODO reconcile these datasets
tl_trees_ba$in_isro <- tl_trees_ba$SPECIES_SYMBOL %in% isro_inv$Plant.Symbol
  
# remove rare species
# TODO replace with functional type
# e.g. combine sorbus spp. into one Sorbus type; change rare species to genus or 
tl_trees_reduced <- tl_trees_ba[tl_trees_ba$species_ba_pct > 0.01, ] 

# tl_trees_clean <- tl_trees %>%
#   mutate(other_species = ifelse(SPCD %in% tl_trees_reduced$SPCD, SPCD,
#                                 sp_ref[match(SPCD, sp_ref$SPCD), "SFTWD_HRDWD"])) %>%
#   mutate(SPCD = ifelse(other_species == "H", 001,
#                        ifelse(other_species == "S", 002, 
#                               SPCD))) %>%
#   left_join(select(sp_ref, c("SPCD", "SPECIES_SYMBOL", "GENUS", "SPECIES"))) %>%
#   dplyr::filter(
#     !is.na(DIA),
#     !is.na(TPA_UNADJ),
#     STATUSCD == 1
#   ) %>%
#   mutate(SPECIES_SYMBOL = ifelse(SPCD == "H", "HRDWD",
#                                  ifelse(SPCD == "S", "SFTWD",
#                                         SPECIES_SYMBOL)))


# TODO
# Get all TREE data for the plots needed, and get biomass from table
# Get functional types for rare species

# getFIA(states = c(unique(tl_trees$State_Abbreviation)),
#        tables = c('TREE', 'PLOT'),
#        dir = './calibration_data/fia/rFIA_downloads/',
#        load = FALSE)

#create an empty dataframe with same structure as FIA TREE table -- this is a bad way to do this
fia_trees <- read.csv('./calibration_data/fia/rFIA_downloads/MI_TREE.csv')[FALSE,]
gc()

for(state in unique(tl_trees$State_Abbreviation)){
  fia_trees_temp <- readFIA(dir = './calibration_data/fia/rFIA_downloads/',
                       tables = c('TREE'),
                       states = state) %>%
    '[['('TREE') %>%
    filter(PLT_CN %in% tl_plots$CN)
  # print(state)
  #add selected TREE data. This is inefficient. TODO refactor here
  fia_trees <- rbind(fia_trees, fia_trees_temp)
  rm(fia_trees_temp)
  gc() #R was keeping the files in memory for some reason and taking up a lot of memory
}

#take out trees with  no diameter data and replace rare species with softwood orhardwood
fia_trees <- fia_trees %>% 
  filter(!is.na(DIA)) %>%
  mutate(other_species = ifelse(SPCD %in% tl_trees_reduced$SPCD, 
                                SPCD,
                                sp_ref[match(SPCD, sp_ref$SPCD), "SFTWD_HRDWD"])) %>%
  mutate(SPCD = ifelse(other_species == "H", 001, 
                       ifelse(other_species == "S", 002,
                              SPCD)))

#there's probably a way to do this using rFIA with its larger-than-RAM methods, TODO
# fia_trees2 <- readFIA('./calibration_data/fia/rFIA_downloads/', 
#                       inMemory = FALSE,
#                       tables = c("TREE"))


# #plot location of particular FIA plot types to visualize the weirdos
# rc <- rowColFromCell(treemap_isro, which(values(treemap_isro == 21217)))
# plot(extent(treemap_isro, rc[1], rc[1], rc[2],  rc[2]), add=TRUE, col='red', lwd=3)


#-------------------------------------------------------------------------------
# estimate biomass from FIA tree table
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# estimate tree ages
# use site tree information from FIA. initially, just using all data from MI
# TODO subset to closest counties in MI and MN
# TODO account for site index -- check SICOND volume in COND table
# TODO more realistic curve -- revise log relationship with more realistic shape


# getFIA(states = c(unique(tl_trees$State_Abbreviation)),
#        tables = c('COND'),
#        dir = './calibration_data/fia/rFIA_downloads/',
#        load = FALSE)

fia_cond <- readFIA(states = c(unique(tl_trees$State_Abbreviation)),
                    tables = c('COND'),
                    dir = './calibration_data/fia/rFIA_downloads/') %>%
  '[['("COND") %>%
  filter(PLT_CN %in% tl_plots$CN)

# getFIA(states = c(unique(tl_trees$State_Abbreviation)),
#        tables = c('SITETREE'),
#        dir = './calibration_data/fia/rFIA_downloads/',
#        load = FALSE)

sitetrees <- readFIA(states = c(unique(tl_trees$State_Abbreviation)),
                     tables = c('SITETREE'),
                     dir = './calibration_data/fia/rFIA_downloads/') %>%
  '[['("SITETREE") %>%
  filter(PLT_CN %in% tl_plots$CN) %>%
  mutate(SFTWD_HRDWD = sp_ref[match(SPCD, sp_ref$SPCD), "SFTWD_HRDWD"]) #add a softwood/hardwood column for later

#fit a linear regression 
tree_regressions <- sitetrees %>% 
  dplyr::filter(!is.na(DIA) & !is.na(AGEDIA) & !is.na(SPCD)) %>%
  dplyr::filter(SPCD %in% fia_trees$SPCD) %>%
  dplyr::group_by(SPCD) %>%
  dplyr::do(model = lm(AGEDIA ~ log(DIA) + 0, data = .))

#do models for hardwood and softwood -- 
hardwood_regression <- lm(AGEDIA ~ log(DIA) + 0, data = sitetrees[sitetrees$SFTWD_HRDWD == "H", ])
softwood_regression <- lm(AGEDIA ~ log(DIA) + 0, data = sitetrees[sitetrees$SFTWD_HRDWD == "S", ])

tree_regressions <- tibble(SPCD = c(1,2),
               model = list(hardwood_regression, softwood_regression)) %>%
  bind_rows(tree_regressions, .)
  
  

# TODO check on functional shape between age and diameter
for(i in 1:nrow(fia_trees)){
  #estimate age from diameter for each tree
  model <- tree_regressions[match(fia_trees$SPCD[i], tree_regressions$SPCD), ] %>% 
    pluck('model', 1)
  fia_trees$age[i] <- predict(model, newdata = list(DIA = fia_trees$DIA[i]))
  if(fia_trees$age[i] < 1) fia_trees$age[i] <- 1
}

plot(fia_trees$age ~ fia_trees$DIA)

#set range of ages for cohorts
# the weird expression in here rounds up the nearest 10
breaks <- seq(0, max(fia_trees$age) + (10 - max(fia_trees$age) %% 10), by = 5)
# breaks[1] <- 1

#get biomass from tree table and aggregate to cohort biomass
site_biomass <- fia_trees %>%
  dplyr::mutate(biomass_area = DRYBIO_AG * TPA_UNADJ) %>%
  dplyr::mutate(age = ifelse(age < 1, 1, age)) %>%
  dplyr::mutate(bin = base::cut(age, breaks = breaks, labels = breaks[-1], right = TRUE)) %>%
  dplyr::mutate(bin = as.integer(as.character(bin))) %>%
  group_by(PLT_CN, SPCD, bin) %>%
  dplyr::summarise(biomass = sum(biomass_area)) %>%
  dplyr::mutate(biomass = round(biomass, digits = 0)) %>%
  dplyr::mutate(biomass = biomass * 0.11) #convert to g m-2 from lb ac-2

names(site_biomass) <- c("PLT_CN", "SPCD", "CohortAge", "CohortBiomass")

#TODO investigate why some plots have very very little AGB
site_total_biomass <- site_biomass %>%
  dplyr::group_by(PLT_CN) %>%
  dplyr::summarise(total_biomass = sum(CohortBiomass)) %>%
  dplyr::mutate(total_biomass_tonnes_ha = total_biomass * 0.01) #convert to tonnes ha-1

density <- tl_trees_clean %>%
  group_by(CN) %>%
  summarize(dens = sum(TPA_UNADJ))



#-------------------------------------------------------------------------------
# estimate dead wood from FIA
# data is only available for a few plots with intensive sampling
# TODO find a better way to interpolate dead wood -- kriging? Or a clustering method
# to find most similar plots

# TODO try using CARBON_DOWN_DEAD column in COND table


# getFIA(states = c(unique(tl_trees$State_Abbreviation)),
#        tables = c('DWM_COARSE_WOODY_DEBRIS', 'DWM_FINE_WOODY_DEBRIS'),
#        dir = './calibration_data/fia/rFIA_downloads/',
#        load = FALSE)

#TODO write a function to check if data is downloaded, and download if not
#this table doesn't have any more information than the DWM_COARSE_WOODY_DEBRIS table -- only Phase 3 info
# getFIA(states = c(unique(tl_trees$State_Abbreviation)),
#        tables = c('COND_DWM_CALC'),
#        dir = './calibration_data/fia/rFIA_downloads/',
#        load = FALSE)


#TODO double check this -- there is way too much CWD in some plots
cwd_plot <- readFIA(dir = './calibration_data/fia/rFIA_downloads/',
               tables = 'DWM_COARSE_WOODY_DEBRIS') %>%
  '[['('DWM_COARSE_WOODY_DEBRIS') %>%
  dplyr::filter(PLT_CN %in% tl_plots$CN) %>%
  dplyr::group_by(PLT_CN) %>%
  dplyr::summarise(total_cwd = sum(DRYBIO_AC_PLOT)) %>%
  dplyr::left_join(dplyr::select(site_total_biomass, c("PLT_CN", "total_biomass")), 
                   by = c("PLT_CN" = "PLT_CN")) 

plot(cwd_plot$total_cwd ~ cwd_plot$total_biomass)
abline(coef(lm(cwd_plot$total_cwd ~ cwd_plot$total_biomass))) #sadly, there isn't alinear relationship between biomass and cwd

cwd_model <- lm(total_cwd ~ total_biomass, data = cwd_plot)
site_total_biomass$cwd <- predict(cwd_model, newdata = data.frame(total_biomass = site_total_biomass$total_biomass))
site_total_biomass$cwd <- site_total_biomass$cwd * 0.11 # convert from lb ac-1 to g m-2

#-------------------------------------------------------------------------------
# coarse roots
#-------------------------------------------------------------------------------
# Coarse roots can be calculated per tree
# TODO we need dead roots!
# for the moment, guestimating that dead roots = 10% of live roots
# root_ratio = Exp(JENKINS_ROOT_RATIO_B1 + JENKINS_ROOT_RATIO_B2 / (DIA*2.54))

# we can also use the DRYBIO_BG column in the TREE table
site_total_biomass <- fia_trees %>%
  dplyr::mutate(biomass_area = DRYBIO_BG * TPA_UNADJ) %>%
  group_by(PLT_CN) %>%
  dplyr::summarise(root_biomass = sum(biomass_area)) %>%
  dplyr::mutate(root_biomass = round(root_biomass, digits = 0)) %>%
  dplyr::mutate(root_biomass = root_biomass * 0.11) %>% #convert to g m-2 from lb ac-2
  dplyr::mutate(root_biomass = root_biomass * 0.10) %>% #scale from live roots to dead roots
  dplyr::left_join(site_total_biomass, by = "PLT_CN")

#-------------------------------------------------------------------------------
# Tidy up and write data
tl_trees_ba <- bind_rows(tl_trees_ba,
                         list(SPCD = c(1,2), SPECIES_SYMBOL = c("HRDWD", "SFTWD")))

site_biomass <- site_biomass %>%
  mutate(MapCode = tl_plots[match(PLT_CN, tl_plots$CN), "tl_id"]) %>%
  left_join(tl_trees_ba %>% select(c("SPCD", "SPECIES_SYMBOL")), by = "SPCD") %>%
  dplyr::rename(SpeciesName = SPECIES_SYMBOL)
  
site_total_biomass$MapCode <- tl_plots[match(site_total_biomass$PLT_CN, tl_plots$CN), "tl_id"] #replace CNs with tl_ids

write.csv(site_biomass[, c(5,6,3,4)], "./LANDIS inputs/NECN files/initial_communities_update.csv")

cwd_raster <- raster::subs(treemap_isro, site_total_biomass, by = "MapCode", which = "cwd")
values(cwd_raster) <- ifelse(values(cwd_raster) <= 0 | is.na(values(cwd_raster)), 1, values(cwd_raster))
writeRaster(cwd_raster, "./LANDIS inputs/input rasters/dead_wood.tif", datatype = "INT2S", NAvalue = 0, overwrite = TRUE)

root_raster <- raster::subs(treemap_isro, site_total_biomass, by = "MapCode", which = "root_biomass")  
values(root_raster) <- ifelse(values(root_raster) <= 0 | is.na(values(root_raster)), 1, values(root_raster))
writeRaster(root_raster, "./LANDIS inputs/input rasters/coarse_roots.tif", datatype = "INT2S", NAvalue = 0, overwrite = TRUE)

################################################################################
# Trash bin

#--------------------------------------------------------------------------------
# #alternative method for biomass using USFS NBEL database -- work in progress TODO
# # we need to take diameters and heights of trees and translate to biomass
# # thankfully FVS has equations to do this already
# # the biomass equations we need are available here: https://www.fs.fed.us/forestmanagement/products/measurement/biomass/index.php
# # they're in an SQLite database, sort of inconveniently, so let's load them up using RSQLite
# 
# biomass_db <- dbConnect(RSQLite::SQLite(), "./calibration_data/fia/BiomassEqns/BiomassEqns.db") 
# dbListTables(biomass_db)
# 
# biomass_eqn_coefs <- dbGetQuery(biomass_db, 'SELECT * FROM BM_EqCoefs') %>%
#   filter(species_code %in% tl_trees_reduced$SPCD)
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
#   filter(SPP_CODE %in% tl_trees_reduced$SPCD) #also has limited species, like eqninfobyspecies
# 
# 
# # species_defaults <- 
# 
# biomass_eqn_forms <- dbGetQuery(biomass_db, 'SELECT * FROM BM_EqForms')
# 
# # forest_codes <- dbGetQuery(biomass_db, 'SELECT * FROM temp_BM_forests') #ISRO is in region 9, and the nearest forest is Superior NF, code 909
# 
# tl_trees[40, ] %>% left_join(biomass_eqn_coefs, by = c("SPCD" = "species_code"))
# 
# biomass_eqn_forms %>% dplyr::filter(eqform_id == "4") %>%
#   select(equation)
# 
# biomass_eqn_cleaned <- 



#------------------------------------------------------------------------------
# estimate biomass using SilviaTerra's R package
# code borrowed from https://github.com/SilviaTerra/rpnc250
# remotes::install_github("SilviaTerra/rpnc250", ref = "main")
# 
# library(rpnc250)
# 
# #calculate basal area
# plot_ba <- tl_trees_clean  %>%
#   dplyr::group_by(
#     CN # FIA plot IDs
#   ) %>%
#   dplyr::summarize(
#     bapa = sum(TPA_UNADJ * 0.005454 * DIA^2),
#     .groups = "drop"
#   )
# 
# # filter the trees and join to plot basal area table
# trees_prepped <- tl_trees_clean %>%
#   dplyr::filter(
#     !is.na(DIA),
#     STATUSCD == 1, # only live trees
#     !is.na(TPA_UNADJ)
#   ) %>%
#   dplyr::left_join(
#     plot_ba,
#     by = "CN"
#   )
# 
# #calculate biomass
# trees_with_biomass <- trees_prepped %>%
#   dplyr::mutate(
#     biomass = estimate_biomass(
#       spcd = SPCD,
#       dbh = DIA,
#       site_index = 65,
#       stand_basal_area = bapa
#     )
#   ) 
# 
# #plot biomass estimates
# trees_with_biomass %>%
#   ggplot2::ggplot(
#     ggplot2::aes(
#       x = DIA,
#       y = biomass,
#       color = SPECIES_SYMBOL
#     )
#   ) +
#   ggplot2::geom_point() +
#   ggplot2::labs(
#     x = "DBH (inches)",
#     y = "biomass (green tons)",
#     color = "species"
#   )
# 
# 
# #add in biomass to our original dataframe
# tl_trees_clean <- tl_trees_clean %>% 
#   left_join(trees_with_biomass %>% select(TreeID, biomass)) %>%
#   #biomass is in imperial tons -- convert to g
#   mutate(biomass = biomass * 907185) %>%
#   # convert to g ha-1 by multiplying by TPA and converting from ac to m^2
#   mutate(biomass_area = biomass * TPA_UNADJ /4046.9)



#-------------------------------------------------------------------------------
# estimate tree ages
# use site tree information from FIA. initially, just using all data from MI
# TODO subset to closest counties in MI and MN
# TODO account for site index
# TODO more realistic curve

# getFIA(states = c(unique(tl_trees$State_Abbreviation)),
#        tables = c('COND'),
#        dir = './calibration_data/fia/rFIA_downloads/',
#        load = FALSE)
# 
# fia_cond <- readFIA(states = c(unique(tl_trees$State_Abbreviation)),
#                     tables = c('COND'),
#                     dir = './calibration_data/fia/rFIA_downloads/') %>%
#   '[['("COND") %>%
#   filter(PLT_CN %in% tl_plots$CN)
# 
# #fit a linear regression 
# tree_regressions <- sitetrees %>% 
#   dplyr::filter(!is.na(DIA) & !is.na(AGEDIA) & !is.na(SPCD)) %>%
#   dplyr::filter(SPCD %in% tl_trees_clean$SPCD) %>%
#   dplyr::group_by(SPCD) %>%
#   dplyr::do(model = lm(AGEDIA ~ log(DIA) + 0, data = .))
# 
# # TODO check on functional shape between age and diameter
# for(i in 1:nrow(tl_trees_clean)){
#   #estimate age from diameter for each tree
#   model <- tree_regressions[match(tl_trees_clean$SPCD[i], tree_regressions$SPCD), ] %>% pluck('model', 1)
#   tl_trees_clean$age[i] <- predict(model, newdata = list(DIA = tl_trees_clean$DIA[i]))
#   if(tl_trees_clean$age[i] < 1) tl_trees_clean$age[i] <- 1
# }
# 
# plot(tl_trees_clean$age ~ tl_trees_clean$DIA)
# 
# #set range of ages for cohorts
# # the weird expression in here rounds up the nearest 10
# breaks <- seq(0, max(tl_trees_clean$age) + (10 - max(tl_trees_clean$age) %% 10), by = 5)
# # breaks[1] <- 1
# 
# site_biomass <- tl_trees_clean %>%
#   dplyr::mutate(age = ifelse(age < 1, 1, age)) %>%
#   dplyr::group_by(tl_id) %>%
#   dplyr::group_by(SPECIES_SYMBOL) %>%
#   dplyr::mutate(bin = base::cut(age, breaks = breaks, labels = breaks[-1], right = TRUE)) %>%
#   dplyr::mutate(bin = as.integer(as.character(bin))) %>%
#   dplyr::group_by(tl_id, SPECIES_SYMBOL, bin) %>%
#   dplyr::summarise(biomass = sum(biomass_area)) %>%
#   dplyr::mutate(biomass = round(biomass, digits = 0))
# 
# names(site_biomass) <- c("MapCode", "SpeciesName", "CohortAge", "CohortBiomass")
# 
# write.csv(site_biomass, "initial_communities_update.csv")
# 
# site_total_biomass <- site_biomass %>%
#   dplyr::group_by(MapCode) %>%
#   dplyr::summarise(total_biomass = sum(CohortBiomass))
# 
# density <- tl_trees_clean %>%
#   group_by(CN) %>%
#   summarize(dens = sum(TPA_UNADJ))

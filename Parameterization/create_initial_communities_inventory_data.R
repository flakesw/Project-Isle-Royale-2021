#create new initial communities
# library("raster")
library("sf")
library("tidyverse")
library("RSQLite")
library("rFIA")
library("terra")

setwd("C:/Users/Sam/Documents/Research/Isle Royale")

options(warn=1)

total_ba_from_DBH <- function(DBH_vector){
  #calculate total basal area in square inches from a vector of DBH
  if(is.na(DBH_vector) | DBH_vector == ""){
    return(NA)
  }
  
  if(length(DBH_vector >= 1)){

    strsplit(DBH_vector, ",")[[1]] %>%
      as.numeric() %>%
      `/`(2) %>%
      `^`(2) %>%
      `*`(pi) %>%
      sum()
  }
}

for(i in 1:nrow(isro_inv)){
  print(total_ba_from_DBH(isro_inv$DBH[i]))
}



# TODO add in alder, willow, herbs

# This script creates initial communities, 

inventory_shape <- sf::st_read("./Parameterization/Parameterization data/inventory_data/1 isrogisdata/isrogisdata/isroveg/isroveg.shp")
plot(sf::st_geometry(inventory_shape))
table(inventory_shape$PI)


#for large trees, convert to biomass and age
#for smaller trees, convert from cover to biomass; use weibull distribution to get diameters; diameters to age


#-------------------------------------------------------------------------------
# Import data from ISRO
isro_inv <- readxl::read_excel("./Parameterization/Parameterization data/inventory_data/5_isro_spcov.xls")

head(isro_inv)

isro_inv <- isro_inv %>% 
  dplyr::filter(Stratum == "T2") %>%
  mutate(BA = map_dbl(DBH, total_ba_from_DBH)) 

total_ba = isro_inv %>%
  group_by(`Plant Symbol`) %>%
  summarise(total_BA = sum(BA, na.rm = TRUE))

comm_mat_isro <- isro_inv %>%
  group_by(`Plot Code`, `Plant Symbol`) %>%
  summarise(total_BA = sum(BA)) %>%
  mutate(total_BA = ifelse(is.na(total_BA), 0, total_BA)) %>%
  mutate(total_BA = total_BA / (400)) %>% #convert square inches per 400 m2 to m2 per ha
  pivot_wider(names_from = `Plant Symbol`, values_from = total_BA, values_fill = 0)

# Plot-level data
isro_plot <- readxl::read_excel("./Parameterization/Parameterization data/inventory_data/4_isroplots.xls") %>%
  filter(Use == "T")
isro_plot[nchar(isro_plot$PI) == 1, "PI"] <- paste0("0", unlist(isro_plot[nchar(isro_plot$PI) == 1, "PI"]))
isro_plot[nchar(isro_plot$Alt) == 1 & !is.na(isro_plot$Alt), "Alt"] <- 
  paste0("0", unlist(isro_plot[nchar(isro_plot$Alt) == 1 & !is.na(isro_plot$Alt), "Alt"]))
table(c(isro_plot$PI, isro_plot$Alt))

sort(unique(inventory_shape$PI)[!(unique(inventory_shape$PI) %in% c(isro_plot$PI, isro_plot$Alt, isro_plot$Alt2))])

isro_map_total_area <- inventory_shape %>%
  sf::st_drop_geometry() %>%
  group_by(PI) %>%
  summarise(total_area = sum(AREA))


#-------------------------------------------------------------------------------
# Create initial communities map

#assign each polygon to a plot
isro_plot$MapCode <- as.numeric(unlist(regmatches(isro_plot$`Plot Code`, gregexpr("[[:digit:]]+", isro_plot$`Plot Code`))))

inventory_shape$MapCode <- NA
PI_ignore <- c("61", "63", "67", "67A", "98", "99")

for(type in unique(inventory_shape$PI)){
  if(type %in% PI_ignore) next()
  
  isro_plot_sub <- isro_plot %>% filter(PI == type | Alt == type | Alt2 == type)
  inventory_shape[inventory_shape$PI == type, "MapCode"] <- sample(isro_plot_sub$MapCode, 
                                                                   size = sum(inventory_shape$PI %in% type),
                                                                   replace = TRUE)
}


test2 <- vect(inventory_shape)


empty_rast <- terra::rast(extent = terra::ext(terra::vect(inventory_shape)), resolution = 50, 
                          crs = terra::crs(terra::vect(inventory_shape)))

initial_communities <- terra::rasterize(x = terra::vect(inventory_shape), y = empty_rast,  field = "MapCode")

terra::writeRaster(initial_communities, "./Models/LANDIS inputs/input rasters/initial_communities_new.tif")


#*******************************************************************************
# Get regressions from FIA data to estimate ages and biomass

# We need equations to get age ~ diameter*species and biomass ~ diameter (Jenkins equations)


################################################################################
library(tidyverse)
library(sf)

forest_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_FOREST_TYPE.csv")  
# 
# boundary <-  sf::st_read(boundary_loc) %>%
#   summarise(do.union = TRUE) %>% 
#   st_make_valid() %>%
#   smoothr::fill_holes(threshold = 0.5) 

all_fia_plot <- paste0("D:/data/fia/rFIA_downloads/", c("MI", "WI", "MN"),"_PLOT.csv") %>%
  purrr::map(read.csv) %>%
  dplyr::bind_rows() 


all_fia_cond <- paste0("D:/data/fia/rFIA_downloads/", c("MI", "WI", "MN"),"_COND.csv") %>%
  purrr::map(read.csv) %>%
  do.call(rbind, .) #bind_rows() doesn't work here because of some columns being cast to different data types

fia_cond_reduced <- all_fia_cond %>%
  filter(OWNCD != 46) %>%
  mutate(IS_FOREST = ifelse(FORTYPCD %in%(c(1:998)), 1, 0)) %>%
  group_by(PLT_CN) %>%
  summarise(total_cond = sum(CONDPROP_UNADJ),
            natural = sum(STDORGCD, na.rm = TRUE),
            treatment = sum(TRTCD1, na.rm = TRUE),
            proportion_forest = sum(CONDPROP_UNADJ * IS_FOREST),
            cc = sum(CONDPROP_UNADJ * LIVE_CANOPY_CVR_PCT, na.rm = TRUE)) %>%
  filter(total_cond > 0.95 ) %>%
  filter(proportion_forest > 0)

plot_forest_type <- all_fia_cond %>%
  group_by(PLT_CN, FORTYPCD) %>%
  summarise(total_fortypcd = sum(CONDPROP_UNADJ)) %>%
  slice_max(total_fortypcd)

fia_cond_reduced <- left_join(fia_cond_reduced, plot_forest_type, by = "PLT_CN")
fia_cond_reduced$forest_group <- forest_ref[match(fia_cond_reduced$FORTYPCD, forest_ref$VALUE), "TYPGRPCD"]

fia_plot_reduced <- all_fia_plot %>% 
  left_join(fia_cond_reduced, by = c("CN" = "PLT_CN")) %>%  
  dplyr::filter(INVYR > 2006)

#----
#Trees
fia_site_trees <-  paste0("D:/data/fia/rFIA_downloads/", c("MI", "WI", "MN"),"_SITETREE.csv") %>%
  purrr::map(read.csv) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(PLT_CN %in% fia_plot_reduced$CN) %>%
  dplyr::mutate(TOTAGE = AGEDIA + 10)
# 
# breaks <- seq(0, max(fia_site_trees$TOTAGE, na.rm = TRUE) + (10 - max(fia_site_trees$TOTAGE, na.rm = TRUE) %% 10),
#               by = 5)

fia_trees <-  paste0("D:/data/fia/rFIA_downloads/", c("MI", "WI", "MN"),"_TREE.csv") %>%
  purrr::map(read.csv) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(PLT_CN %in% fia_plot_reduced$CN)

fia_trees <- fia_trees %>%
  filter(STATUSCD == 1) %>%
  mutate(DRYBIO_TOTAL = CARBON_AG * 2 / 2204, #convert to Mg
         AGE_BIN = as.numeric(base::cut(TOTAGE, breaks)),
         SPCD = as.character(SPCD))

#------------------
# write outputs

write.csv(fia_plot_reduced, plot_output_loc)
write.csv(fia_trees, tree_output_loc)



################################################################################
# Estimate diameter:age regressions

#these are needed to convert LANDIS cohorts to FIA world
#TODO revise this -- which trees do we want? Can we include CWHR as a predictor?

#species used for classification
species_class <- unique(isro_inv$`Plant Symbol`)


species_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_SPECIES.csv")
# species_ref$SpeciesName <- paste0(
#   substr(species_ref$GENUS, 1, 4),
#   substr(toupper(species_ref$SPECIES), 1, 1), 
#   substr(species_ref$SPECIES, 2, 4)
# )

species_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_PLANT_DICTIONARY.csv") %>%
  dplyr::filter(SYMBOL %in% species_ref$SPECIES_SYMBOL) %>%
  mutate(shrub = grepl("Shrub", .$GROWTH_HABIT, ignore.case = TRUE)) %>%
  dplyr::select(c(SYMBOL, shrub)) %>%
  right_join(species_ref, by = c("SYMBOL" = "SPECIES_SYMBOL"))


#import all FIA data for CA
fia_trees_ca <- read.csv("fia_trees.csv") %>%
  left_join(species_ref[, c("SPCD", "SpeciesName", "shrub", "SFTWD_HRDWD")],
            by = "SPCD") %>%
  dplyr::filter(SpeciesName %in% species_class &
                  !is.na(TOTAGE)) %>%
  dplyr::mutate(SpeciesName = ifelse(shrub, "Shrub", SpeciesName)) #replace shrub species with "Shrub" functional type
# mutate(TOTAGE = BHAGE + 10) #already done in preprocess

#look at the diameter:age relationship for each species
#it's nonlinear, but should be more or less linear on a log-log scale
for(i in 1:length(unique(fia_trees_ca$SpeciesName))){
  fia_sub <- fia_trees_ca[fia_trees_ca$SpeciesName == unique(fia_trees_ca$SpeciesName)[i],]
  
  if(nrow(fia_sub[!is.na(fia_sub$TOTAGE), ]) > 5){
    plot(I((DIA)+rnorm(nrow(fia_sub),0,0.1)) ~ I((TOTAGE)+rnorm(nrow(fia_sub),0,0.11)), 
         data = fia_sub,
         main = unique(fia_trees_ca$SpeciesName)[i],
         log = "xy")
  }
}

#Charles just uses log-log
library(mgcv)
# cub_regressions <- fia_trees_ca %>% 
#   dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
#   filter(SpeciesName %in% species_class) %>%
#   dplyr::group_by(SpeciesName) %>%
#   filter(n() > 10) %>%
#   filter(TOTAGE < 500) %>%
#   dplyr::do(model = lm(log(DIA) ~ log(TOTAGE), data = .))

# cub_regressions <- fia_trees_ca %>%
#   dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
#   filter(SpeciesName %in% species_class) %>%
#   dplyr::group_by(SpeciesName) %>%
#   filter(n() > 10) %>%
#   dplyr::do(model = gam(log(DIA) ~ log(TOTAGE), data = .))

library(earth)
cub_regressions <- fia_trees_ca %>%
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SpeciesName %in% species_class) %>%
  dplyr::group_by(SpeciesName) %>%
  filter(n() > 40) %>%
  dplyr::do(model = earth(log(DIA) ~ log(TOTAGE), data = .))


for(i in 1:12){
  fia_sub <- fia_trees_ca[fia_trees_ca$SpeciesName == cub_regressions$SpeciesName[i], ]
  plot(DIA ~ TOTAGE, data = fia_sub,
       main = cub_regressions$SpeciesName[i])
  
  newdata = data.frame(TOTAGE = seq(0, max(fia_sub$TOTAGE, na.rm = TRUE), length.out = 1000))
  preds <- exp(predict(cub_regressions$model[i][[1]], newdata = newdata)) 
  
  # var(residuals(cub_regressions$model[i][[1]])))
  lines(preds ~ newdata$TOTAGE)
  abline(h = 24)
}



# 
# nls_regressions <- fia_trees_ca %>%
#   dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
#   filter(SpeciesName %in% species_class) %>%
#   dplyr::group_by(SpeciesName) %>%
#   filter(n() > 30) %>%
#   # dplyr::do(model = lm(log(DIA) ~ poly(log(TOTAGE), 1), data = .))
#   dplyr::do(model = nls(log(DIA) ~ SSlogis(log(TOTAGE), Asym, xmid, scal), data = .))
# 
# for(i in 1:22){
#   fia_sub <- fia_trees_ca[fia_trees_ca$SpeciesName == nls_regressions$SpeciesName[i], ]
#   plot(DIA ~ TOTAGE, data = fia_sub,
#        main = nls_regressions$SpeciesName[i])
# 
#   newdata = data.frame(TOTAGE = seq(0, max(fia_sub$TOTAGE, na.rm = TRUE), length.out = 1000))
#   preds <- exp(predict(nls_regressions$model[i][[1]], newdata = newdata) +
#                  var(residuals(nls_regressions$model[i][[1]])))
#   lines(preds ~ newdata$TOTAGE)
# }
# 
# 
# mixed_model <- lme4::lmer(log(DIA) ~ poly(log(TOTAGE), 2) + (1|SpeciesName), data = fia_trees_ca[!is.na(fia_trees_ca$TOTAGE),])
# 
# for(i in 1:length(unique(mixed_model@frame$SpeciesName))){
#   fia_sub <- fia_trees_ca[fia_trees_ca$SpeciesName == unique(mixed_model@frame$SpeciesName)[i], ]
#   plot((DIA) ~ TOTAGE, data = fia_sub,
#        main = unique(mixed_model@frame$SpeciesName)[i])
# 
#   newdata = data.frame(TOTAGE = seq(0, max(fia_sub$TOTAGE, na.rm = TRUE), length.out = 1000),
#                        SpeciesName = unique(mixed_model@frame$SpeciesName)[i])
#   preds <- exp(predict(mixed_model, newdata = newdata) + var(residuals(mixed_model))/2)
#   lines(preds ~ newdata$TOTAGE)
#   abline(h = 0)
#   abline(v = 0)
# }
# 
# 
# newdata <- expand.grid(TOTAGE = seq(5, 500, by = 5),
#                        SpeciesName = unique(mixed_model@frame$SpeciesName),
#                        preds_lm = NA,
#                        preds_nls = NA,
#                        preds_mixed = NA)
# for(i in 1:nrow(newdata)){
#   newdata$preds_lm[i] <- exp(predict(cub_regressions$model[match(newdata$SpeciesName[i], cub_regressions[[1]])][[1]],
#                                      newdata = data.frame(TOTAGE = newdata$TOTAGE[i]))) 
#                              # +
#                              #   var(residuals(cub_regressions$model[match(newdata$SpeciesName[i], cub_regressions[[1]])][[1]], na.rm = TRUE))/2)
#   newdata$preds_nls[i] <- exp(predict(nls_regressions$model[match(newdata$SpeciesName[i], nls_regressions[[1]])][[1]],
#                                       newdata = data.frame(TOTAGE = newdata$TOTAGE[i])) +
#                                 var(residuals(nls_regressions$model[match(newdata$SpeciesName[i], nls_regressions[[1]])][[1]], na.rm = TRUE))/2)
# 
# }
# 
# 
# newdata$preds_mixed <- exp(predict(mixed_model, newdata = newdata) +
#                              var(residuals(mixed_model))/2)
# 
# charles_data <- read.csv("pred_values_all_spp_catrees.csv") %>%
#   rename(TOTAGE = TOTAGE2,
#          preds1 = exp.pmd1.) %>%
#   left_join(species_ref[, c("COMMON_NAME", "SpeciesName")], by = c("curr_name" = "COMMON_NAME")) %>%
#   left_join(newdata, by = c("SpeciesName", "TOTAGE"))
# 
# plot(charles_data$preds_lm ~ charles_data$TOTAGE, xlim = c(0,200), ylim = c(0, 50))
# plot(charles_data$preds_nls ~ charles_data$TOTAGE, xlim = c(0,200), ylim = c(0, 50))
# plot(charles_data$preds_mixed ~ charles_data$TOTAGE, xlim = c(0,200), ylim = c(0, 50))
# abline(h = 24)
# 
# plot(charles_data$preds_lm ~ charles_data$preds1, xlim = c(0,100), ylim = c(0,100),
#      xlab = "Charles' predicted diameter",
#      ylab = "Sam's predicted diameter")
# abline(0,1)
# 
# 
# plot(charles_data[charles_data$TOTAGE < 250, ]$preds_lm ~ charles_data[charles_data$TOTAGE < 250, ]$preds1, 
#      xlim = c(0,50), ylim = c(0,50),
#      xlab = "Charles' predicted diameter (in)",
#      ylab = "Sam's predicted diameter (in)")
# abline(0,1)
# 
# plot(charles_data[charles_data$TOTAGE < 250, ]$preds_nls ~ charles_data[charles_data$TOTAGE < 250, ]$preds1, 
#      xlim = c(0,50), ylim = c(0,50),
#      xlab = "Charles' predicted diameter (in)",
#      ylab = "Sam's predicted diameter (in)")
# abline(0,1)
# 
# plot(charles_data[charles_data$TOTAGE < 250, ]$preds_mixed ~ charles_data[charles_data$TOTAGE < 250, ]$preds1, 
#      xlim = c(0,50), ylim = c(0,50),
#      xlab = "Charles' predicted diameter (in)",
#      ylab = "Sam's predicted diameter (in)")
# abline(0,1)
# 
# write.csv(charles_data, "compare_charles_sam_diameters.csv")
# 
# charles_data$diff <- charles_data$preds1 - charles_data$preds_lm
# mean(charles_data$diff, na.rm = TRUE)

# no_sp_regression <- gam(DIA ~ log(TOTAGE), data = fia_trees_ca)
no_sp_regression <- earth(log(DIA) ~ log(TOTAGE), 
                          data = fia_trees_ca[!is.na(fia_trees_ca$TOTAGE) & 
                                                !is.na(fia_trees_ca$DIA) &
                                                !(fia_trees_ca$SpeciesName %in% cub_regressions$SpeciesName), ])

write_rds(cub_regressions, "linear_models_diam_from_age.RDS")
write_rds(no_sp_regression, "linear_models_diam_from_age_no_sp.RDS")
# write_rds(mixed_model, "mixed_model_diam_from_age.RDS")

################################################################################
# Estimate age:diameter regressions

#these are needed to convert FIA plots into LANDIS biomass-age cohorts

#import all FIA data for CA
fia_trees_ca <- read.csv("D:/Data/fia/rFIA_downloads/CA_TREE.csv") %>%
  left_join(species_ref[, c("SPCD", "SpeciesName", "shrub", "SFTWD_HRDWD")],
            by = "SPCD") %>%
  dplyr::filter(SpeciesName %in% species_class) %>%
  dplyr::mutate(SpeciesName = ifelse(shrub, "Shrub", SpeciesName)) %>%#replace shrub species with "Shrub" functional type
  mutate(TOTAGE = BHAGE+10)

#look at the diameter:age relationship for each species
#it's nonlinear, but should be more or less linear on a log-log scale
for(i in 1:length(unique(fia_trees_ca$SpeciesName))){
  fia_sub <- fia_trees_ca[fia_trees_ca$SpeciesName == unique(fia_trees_ca$SpeciesName)[i],]
  
  if(nrow(fia_sub[!is.na(fia_sub$TOTAGE), ]) > 5){
    plot(I(log(TOTAGE)+rnorm(nrow(fia_sub),0,0.1)) ~ I(log(DIA)+rnorm(nrow(fia_sub),0,0.11)), 
         data = fia_sub,
         main = unique(fia_trees_ca$SpeciesName)[i])
  }
}

cub_regressions_age <- fia_trees_ca %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SpeciesName %in% species_class) %>%
  dplyr::group_by(SpeciesName) %>%
  filter(n() > 10) %>%
  dplyr::do(model = lm(log(TOTAGE) ~ poly(log(DIA), 3), data = .))

for(i in 1:18){
  fia_sub <- fia_trees[fia_trees$SpeciesName == cub_regressions_age$SpeciesName[i], ]
  plot(TOTAGE ~ DIA, data = fia_sub,
       main = cub_regressions_age$SpeciesName[i],
       ylim = c(0, 400))
  
  newdata = data.frame(DIA = seq(1, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
  preds <- exp(predict(cub_regressions_age$model[i][[1]], newdata = newdata))
  lines(preds ~ newdata$DIA)
  abline(v = 12)
}

nls_regressions_age <- fia_trees_ca %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SpeciesName %in% species_class) %>%
  dplyr::group_by(SpeciesName) %>%
  filter(n() > 10) %>%
  # dplyr::do(model = lm(log(DIA) ~ poly(log(TOTAGE), 1), data = .))
  dplyr::do(model = nls(log(TOTAGE) ~ SSlogis(DIA, Asym, xmid, scal), data = .))

for(i in 1:18){
  fia_sub <- fia_trees[fia_trees$SpeciesName == agedia_regressions$SpeciesName[i], ]
  plot(TOTAGE ~ DIA, data = fia_sub,
       main = agedia_regressions$SpeciesName[i],
       ylim = c(0, 400))
  
  newdata = data.frame(DIA = seq(0, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
  preds <- exp(predict(nls_regressions_age$model[i][[1]], newdata = newdata)) 
  lines(preds ~ newdata$DIA)
  abline(v = 24)
}


mixed_model_age <- lme4::lmer(log(TOTAGE) ~ poly(log(DIA), 3) + (1|SpeciesName), data = fia_trees_ca[!is.na(fia_trees_ca$TOTAGE),])

for(i in 1:length(unique(mixed_model_age@frame$SpeciesName))){
  fia_sub <- fia_trees_ca[fia_trees_ca$SpeciesName == unique(mixed_model_age@frame$SpeciesName)[i], ]
  plot((TOTAGE) ~ DIA, data = fia_sub,
       main = unique(mixed_model_age@frame$SpeciesName)[i],
       ylim = c(0, 400))
  
  newdata = data.frame(DIA = seq(0, max(fia_sub$TOTAGE, na.rm = TRUE), length.out = 1000),
                       SpeciesName = unique(mixed_model_age@frame$SpeciesName)[i])
  preds <- exp(predict(mixed_model_age, newdata = newdata) + var(residuals(mixed_model_age))/2)
  lines(preds ~ newdata$DIA)
  abline(h = 0)
  abline(v = 0)
  abline(v = 24)
}


#compare predictions
newdata <- expand.grid(DIA = seq(0, 200, by = 5),
                       SpeciesName = unique(cub_regressions_age$SpeciesName),
                       preds = NA)
for(i in 1:nrow(newdata)){
  newdata$preds[i] <- exp(predict(cub_regressions_age$model[match(newdata$SpeciesName[i], 
                                                                  cub_regressions_age[[1]])][[1]],
                                  newdata = data.frame(DIA = newdata$DIA[i])))
}


newdata$preds_mixed <- exp(predict(mixed_model_age, newdata = newdata) + var(residuals(mixed_model_age))/2)

charles_data <- read.csv("pred_values_all_spp_catrees.csv") %>%
  rename(TOTAGE = TOTAGE2,
         DIA = exp.pmd1.) %>%
  left_join(species_ref[, c("COMMON_NAME", "SpeciesName")], by = c("curr_name" = "COMMON_NAME"))# %>%
# left_join(newdata, by = c("SpeciesName", "DIA"))

plot(charles_data$TOTAGE ~ charles_data$DIA, xlim = c(0,50), ylim = c(0, 400))
plot(newdata$preds ~ newdata$DIA, xlim = c(0,50), ylim = c(0, 400))
plot(newdata$preds_mixed ~ newdata$DIA, xlim = c(0,50), ylim = c(0, 400))
abline(h = 100)
abline(v = 24)


write_rds(cub_regressions_age, "linear_models_age_from_diam.RDS")



gdd <- function(jan_t, july_t){
  gdday <- ((365/(2*pi)) * (july_t - jan_t)) - ((365/2) * (40 - (july_t + jan_t)/2)) +
    (365/(pi)) * (((40 - (july_t + jan_t)/2)^2)/(july_t - jan_t))
  return(gdday)
}

gdd(-16, 19)



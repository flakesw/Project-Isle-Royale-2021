#create new initial communities
# library("raster")
library("sf")
library("tidyverse")
library("RSQLite")
library("rFIA")
library("terra")

setwd("C:/Users/Sam/Documents/Research/Isle Royale")

options(warn=1)


#We need to do a few things: 
#1. map the plot data we have to the digitized polygons of vegetative community
#2. Get ages for each tree in the plot data
#3. Get biomass for each tree in the plot data
#4. Convert cover to diameter (or directly to biomass) for shrubs and saplings
#5. Convert diameter to biomass and age


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




# TODO add in alder, willow, herbs

# This script creates initial communities, 

inventory_shape <- sf::st_read("./Parameterization/Parameterization data/inventory_data/1 isrogisdata/isrogisdata/isroveg/isroveg.shp")
plot(sf::st_geometry(inventory_shape))
table(inventory_shape$PI)

#for large trees, convert to biomass and age
#for smaller trees, convert from cover to biomass; use weibull distribution to get diameters; diameters to age

# cover-biomass allometry
#Muukkonen et al. 2006 has allometry between cover and biomass for understory for Finnish upland and peatland forests
#MacDonald et al. 2012 has allometries for understory for a Canadian boreal riparian site
#Chieppa et al. 2020 has allometries for herbs/grasses and a few woody shrubs, from Australian rangelands
#Quint and Dech 2010: allometric equations for Canada yew in central Ontario

#diameter-biomass allometry
#Berner et al. 2015: alder and willow
#Frouz et al. 2015: Populus tremula and Alnus gluttinosa
#Xing et al. 2019: white spruce, quaking aspen, balsam poplar
#Ung et al. 2008: many species


#-------------------------------------------------------------------------------
# Import data from ISRO
isro_inv <- readxl::read_excel("./Parameterization/Parameterization data/inventory_data/5_isro_spcov.xls")

head(isro_inv)

isro_trees <- isro_inv %>% 
  dplyr::filter(Stratum == "T2") %>%
  mutate(BA = map_dbl(DBH, total_ba_from_DBH)) 

total_ba = isro_trees %>%
  group_by(`Plant Symbol`) %>%
  summarise(total_BA = sum(BA, na.rm = TRUE))

comm_mat_isro_trees <- isro_trees %>%
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
plot(initial_communities)
hist(values(initial_communities))
table(values(initial_communities))

terra::writeRaster(initial_communities, 
                   "./Models/LANDIS inputs/input rasters/initial_communities_inv_data_2022-11-9.tif")


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
  left_join(fia_cond_reduced, by = c("CN" = "PLT_CN")) #%>%  
  #dplyr::filter(INVYR > 2006)

#----
#Trees
fia_site_trees <-  paste0("D:/data/fia/rFIA_downloads/", c("MI", "WI", "MN"),"_SITETREE.csv") %>%
  purrr::map(read.csv) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(PLT_CN %in% fia_plot_reduced$CN) %>%
  dplyr::mutate(TOTAGE = AGEDIA + 10)

breaks <- seq(0, max(fia_site_trees$TOTAGE, na.rm = TRUE) + (10 - max(fia_site_trees$TOTAGE, na.rm = TRUE) %% 10),
              by = 5)

fia_trees <-  paste0("D:/data/fia/rFIA_downloads/", c("MI", "WI", "MN"),"_TREE.csv") %>%
  purrr::map(read.csv) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(PLT_CN %in% fia_plot_reduced$CN)


#------------------
# write outputs

write.csv(fia_plot_reduced, "./Parameterization/Parameterization data/fia/fia_plot_reduced.csv")
write.csv(fia_trees, "./Parameterization/Parameterization data/fia/fia_trees.csv")
write.csv(fia_site_trees, "./Parameterization/Parameterization data/fia/fia_site_trees.csv")


################################################################################
# Estimate diameter:age regressions

fia_site_trees  <- read.csv("./Parameterization/Parameterization data/fia/fia_site_trees.csv")

#species used for classification
species_class <- unique(isro_inv$`Plant Symbol`)

species_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_SPECIES.csv")
species_ref$SpeciesName <- paste0(
  substr(species_ref$GENUS, 1, 4),
  substr(toupper(species_ref$SPECIES), 1, 1),
  substr(species_ref$SPECIES, 2, 4)
)

species_ref2 <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_PLANT_DICTIONARY.csv") %>%
  dplyr::filter(SYMBOL %in% species_ref$SPECIES_SYMBOL) %>%
  mutate(shrub = grepl("Shrub", .$GROWTH_HABIT, ignore.case = TRUE)) %>%
  mutate(shrub = ifelse(SYMBOL %in% c("ACSA3", "QUMA"), FALSE, shrub)) %>%
  dplyr::select(c(SYMBOL, shrub)) %>%
  right_join(species_ref, by = c("SYMBOL" = "SPECIES_SYMBOL"))


#import all FIA data for
fia_site_trees2 <- fia_site_trees %>%
  left_join(species_ref2[, c("SPCD", "SpeciesName", "SYMBOL", "shrub", "SFTWD_HRDWD")],
            by = "SPCD") %>%
  dplyr::filter(SYMBOL %in% species_class &
                  !is.na(TOTAGE)) %>%
  dplyr::mutate(SpeciesName = ifelse(shrub, "Shrub", SpeciesName)) #replace shrub species with "Shrub" functional type

table(fia_site_trees2$SYMBOL)



#look at the diameter:age relationship for each species
#it's nonlinear, but should be more or less linear on a log-log scale

for(i in 1:length(unique(fia_site_trees2$SYMBOL))){
  fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == unique(fia_site_trees2$SYMBOL)[i],]
  
  if(nrow(fia_sub[!is.na(fia_sub$TOTAGE), ]) > 5){
    plot(I((DIA)+rnorm(nrow(fia_sub),0,0.1)) ~ I((TOTAGE)+rnorm(nrow(fia_sub),0,0.11)), 
         data = fia_sub,
         main = unique(fia_site_trees2$SYMBOL)[i],
         log = "xy")
  }
}

#Charles just uses log-log
library(mgcv)
cub_regressions <- fia_site_trees2 %>%
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SYMBOL)) %>%
  filter(SYMBOL %in% species_class) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  filter(TOTAGE < 500) %>%
  dplyr::do(model = lm(log(DIA) ~ log(TOTAGE), data = .))

# cub_regressions <- fia_site_trees2 %>%
#   dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
#   filter(SpeciesName %in% species_class) %>%
#   dplyr::group_by(SpeciesName) %>%
#   filter(n() > 10) %>%
#   dplyr::do(model = gam(log(DIA) ~ log(TOTAGE), data = .))

library(earth)
cub_regressions <- fia_site_trees2 %>%
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SYMBOL)) %>%
  filter(SYMBOL %in% species_class) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 40) %>%
  dplyr::do(model = earth(log(DIA) ~ log(TOTAGE), data = .))


for(i in 1:18){
  fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == cub_regressions$SYMBOL[i], ]
  plot(DIA ~ TOTAGE, data = fia_sub,
       main = cub_regressions$SYMBOL[i])
  
  newdata = data.frame(TOTAGE = seq(0, max(fia_sub$TOTAGE, na.rm = TRUE), length.out = 1000))
  preds <- exp(predict(cub_regressions$model[i][[1]], newdata = newdata)) 
  
  # var(residuals(cub_regressions$model[i][[1]])))
  lines(preds ~ newdata$TOTAGE)
  abline(h = 24)
}

no_sp_regression <- earth(log(DIA) ~ log(TOTAGE), 
                          data = fia_site_trees2[!is.na(fia_site_trees2$TOTAGE) & 
                                                !is.na(fia_site_trees2$DIA) &
                                                !(fia_site_trees2$SYMBOL %in% cub_regressions$SYMBOL), ])

write_rds(cub_regressions, "linear_models_diam_from_age.RDS")
write_rds(no_sp_regression, "linear_models_diam_from_age_no_sp.RDS")
# write_rds(mixed_model, "mixed_model_diam_from_age.RDS")

################################################################################
# Estimate age:diameter regressions

#these are needed to convert FIA plots into LANDIS biomass-age cohorts

#look at the diameter:age relationship for each species
#it's nonlinear, but should be more or less linear on a log-log scale
for(i in 1:length(unique(fia_site_trees2$SpeciesName))){
  fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == unique(fia_site_trees2$SYMBOL)[i],]
  
  if(nrow(fia_sub[!is.na(fia_sub$TOTAGE), ]) > 5){
    plot(I(log(TOTAGE)+rnorm(nrow(fia_sub),0,0.1)) ~ I(log(DIA)+rnorm(nrow(fia_sub),0,0.11)), 
         data = fia_sub,
         main = unique(fia_site_trees2$SYMBOL)[i])
  }
}

cub_regressions_age <- fia_site_trees2 %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SYMBOL %in% species_class) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  dplyr::do(model = lm(log(TOTAGE) ~ poly(log(DIA), 3), data = .))

for(i in 1:18){
  fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == cub_regressions_age$SYMBOL[i], ]
  plot(TOTAGE ~ DIA, data = fia_sub,
       main = cub_regressions_age$SYMBOL[i],
       ylim = c(0, 400),
       xlim = c(0, 24))
  
  newdata = data.frame(DIA = seq(1, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
  preds <- exp(predict(cub_regressions_age$model[i][[1]], newdata = newdata))
  lines(preds ~ newdata$DIA)
  abline(v = 12)
  
  #this has a bad functional form for most species -- small trees are too old
}

nls_regressions_age <- fia_site_trees2 %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SYMBOL %in% species_class) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  dplyr::do(model = nls(log(TOTAGE) ~ SSlogis(DIA, Asym, xmid, scal), data = ., 
                        start=list(Asym=1, xmid=10, scal=1), nls.control(warnOnly = TRUE)))

  #I couldn't get this to converge

for(i in 1:18){
  fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == nls_regressions_age$SYMBOL[i], ]
  plot(TOTAGE ~ DIA, data = fia_sub,
       main = nls_regressions_age$SYMBOL[i],
       ylim = c(0, 400),
       xlim = c(0, 24))
  
  newdata = data.frame(DIA = seq(0, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
  preds <- exp(predict(nls_regressions_age$model[i][[1]], newdata = newdata)) 
  lines(preds ~ newdata$DIA)
  abline(v = 24)
}


library("earth")
earth_regressions_age <- fia_site_trees2 %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SYMBOL %in% species_class) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  filter(DIA >= 5) %>%
  dplyr::do(model = earth(log(TOTAGE) ~ log(DIA), data = .))
for(i in 1:18){
  fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == earth_regressions_age$SYMBOL[i], ] 
  
  plot(TOTAGE ~ DIA, data = fia_sub,
       main = earth_regressions_age$SYMBOL[i],
       ylim = c(0, 400),
       xlim = c(0, 24))
  
  newdata = data.frame(DIA = seq(0, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
  preds <- exp(predict(earth_regressions_age$model[i][[1]], newdata = newdata)) 
  lines(preds ~ newdata$DIA)
  abline(v = 24)
  #there's a few species where this doesn't give us a great fuctional form: JUVI, LALA, PIMA, POTR5, POBA2,ABBA
}

log_regressions_age <- fia_site_trees2 %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SYMBOL %in% species_class) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  filter(DIA >= 5) %>%
  dplyr::do(model = lm(log(TOTAGE) ~ log(DIA), data = .))
for(i in 1:18){
  fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == earth_regressions_age$SYMBOL[i], ] 
  
  plot(TOTAGE ~ DIA, data = fia_sub,
       main = earth_regressions_age$SYMBOL[i],
       ylim = c(0, 400),
       xlim = c(0, 24))
  
  newdata = data.frame(DIA = seq(0, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
  preds <- exp(predict(log_regressions_age$model[i][[1]], newdata = newdata)) 
  lines(preds ~ newdata$DIA)
  abline(v = 24)
  # the log-log model works well enough for most species, but not for PIMA, LALA, or JUVI -- for those, we'll just
  # use linear regression to avoid weird functional forms messing up our ages
}

linear_regressions_age <- fia_site_trees2 %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SYMBOL %in% species_class) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  filter(DIA >= 5) %>%
  dplyr::do(model = lm(TOTAGE ~ I(DIA*25.4) + I((DIA*25.4)^2), data = .))
for(i in 1:18){
  fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == linear_regressions_age$SYMBOL[i], ] 
  
  plot(TOTAGE ~ DIA, data = fia_sub,
       main = linear_regressions_age$SYMBOL[i],
       ylim = c(0, 400),
       xlim = c(0, 24))
  
  newdata = data.frame(DIA = seq(0, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
  preds <- predict(linear_regressions_age$model[i][[1]], newdata = newdata)
  lines(preds ~ newdata$DIA)
  abline(v = 24)
  # the log-log model works well enough for most species, but not for PIMA, LALA, or JUVI -- for those, we'll just
  # use linear regression to avoid weird functional forms messing up our ages
}

##### Did we get all the trees we need?
isro_trees$`Plant Symbol`[!(isro_trees$`Plant Symbol` %in% log_regressions_age$SYMBOL)]
#We just need mountain-ash, mountain maple, alder, hawthorn, and chokecherry

#mountain maple (Leak 1985)
# age = 6.75 + 0.2177 * diameter (in mm) = 6.75 + 5.530 (in inches)

#mountain ash (Leak 1985)
# age = 4.58 + 0.0.7039 * diameter -0.002677 diameter^2 (in mm)

#chokecherry -- no good data available

#alder


#hawthorn



################################################################################
# Estimate biomass from diameter




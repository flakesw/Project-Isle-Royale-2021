#create new initial communities


#We need to do a few things: 
#1. map the plot data we have to the digitized polygons of vegetative community
#2. Get ages for each tree in the plot data
#3. Get biomass for each tree in the plot data
#4. Convert cover to diameter (or directly to biomass) for shrubs and saplings
#5. Convert diameter to biomass and age

#some jankiness here because other rasters are made using the treemap version,
#so we need to mask out places where treemap did not have data as well as places
#where the inventory polygons do not have data


#for large trees (in the T strata, with diameters), convert to biomass and age with FIA
#for smaller trees (in the S strata), convert from cover to biomass; convert from height to age

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
# packages


# library("raster")
library("sf")
library("tidyverse")
library("RSQLite")
library("rFIA")
library("terra")
library("earth")

setwd("C:/Users/Sam/Documents/Research/Isle Royale")

options(warn=1)

#----------------------------------------------------------------------------
# functions

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


resample <- function(x, ...) x[sample.int(length(x), ...)]

######################################################################
#set up study area polygons

inventory_shape <- sf::st_read("./Parameterization/Parameterization data/inventory_data/1 isrogisdata/isrogisdata/isroveg/isroveg.shp") %>%
  sf::st_transform("EPSG:26917", method = "near")

table(inventory_shape$PI)

isro_bound <- sf::st_read("./Parameterization/Parameterization data/isle_royale_boundary_buffer/isro_buffer.shp") %>%
  sf::st_transform("EPSG:26917", method = "near")
plot(isro_bound)
plot(sf::st_geometry(inventory_shape), add = TRUE)

# mapview::mapview(inventory_shape, layer.name = "PI") + mapview::mapview(raster(comm_map_test))

#-------------------------------------------------------------------------------
# Import data from ISRO
isro_inv <- readxl::read_excel("./Parameterization/Parameterization data/inventory_data/5_isro_spcov.xls")

head(isro_inv)

isro_trees <- isro_inv %>% 
  dplyr::filter(Stratum %in% c("T1", "T2", "T3")) %>%
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

#all trees being used
tree_names <- total_ba$`Plant Symbol`

# Plot-level data
isro_plot <- readxl::read_excel("./Parameterization/Parameterization data/inventory_data/4_isroplots.xls") %>%
  filter(Use == "T") #remove plots we don't want to use
#add zero before single-digit codes to standardize
isro_plot[nchar(isro_plot$PI) == 1, "PI"] <- paste0("0", unlist(isro_plot[nchar(isro_plot$PI) == 1, "PI"]))
isro_plot[nchar(isro_plot$Alt) == 1 & !is.na(isro_plot$Alt), "Alt"] <- 
  paste0("0", unlist(isro_plot[nchar(isro_plot$Alt) == 1 & !is.na(isro_plot$Alt), "Alt"]))
isro_plot[nchar(isro_plot$Alt2) == 1 & !is.na(isro_plot$Alt2), "Alt"] <- 
  paste0("0", unlist(isro_plot[nchar(isro_plot$Alt2) == 1 & !is.na(isro_plot$Alt2), "Alt2"]))
table(c(isro_plot$PI, isro_plot$Alt))
isro_plot$area <- isro_plot$`X Dimension` * isro_plot$`Y Dimension`

#PIs with no plots to represent them (because they're water or bedrock, and marked USE = F in the spreadsheet)
PI_not_used <- sort(unique(inventory_shape$PI)[!(unique(inventory_shape$PI) %in% c(isro_plot$PI, isro_plot$Alt, isro_plot$Alt2))])

isro_map_total_area <- inventory_shape %>%
  sf::st_drop_geometry() %>%
  group_by(PI) %>%
  summarise(total_area = sum(AREA))


#split up the entries with multiple trees
isro_trees_diam <- isro_inv[0, ]
isro_trees_diam$dia <- NA

for(i in 1:nrow(isro_inv)){
  diams <- strsplit(isro_inv[i, ]$DBH, ",")[[1]]
  temp <- isro_inv[i, ]
  
  if(length(diams) > 0){
    
    temp_dias <- as.data.frame(lapply(temp, rep, length(diams)))
    temp_dias$dia <- diams
    isro_trees_diam <- rbind(isro_trees_diam, temp_dias)
    
  } else{
    temp$dia <- NA
    isro_trees_diam <- rbind(isro_trees_diam, temp)
    
  }
  
}

isro_trees_diam$dia <- as.numeric(isro_trees_diam$dia)
isro_trees_diam[which(isro_trees_diam$dia == 289), "dia"] <- 28

# table(isro_trees_diam$dia, isro_trees_diam$Stratum)

#remove herbs, graminoids, and nonvascular plants
isro_trees_diam <- isro_trees_diam[isro_trees_diam$Stratum %in% c("S1", "S2", "S3", "T1", "T2", "T3"), ]
# isro_trees_diam <- isro_trees_diam[isro_trees_diam$Plot.Code %in% isro_plot$`Plot Code`, ] #needed?

#-------------------------------------------------------------------------------
# Create initial communities map
#-------------------------------------------------------------------------------

#create an integer mapcode for each plot
isro_plot$MapCode <- as.numeric(unlist(regmatches(isro_plot$`Plot Code`, gregexpr("[[:digit:]]+", isro_plot$`Plot Code`))))
# isro_plot <- isro_plot[!(isro_plot$MapCode %in% c("72", "113", "103", "151")), ] #remove these plots with unrealistically high biomass

inventory_shape$MapCode <- NA
PI_ignore <- c("61", "63", "67", "67A", "98", "99") #these plots are water or shoreline 


#import treemap, just for making initial communities that match up with the treemap method (see create_initial_communities_treemap.R)
#treemap is in a weird CRS; probably a NAD83 UTM, EPSG:42303
treemap <- terra::rast("./Parameterization/Parameterization data/treemap/RDS-2019-0026_Data/Data/national_c2014_tree_list.tif")

isro_bound2 <- sf::st_read("./Parameterization/Parameterization data/isle_royale_boundary_buffer/isro_buffer.shp") %>%
  sf::st_transform(crs = terra::crs(treemap))

#TODO check where the data are coming from -- some from, e.g., AR and ID might be pretty far off
treemap_isro <- terra::crop(treemap, isro_bound2) %>%
  terra::project("EPSG:26917", method = "near")

template <- terra::rast(extent = terra::ext(treemap_isro),
                        resolution = 60,
                        crs = terra::crs(treemap_isro))

treemap_isro <- terra::resample(treemap_isro, template, method = "near")

#make photointerpreted zones into a raster
initial_communities <- terra::rasterize(x = terra::vect(inventory_shape), y = template,  field = "PI")
plot(initial_communities)
hist(values(initial_communities))
table(values(initial_communities)) #these are factor levels, not the actual plot values or PI values
labs <- cats(initial_communities)[[1]] #table to convert factor level to PI

#make a vector of values to use to sample plots
pi_vals <- labs[base::match(values(initial_communities), labs[, 1]), 2]
pi_vals <- ifelse(is.na(pi_vals), 0, pi_vals)
table(pi_vals)#PI codes

new_vals <- character(length = length(pi_vals))


#this is quite slow and clunky -- maybe could use terra::classify?
#Take each PI from raster and randomly assign a plot to it
for(i in 1:length(new_vals)){
  if(pi_vals[i] %in% c(0, PI_ignore)){
    #if the PI is one of the ones we don't want to use, replace with 0 (no data)
    new_vals[i] <- 0
  } else{
    which_plots <- as.data.frame(isro_plot[, c("PI", "Alt", "Alt2")] == pi_vals[i])
    temp_plots <- isro_plot[which(which_plots$PI | which_plots$Alt | which_plots$Alt2), ]
    new_vals[i] <- resample(x = temp_plots$MapCode, size = 1) #if x has length 1, sample does some crazy stuff. Beware! Sample on the index instead
  }
}

table(new_vals)
table(new_vals[new_vals %in% isro_plot$MapCode])
table(new_vals[!(new_vals %in% isro_plot$MapCode)])

test <- cbind(pi_vals, new_vals)
test[!(new_vals %in% isro_plot$MapCode) & new_vals != 0, ] #TODO plots that ended up weird -- plots chosen are missing from isro_plot. How?

# new_vals_bak <- new_vals

new_vals <- as.numeric(new_vals)

initial_communities_mapcode <- initial_communities
values(initial_communities_mapcode) <- new_vals


terra::writeRaster(initial_communities_mapcode, 
                   "./Models/LANDIS inputs/input rasters/initial_communities_inv.tif",
                   datatype = "INT4S",
                   overwrite = TRUE)


#*******************************************************************************
# Get regressions from FIA data to estimate ages and biomass

# We need equations to get age ~ diameter*species and biomass ~ diameter (Jenkins equations)

################################################################################

# forest_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_FOREST_TYPE.csv")  
# 
# all_fia_plot <- paste0("D:/data/fia/rFIA_downloads/", c("MI", "WI", "MN"),"_PLOT.csv") %>%
#   purrr::map(read.csv) %>%
#   dplyr::bind_rows() 
# 
# 
# all_fia_cond <- paste0("D:/data/fia/rFIA_downloads/", c("MI", "WI", "MN"),"_COND.csv") %>%
#   purrr::map(read.csv) %>%
#   do.call(rbind, .) #bind_rows() doesn't work here because of some columns being cast to different data types
# 
# fia_cond_reduced <- all_fia_cond %>%
#   filter(OWNCD != 46) %>%
#   mutate(IS_FOREST = ifelse(FORTYPCD %in%(c(1:998)), 1, 0)) %>%
#   group_by(PLT_CN) %>%
#   summarise(total_cond = sum(CONDPROP_UNADJ),
#             natural = sum(STDORGCD, na.rm = TRUE),
#             treatment = sum(TRTCD1, na.rm = TRUE),
#             proportion_forest = sum(CONDPROP_UNADJ * IS_FOREST),
#             cc = sum(CONDPROP_UNADJ * LIVE_CANOPY_CVR_PCT, na.rm = TRUE)) %>%
#   filter(total_cond > 0.95 ) %>%
#   filter(proportion_forest > 0)
# 
# plot_forest_type <- all_fia_cond %>%
#   group_by(PLT_CN, FORTYPCD) %>%
#   summarise(total_fortypcd = sum(CONDPROP_UNADJ)) %>%
#   slice_max(total_fortypcd)
# 
# fia_cond_reduced <- left_join(fia_cond_reduced, plot_forest_type, by = "PLT_CN")
# fia_cond_reduced$forest_group <- forest_ref[match(fia_cond_reduced$FORTYPCD, forest_ref$VALUE), "TYPGRPCD"]
# 
# fia_plot_reduced <- all_fia_plot %>% 
#   left_join(fia_cond_reduced, by = c("CN" = "PLT_CN")) #%>%  
#   #dplyr::filter(INVYR > 2006)

#----
#Trees
# fia_site_trees <-  paste0("D:/data/fia/rFIA_downloads/", c("MI", "WI", "MN"),"_SITETREE.csv") %>%
#   purrr::map(read.csv) %>%
#   dplyr::bind_rows() %>%
#   dplyr::filter(PLT_CN %in% fia_plot_reduced$CN) %>%
#   dplyr::mutate(TOTAGE = AGEDIA + 10)
# 
# breaks <- seq(0, max(fia_site_trees$TOTAGE, na.rm = TRUE) + (10 - max(fia_site_trees$TOTAGE, na.rm = TRUE) %% 10),
#               by = 5)
# 
# fia_trees <-  paste0("D:/data/fia/rFIA_downloads/", c("MI", "WI", "MN"),"_TREE.csv") %>%
#   purrr::map(read.csv) %>%
#   dplyr::bind_rows() %>%
#   dplyr::filter(PLT_CN %in% fia_plot_reduced$CN)


#------------------
# write outputs

# write.csv(fia_plot_reduced, "./Parameterization/Parameterization data/fia/fia_plot_reduced.csv")
# write.csv(fia_trees, "./Parameterization/Parameterization data/fia/fia_trees.csv")
# write.csv(fia_site_trees, "./Parameterization/Parameterization data/fia/fia_site_trees.csv")

################################################################################
# Estimate age and biomass for trees with diameters
################################################################################
#read inputs (to save time)
fia_site_trees <- read.csv("./Parameterization/Parameterization data/fia/fia_site_trees.csv")
fia_trees <- read.csv("./Parameterization/Parameterization data/fia/fia_trees.csv")

#these are needed to convert FIA plots into LANDIS biomass-age cohorts
sp_ref <- read.csv("D:/data/fia/FIADB_REFERENCE/REF_SPECIES.csv")
names(sp_ref)

fia_site_trees2 <- fia_site_trees %>%
  left_join(dplyr::select(sp_ref, "SPCD", "SPECIES_SYMBOL"), by = c("SPCD")) %>%
  rename(SYMBOL = SPECIES_SYMBOL) %>%
  filter(SYMBOL %in% tree_names)

#look at the diameter:age relationship for each species
#it's nonlinear, but should be more or less linear on a log-log scale
# for(i in 1:length(unique(fia_site_trees2$SYMBOL))){
#   fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == unique(fia_site_trees2$SYMBOL)[i],]
# 
#   if(nrow(fia_sub[!is.na(fia_sub$TOTAGE), ]) > 5){
#     plot(I(log(TOTAGE)+rnorm(nrow(fia_sub),0,0.1)) ~ I(log(DIA)+rnorm(nrow(fia_sub),0,0.11)),
#          data = fia_sub,
#          main = unique(fia_site_trees2$SYMBOL)[i])
#   }
# }

cub_regressions_age <- fia_site_trees2 %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SYMBOL %in% tree_names) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  dplyr::do(model = lm(log(TOTAGE) ~ poly(log(DIA), 3), data = .))

# for(i in 1:18){
#   fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == cub_regressions_age$SYMBOL[i], ]
#   plot(TOTAGE ~ DIA, data = fia_sub,
#        main = cub_regressions_age$SYMBOL[i],
#        ylim = c(0, 400),
#        xlim = c(0, 24))
#   
#   newdata = data.frame(DIA = seq(1, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
#   preds <- exp(predict(cub_regressions_age$model[i][[1]], newdata = newdata))
#   lines(preds ~ newdata$DIA)
#   abline(v = 12)
#   
#   #this has a bad functional form for most species -- small trees are too old
# }

# nls_regressions_age <- fia_site_trees2 %>% 
#   dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
#   filter(SYMBOL %in% tree_names) %>%
#   dplyr::group_by(SYMBOL) %>%
#   filter(n() > 10) %>%
#   dplyr::do(model = nls(log(TOTAGE) ~ SSlogis(DIA, Asym, xmid, scal), data = ., 
#                         start=list(Asym=1, xmid=10, scal=1), nls.control(warnOnly = TRUE)))
# 
#   #I couldn't get this to converge
# 
# for(i in 1:18){
#   fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == nls_regressions_age$SYMBOL[i], ]
#   plot(TOTAGE ~ DIA, data = fia_sub,
#        main = nls_regressions_age$SYMBOL[i],
#        ylim = c(0, 400),
#        xlim = c(0, 24))
#   
#   newdata = data.frame(DIA = seq(0, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
#   preds <- exp(predict(nls_regressions_age$model[i][[1]], newdata = newdata)) 
#   lines(preds ~ newdata$DIA)
#   abline(v = 24)
# }


library("earth")
earth_regressions_age <- fia_site_trees2 %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SYMBOL %in% tree_names) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  filter(DIA >= 5) %>%
  dplyr::do(model = earth(log(TOTAGE) ~ log(DIA), data = .))
# for(i in 1:17){
#   fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == earth_regressions_age$SYMBOL[i], ]
# 
#   plot(TOTAGE ~ DIA, data = fia_sub,
#        main = earth_regressions_age$SYMBOL[i],
#        ylim = c(0, 400),
#        xlim = c(0, 24))
# 
#   newdata = data.frame(DIA = seq(0, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
#   preds <- exp(predict(earth_regressions_age$model[i][[1]], newdata = newdata))
#   lines(preds ~ newdata$DIA)
#   abline(v = 24)
#   #there's a few species where this doesn't give us a great fuctional form:
#   #JUVI, LALA, PIMA, POTR5, POBA2,ABBA
# }

log_regressions_age <- fia_site_trees2 %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SYMBOL %in% tree_names) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  filter(DIA >= 5) %>%
  dplyr::do(model = lm(log(TOTAGE) ~ log(DIA), data = .))
# for(i in 1:17){
#   fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == earth_regressions_age$SYMBOL[i], ]
# 
#   plot(TOTAGE ~ DIA, data = fia_sub,
#        main = earth_regressions_age$SYMBOL[i],
#        ylim = c(0, 400),
#        xlim = c(0, 24))
# 
#   newdata = data.frame(DIA = seq(0, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
#   preds <- exp(predict(log_regressions_age$model[i][[1]], newdata = newdata))
#   lines(preds ~ newdata$DIA)
#   abline(v = 24)
#   # the log-log model works well enough for most species, but not for PIMA, LALA, or JUVI -- for those, we'll just
#   # use linear regression to avoid weird functional forms messing up our ages
#   # use for POTR5, POBA2, ABBA
# }

linear_regressions_age <- fia_site_trees2 %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SYMBOL %in% tree_names) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  filter(DIA >= 5) %>%
  dplyr::do(model = lm(TOTAGE ~ DIA + 0, data = .))
# for(i in 1:18){
#   fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == linear_regressions_age$SYMBOL[i], ] 
#   
#   plot(TOTAGE ~ DIA, data = fia_sub,
#        main = linear_regressions_age$SYMBOL[i],
#        ylim = c(0, 400),
#        xlim = c(0, 24))
#   
#   newdata = data.frame(DIA = seq(0, max(fia_sub$DIA, na.rm = TRUE), length.out = 1000))
#   preds <- predict(linear_regressions_age$model[i][[1]], newdata = newdata)
#   lines(preds ~ newdata$DIA)
#   abline(v = 24)
#   # use for PIMA, LALA, or JUVI
# }

##### Did we get all the trees we need?
table(isro_trees$`Plant Symbol`[!(isro_trees$`Plant Symbol` %in% log_regressions_age$SYMBOL)])

#We just need mountain-ash, mountain maple, alder, hawthorn, and chokecherry

#mountain maple (Leak 1985)
# age = 6.75 + 0.2177 * diameter (in mm) = 6.75 + 5.530 (in inches)

#mountain ash (Leak 1985)
# age = 4.58 + 0.0.7039 * diameter -0.002677 diameter^2 (in mm)

#chokecherry -- no good data available -- age increase linearly with diameter
test <- fia_trees[fia_trees$SPCD == 763, ]
quantile(test$DIA, 1, na.rm = TRUE)

  #max age = 200
  #age = (200/12.8) * diameter

#alder
#no Alnus incana, but there is some Alnus glutinosa
test <- fia_trees[fia_trees$SPCD == 355, ]
quantile(test$DIA, .99, na.rm = TRUE)

  #max age = 100
  #age = (100/8.486) * diameter

#hawthorn

test <- fia_trees[fia_trees$SPCD == 500, ]
quantile(test$DIA, .99, na.rm = TRUE)

  #max age = 100
  #age = (100/8) * diameter


#most species had good earth fits
age_diam_models <- earth_regressions_age
#replace some with log-log fits
age_diam_models[c(1,5, 13, 15), ] <- log_regressions_age[c(1,5, 13,15), ]
#replace some with linear fits
age_diam_models[c(7,10), ] <- linear_regressions_age[c(7,10), ]

log_response <- c(1:17)[-c(7,10)]
log_response_sp <- age_diam_models$SYMBOL[log_response]


#-------------------------------------------------------------------------------
#assign ages to trees with diameters
#-------------------------------------------------------------------------------

# isro_trees_diam_backup <- isro_trees_diam
# isro_trees_diam <- isro_trees_diam_backup

isro_trees_diam$age <- NA
for(i in 1:nrow(isro_trees_diam)){
  sym <- isro_trees_diam$Plant.Symbol[i]
  dia_inch <- isro_trees_diam$dia[i] / 2.54
  
  if(sym %in% tree_names & !is.na(isro_trees_diam$dia[i])){
    #use regressions for trees which have them
    if(sym %in% age_diam_models$SYMBOL){
      isro_trees_diam$age[i] <- 
        predict(age_diam_models[match(sym, age_diam_models$SYMBOL), ]$model[[1]], 
                newdata = list(DIA = dia_inch))
      if(match(sym, age_diam_models$SYMBOL) %in% log_response){
        isro_trees_diam$age[i] <- exp(isro_trees_diam$age[i])
      }
    }
  }
  
  if(sym == "ACSP2"){
    #equation from Leak 1985
    isro_trees_diam$age[i] <- 6.75 + 0.2177 * (isro_trees_diam$dia[i] * 10)
  } 
  if(sym == "SODE3"){
  isro_trees_diam$age[i] <- 4.58 + 0.7039 * (isro_trees_diam$dia[i] * 10) - 
    0.002677 *((isro_trees_diam$dia[i] * 10)^2)
    if(isro_trees_diam$dia[i] >= 16 & !is.na(isro_trees_diam$dia[i])){
      isro_trees_diam$age[i] <- 55 #quadratic term makes larger SODE too small -- 
      # so if trees are larger than the largest SODE measured by Leak 1985, 
      # set age to the maximum age observed by Leak 1985
    }
  }
  if(sym == "PRVI"){
  isro_trees_diam$age[i] <- (200/12.8) * dia_inch
  } 
  if(sym == "ALIN2"){ 
  isro_trees_diam$age[i] <- (100/8.486) * dia_inch
  } 
  if(sym == "CRDO2"){
  isro_trees_diam$age[i] <- (100/8) * dia_inch
  }
  
}

plot(isro_trees_diam$dia ~ isro_trees_diam$age)
# isro_trees_diam[c(2080, 2330, 2331, 3443), ]
# plot(dia ~ age, data = isro_trees_diam[isro_trees_diam$Plant.Symbol == "POTR5", ])

#-------------------------------------------------------------------------------
# Estimate biomass from diameter

sp_ref <- read.csv("D:/data/fia/FIADB_REFERENCE/REF_SPECIES.csv")
names(sp_ref)

isro_trees_diam$jenkins_b1 <- sp_ref[match(isro_trees_diam$Plant.Symbol, sp_ref$SPECIES_SYMBOL), 
                                     "JENKINS_TOTAL_B1"]
isro_trees_diam$jenkins_b2 <- sp_ref[match(isro_trees_diam$Plant.Symbol, sp_ref$SPECIES_SYMBOL), 
                                     "JENKINS_TOTAL_B2"]
isro_trees_diam$agb <- exp(isro_trees_diam$jenkins_b1 + 
                             isro_trees_diam$jenkins_b2 * log(isro_trees_diam$dia)) * 2.2046

#TODO do all trees get assigned parameters?

isro_trees_diam$agb <- isro_trees_diam$agb * 453 #convert to g

isro_trees_diam$agb <- isro_trees_diam$agb / 
  isro_plot[match(isro_trees_diam$Plot.Code, isro_plot$`Plot Code`), ]$area #convert to g m-2

plot(agb ~ age, data = isro_trees_diam)

################################################################################
# Estimate biomass and age from cover, for shrubs and saplings
################################################################################

# minimum diameter for trees is 10 cm = 3.93 inches, so we can use regressions to find
# the maximum age of saplings

age_min <- data.frame(age_min = numeric(length(tree_names)),
                      Species = tree_names)
for(i in 1:nrow(age_min)){
  name = age_min$Species[i]
  if(name %in% age_diam_models$SYMBOL){
    age_min[match(name, age_min$Species), 1] <- predict(age_diam_models[match(name, age_diam_models$SYMBOL), 2]$model[[1]], newdata = list(DIA = 3.93))
    if(name %in% log_response_sp) age_min[i, 1] <- exp(age_min[i, 1])
  }
}
 

#TODO fix row names
#TODO fix 18s to 17s
age_min[4, 1] <- 6.75 + 0.2177 * (10 * 10)
age_min[21, 1] <- 4.58 + 0.7039 * (10 * 10) - 
  0.002677 *((10 * 10)^2)
age_min[19, 1] <- (200/12.8) * 3.93
age_min[5, 1] <- (100/8.486) * 3.93
age_min[8, 1] <- (100/8) * 3.93
  

# S3 is usually <0.5m, S2 is usually 0.5m to 1 m, and S3 is usually 1-5 m (ish)
# For trees, we can use regressions to find age ~ height

linear_regressions_age_ht <- fia_site_trees2 %>% 
  dplyr::filter(!is.na(DIA) & !is.na(TOTAGE) & !is.na(SPCD)) %>%
  filter(SYMBOL %in% tree_names) %>%
  dplyr::group_by(SYMBOL) %>%
  filter(n() > 10) %>%
  dplyr::do(model = lm(log(TOTAGE) ~ log(HT), data = .))
# for(i in 1:nrow(linear_regressions_age_ht)){
#   fia_sub <- fia_site_trees2[fia_site_trees2$SYMBOL == linear_regressions_age$SYMBOL[i], ] 
#   
#   plot(TOTAGE ~ HT, data = fia_sub,
#        main = linear_regressions_age_ht$SYMBOL[i],
#        ylim = c(0, 400),
#        xlim = c(0, 100))
#   
#   newdata = data.frame(HT = seq(0, max(fia_sub$HT, na.rm = TRUE), length.out = 1000))
#   preds <- exp(predict(linear_regressions_age_ht$model[i][[1]], newdata = newdata))
#   lines(preds ~ newdata$HT)
# }


s_age <- matrix(data = NA, nrow = 17, ncol = 3)
for(i in 1:nrow(linear_regressions_age_ht)){
  s_age[i, ] <- exp(predict(linear_regressions_age_ht[i, 2]$model[[1]], newdata = list(HT = c(0.5, 1, 2.5))))
}

s_age <- as.data.frame(s_age)
s_age[, 4] <- linear_regressions_age_ht$SYMBOL
plot(age_min[1:17, 1] ~ s_age[, 3])
abline(0,1) #not a very good relationship here at all

names(s_age) <- c("S3", "S2", "S1", "Species")

#####
# convert cover to biomass

#biomass = 2.134*cover (dwarf shrubs) (MacDonald et al. 2010)
#biomass = 2.1262*cover (dwarf shrubs; pine forests) (Muukkonen et al. 2006)
#biomass = 3.519 * cover ^ 1.18 (yew) (Quint and Dech 2010)
# isro_trees_diam_backup <- isro_trees_diam

isro_trees_diam$agb2 <- isro_trees_diam$Real.Cover * 2.1262 #dwarf shrub model
isro_trees_diam$agb3 <- 3.519 * isro_trees_diam$Real.Cover ^ 1.18 #yew model
plot(isro_trees_diam$agb2 ~ isro_trees_diam$agb3)

for(i in 1:nrow(isro_trees_diam)){
  if(is.na(isro_trees_diam$agb)[i]){
    if(isro_trees_diam$Plant.Symbol[i] %in% tree_names){
      #for tree saplings, give them the canada yew model estimate
      isro_trees_diam$agb[i] <- isro_trees_diam$agb3[i]
    } else{
      #for everything else, give them the shrub model
      isro_trees_diam$agb[i] <- isro_trees_diam$agb2[i]
    }
  }
}

#remove stragglers with missing cover data
isro_trees_diam <- isro_trees_diam[which(isro_trees_diam$agb > 0), ]

#-------------------------------------------------------------------------------
# add ages

# isro_trees_diam_backup <- isro_trees_diam

for(i in 1:nrow(isro_trees_diam)){
 if(is.na(isro_trees_diam$age[i])){
    if(isro_trees_diam$Stratum[i] %in% c("T1", "T2", "T3")){
      #remove large trees with no diameter
      isro_trees_diam$age[i] <- 0
      isro_trees_diam$agb[i] <- 0
    }else if(isro_trees_diam$Plant.Symbol[i] %in% s_age$Species){
        isro_trees_diam$age[i] <- s_age[match(isro_trees_diam$Plant.Symbol[i], s_age$Species),
                                        match(isro_trees_diam$Stratum[i], names(s_age))]
    }else if(isro_trees_diam$Stratum[i] == "S2"){
        isro_trees_diam$age[i] <- 5
    }else if (isro_trees_diam$Stratum[i] == "S1"){
        isro_trees_diam$age[i] <- 10
    }else if(isro_trees_diam$Stratum[i] == "S3"){
      isro_trees_diam$age[i] <- 1
    }
 }
}



boxplot(isro_trees_diam$age ~ isro_trees_diam$Plant.Symbol)    
boxplot(isro_trees_diam$agb ~ isro_trees_diam$Plant.Symbol)  
plot(isro_trees_diam$agb ~ as.numeric(isro_trees_diam$dia))  

age_breaks <- seq(0, max(isro_trees_diam$age, na.rm = TRUE) + (10 - max(isro_trees_diam$age, na.rm = TRUE) %% 10),
                            by = 5)

isro_trees_diam$age_bin <- as.numeric(as.character(base::cut(isro_trees_diam$age, age_breaks, labels = age_breaks[-1],
                                     right = TRUE)))

isro_trees_diam <- filter(isro_trees_diam, !is.na(age_bin), !is.na(agb))

#-------------------------------------------------------------------------------
# combine non-tree species and combine species-age cohorts

isro_trees_diam$shrub <- ifelse(isro_trees_diam$Plant.Symbol %in% tree_names, FALSE, TRUE)

isro_trees_diam$Plant.Symbol <- ifelse(isro_trees_diam$shrub, "Shrub", isro_trees_diam$Plant.Symbol)

isro_trees_diam$MapCode = isro_plot[match(isro_trees_diam$Plot.Code, isro_plot$`Plot Code`), ]$MapCode

isro_ic <- isro_trees_diam %>%
  dplyr::group_by(Plot.Code, Plant.Symbol, age_bin) %>%
  summarise(MapCode = MapCode[1],
            SpeciesName	= Plant.Symbol[1],
            CohortAge	= age_bin[1], 
            CohortBiomass = sum(agb, na.rm = TRUE),
            .groups = "drop") %>%
  dplyr::select(c(4:7))%>%
  dplyr::mutate(MapCode = as.integer(MapCode),
                CohortBiomass = as.integer(CohortBiomass),
                CohortAge = as.integer(CohortAge)) %>%
  dplyr::filter(!is.na(MapCode))


#do some checks -- looks good!
table(values(initial_communities_mapcode)[values(initial_communities_mapcode) %in% isro_ic$MapCode])
table(values(initial_communities_mapcode)[!(values(initial_communities_mapcode) %in% isro_ic$MapCode)])

# isro_ic_backup <- isro_ic
# isro_ic <- isro_ic_backup

#fix a few sites with too much biomass due to large tree diameters
isro_ic[isro_ic$MapCode == 100, "CohortBiomass"] <- round(isro_ic[isro_ic$MapCode == 100, "CohortBiomass"] * 8/30)
isro_ic[isro_ic$MapCode == 72, "CohortBiomass"] <- round(isro_ic[isro_ic$MapCode == 72, "CohortBiomass"] * 16/40)
isro_ic[isro_ic$MapCode == 113, "CohortBiomass"] <- round(isro_ic[isro_ic$MapCode == 113, "CohortBiomass"] * 12/30)
isro_ic[isro_ic$MapCode == 103, "CohortBiomass"] <- round(isro_ic[isro_ic$MapCode == 103, "CohortBiomass"] * 12/30)
isro_ic[isro_ic$MapCode == 151, "CohortBiomass"] <- round(isro_ic[isro_ic$MapCode == 151, "CohortBiomass"] * 12/30)
isro_ic[isro_ic$MapCode == 151, "CohortBiomass"] <- round(isro_ic[isro_ic$MapCode == 151, "CohortBiomass"] * 12/30)
isro_ic[isro_ic$MapCode == 28, "CohortBiomass"] <- round(isro_ic[isro_ic$MapCode == 28, "CohortBiomass"] * 14/30)


isro_ic$CohortBiomass <- ifelse(isro_ic$CohortBiomass < 20, 20, isro_ic$CohortBiomass)

write.csv(isro_ic, "./Models/LANDIS inputs/NECN files/initial_communities_inv.csv")


total_biomass <- isro_ic %>%
  group_by(MapCode) %>%
  summarise(biomass = sum(CohortBiomass))
total_biomass_rast <- terra::subst(initial_communities_mapcode, from = total_biomass$MapCode, to = total_biomass$biomass)
plot(total_biomass_rast)

#coarse debris is approximately 19% of live biomass, from a test run at ISRO
#SF 2023-4-13: actually let's try 0.023 of live biomass; 19% was too high and added too much C and N at the beginning
cwd_rast <- total_biomass_rast * 0.023
terra::writeRaster(cwd_rast, "./Models/LANDIS inputs/input rasters/dead_wood_inv.tif", 
                   datatype = "INT2S", overwrite = TRUE, NAflag = 0)

# belowground dead biomass
hist(fia_trees$DRYBIO_BG / fia_trees$DRYBIO_AG)
mean(fia_trees$DRYBIO_BG / fia_trees$DRYBIO_AG, na.rm = TRUE)

#multiply aboveground biomass by 24% for belowground biomass, then by 30% for dead root biomass

root_rast  <- total_biomass_rast * 0.24 * 0.30
terra::writeRaster(root_rast, "./Models/LANDIS inputs/input rasters/dead_root_inv.tif", 
                   datatype = "INT2S", overwrite = TRUE, NAflag = 0)


plot(CohortBiomass ~ CohortAge, data = isro_ic[isro_ic$SpeciesName == "POTR5", ])
hist(as.numeric(isro_ic[isro_ic$SpeciesName == "POTR5",]$CohortAge))

#create ecoregions--------------------------------------------------------------

ecoregions <- terra::classify(initial_communities_mapcode, 
                              matrix(c(0,0,0,1,99999,1), ncol = 3, byrow = TRUE)) #just two ecoregions, active and inactive
ecoregions2 <- terra::classify(treemap_isro, 
                               matrix(c(0,0,0,1,99999,1), ncol = 3, byrow = TRUE)) #just two ecoregions, active and inactive
values(ecoregions)[values(ecoregions2) == 0 | is.na(values(ecoregions2))] <- 0 #mask out cells where we might not have soil data

terra::writeRaster(ecoregions, "./Models/LANDIS inputs/input rasters/ecoregions_inv.tif", 
                   datatype = "INT2S", overwrite = TRUE, NAflag = 0)


#------------------------------------------------------------------------------
#Compare to remotely sensed data
total_biomass_rast <- total_biomass_rast/100

gedi <- terra::rast("D:/Data/gedi/GEDI_L4B_Gridded_Biomass_2017/data/GEDI04_B_MW019MW138_02_002_05_R01000M_MU.tif")

isro_bound_gedi <- isro_bound %>%
  sf::st_transform(crs(gedi))

gedi <- terra::crop(gedi, vect(isro_bound_gedi))

plot(gedi)

biomass_lo_res <- terra::project(total_biomass_rast, gedi)/100

plot(biomass_lo_res)

mean(values(biomass_lo_res)[values(biomass_lo_res) > 0])

mean(values(gedi)[values(biomass_lo_res) > 0], na.rm = TRUE)

#bring in nfcms layer
nfcms <- terra::rast("D:/Data/nfcms biomass/NLS_biomass.tif") %>%
  terra::project(gedi_hires) %>%
  terra::crop(total_biomass_rast)

nfcms_coarse <- terra::project(nfcms, gedi, method = "near")

values(nfcms) <- ifelse(values(total_biomass_rast) > 0, values(nfcms), 0)

#treemap data comes from the other script (create_initial_communities_treemap.R)
treemap_coarse <- terra::project(total_biomass_treemap_rast, gedi, method = "near")
plot(treemap_coarse)

plot(values(biomass_lo_res)[values(biomass_lo_res) > 0] ~ values(gedi)[values(biomass_lo_res) > 0])
abline(0,1)
abline(coef(lm(values(biomass_lo_res)[values(biomass_lo_res) > 0] ~ values(gedi)[values(biomass_lo_res) > 0])))
summary((lm(values(biomass_lo_res)[values(biomass_lo_res) > 0] ~ values(gedi)[values(biomass_lo_res) > 0])))

biomass_compare <- data.frame(ic = values(biomass_lo_res)[values(biomass_lo_res) > 0],
                              gedi = values(gedi)[values(biomass_lo_res) > 0],
                              nfcms = values(nfcms_coarse)[values(biomass_lo_res) > 0],
                              treemap = values(treemap_coarse)[values(biomass_lo_res) > 0])


theme_set(theme_bw())

ggplot(biomass_compare) + 
  labs(y = "Aboveground biomass density \n(VMIP) (Mg ha-1)", x = "Aboveground biomass density \n(GEDI) (Mg ha-1)") + 
  geom_point(color="steelblue", mapping = aes(x = gedi, y = ic)) + 
  geom_smooth(mapping = aes(x = gedi, y = ic), method = "lm", size = 2, color = "#CC4C02") +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 1)
ggplot(biomass_compare[biomass_compare$nfcms != 0, ]) + 
  labs(y = "Aboveground biomass (Initial Communities) (Mg ha-1)", x = "Aboveground biomass (NFCMS) (Mg ha-1)") + 
  geom_point(color="steelblue", mapping = aes(x = nfcms, y = ic)) + 
  geom_smooth(mapping = aes(x = gedi, y = ic), method = "lm") +
  geom_abline(slope = 1, intercept = 0)
ggplot(biomass_compare[biomass_compare$nfcms != 0, ]) + 
  labs(y = "Aboveground biomass (Initial Communities) (Mg ha-1)", x = "Aboveground biomass (Treemap) (Mg ha-1)") + 
  geom_point(color="steelblue", mapping = aes(x = treemap, y = ic)) + 
  geom_smooth(mapping = aes(x = treemap, y = ic), method = "lm") +
  geom_abline(slope = 1, intercept = 0)


biomass_diff <- biomass_lo_res/gedi
values(biomass_diff) <- ifelse(values(biomass_diff > 3), 3, values(biomass_diff))
plot(biomass_diff)

gedi_hires <- terra::project(gedi, total_biomass_rast) %>%
  terra::mask(total_biomass_rast)
gedi_hires2 <- gedi_hires
values(gedi_hires2) <- 0
values(gedi_hires2)[values(total_biomass_rast) > 1] <- values(gedi_hires)[values(total_biomass_rast) > 1]
plot(gedi_hires)
plot(gedi_hires2)
biomass_diff2 <- (total_biomass_rast/100)/gedi_hires2
values(biomass_diff2) <- ifelse(values(biomass_diff2 > 3), 3, values(biomass_diff2))
plot(biomass_diff2)


######

plot(nfcms)
plot(total_biomass_rast)
plot(gedi_hires)

plot(values(total_biomass_rast)/100 ~ values(nfcms))
summary(lm(values(total_biomass_rast)/100 ~ values(nfcms)))

biomass_compare_hires <- data.frame(ic = values(total_biomass_rast)[values(total_biomass_rast) > 0]/100,
                                    gedi = values(gedi_hires)[values(total_biomass_rast) > 0],
                                    nfcms = values(nfcms)[values(total_biomass_rast) > 0],
                                    treemap = values(total_biomass_treemap_rast)[values(total_biomass_rast) > 0])


ggplot(biomass_compare_hires) + 
  labs(y = "Aboveground biomass (Initial Communities) (Mg ha-1)", x = "Aboveground biomass (GEDI) (Mg ha-1)") + 
  geom_hex(color="steelblue", mapping = aes(x = gedi, y = ic)) + 
  geom_smooth(mapping = aes(x = gedi, y = ic), method = "lm") +
  geom_abline(slope = 1, intercept = 0)
ggplot(biomass_compare_hires) + 
  labs(y = "Aboveground biomass (Initial Communities) (Mg ha-1)", x = "Aboveground biomass (NFCMS) (Mg ha-1)") + 
  geom_hex(color="steelblue", mapping = aes(x = nfcms, y = ic)) + 
  geom_smooth(mapping = aes(x = nfcms, y = ic), method = "lm") +
  geom_abline(slope = 1, intercept = 0)
ggplot(biomass_compare_hires) + 
  labs(y = "Aboveground biomass (Initial Communities) (Mg ha-1)", x = "Aboveground biomass (Treemap) (Mg ha-1)") + 
  geom_hex(color="steelblue", mapping = aes(x = treemap, y = ic)) + 
  geom_smooth(mapping = aes(x = treemap, y = ic), method = "lm") +
  geom_abline(slope = 1, intercept = 0)


terra::writeRaster(total_biomass_rast, "./Analysis/biomass_rasters/initial_comm_total_biomass.tif", overwrite = TRUE)
terra::writeRaster(gedi_hires2, "./Analysis/biomass_rasters/gedi_downscale.tif", overwrite = TRUE)
terra::writeRaster(nfcms, "./Analysis/biomass_rasters/nfcms_coarse.tif")
terra::writeRaster(total_biomass_treemap_rast, "./Analysis/biomass_rasters/treemap_total_biomass.tif")

#create new initial communities
# library("raster")
library("sf")
library("tidyverse")
library("RSQLite")
library("rFIA")
library("terra")

setwd("C:/Users/Sam/Documents/Research/Isle Royale")

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

# for(i in 1:nrow(isro_inv)){
#   print(total_ba_from_DBH(isro_inv$DBH[i]))
# }



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
table(c(isro_plot$PI, isro_plot$Alt))

sort(unique(inventory_shape$PI)[!(unique(inventory_shape$PI) %in% c(isro_plot$PI, isro_plot$Alt))])

#-------------------------------------------------------------------------------
# inventory points to compare with FIA

inventory_points <- sf::st_read("./Parameterization/Parameterization data/inventory_data/1 isrogisdata/isrogisdata/isroplot/isroplot.shp")

################################################################################

# treemap, to compare against
# https://www.fs.usda.gov/rmrs/publications/treemap-tree-level-model-conterminous-us-forests-circa-2014-produced-imputation-fia
# Riley et al. 2021

#treemap is in a weird CRS; probably a NAD83 UTM, EPSG:42303
treemap <- terra::rast("./Parameterization/Parameterization data/treemap/RDS-2019-0026_Data/Data/national_c2014_tree_list.tif")

isro_bound <- sf::st_read("./Parameterization/Parameterization data/isle_royale_boundary_buffer/isro_buffer.shp") %>%
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
 
inventory_points <- inventory_points %>%
  sf::st_transform(crs = crs(treemap_isro))
treemap_extract <- terra::extract(treemap_isro, vect(inventory_points)) %>%
  mutate(tl_id = as.numeric(as.character(tl_id)),
         ISRO_PLOT = inventory_points$ISRO_PLOT)
treemap_extract


tl_plots <- read.csv("./Parameterization/Parameterization data/treemap/RDS-2019-0026_Data/Data/TL_CN_Lookup.txt") %>%
  filter(tl_id %in% treemap_extract$tl_id)

tl_trees <- read.csv("./Parameterization/Parameterization data/treemap/RDS-2019-0026_Data/Data/Tree_table_CONUS.txt") %>%
  filter(tl_id %in% treemap_extract$tl_id)

tree_info <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_SPECIES.csv")

treemap_fia_trees <- treemap_extract %>%
  left_join(tl_trees) %>%
  left_join(tree_info[, c("SPCD", "SPECIES_SYMBOL")], by = c("SPCD"))

#-------------------------------------------------------------------------------
# Community matrix for FIA plots

comm_mat_fia <- treemap_fia_trees %>%
  mutate(BA = pi*((DIA*2.54/2)^2) * TPA_UNADJ) %>%
  group_by(ISRO_PLOT, SPECIES_SYMBOL) %>%
  summarise(total_BA = sum(BA, na.rm = TRUE)) %>%
  mutate(total_BA = ifelse(is.na(total_BA), 0, total_BA)) %>%
  mutate(total_BA = total_BA /2.471 / 10000) %>% #convert square inches per acre to meters squared per hectare
  pivot_wider(names_from = SPECIES_SYMBOL, values_from = total_BA, values_fill = 0) %>%
  # dplyr::mutate(ISRO_PLOT = paste0("FIA", ISRO_PLOT)) %>%
  dplyr::rename(`Plot Code` = ISRO_PLOT)



comm_mat_combined <- dplyr::bind_rows(comm_mat_isro, comm_mat_fia) %>%
  dplyr::mutate(across(everything(), ~replace(., is.na(.), 0))) 
comm_mat_combined$rowsum <- rowSums(comm_mat_combined[, -1])
comm_mat_combined <- comm_mat_combined[comm_mat_combined$rowsum > 0, ]%>%
  select(!c(rowsum))


pca_data <- comm_mat_combined %>%
  ungroup() %>%
  select(!(`Plot Code`))

colsums <- colSums(pca_data[1:129, ])

comm_mat_combined <- comm_mat_combined[, c(TRUE, colsums > 1)]


pca <- vegan::rda(pca_data)
dis <- vegan::vegdist(pca_data[c(1,2), ])
biplot(pca)
pca$CA$u

dist_mat <- as.data.frame(as.matrix(vegan::vegdist(pca_data)))
dist_mat$plot <- comm_mat_combined$`Plot Code`

plots_with_matches <- comm_mat_combined$`Plot Code`[duplicated(comm_mat_combined$`Plot Code`)]
distance_isro_fia <- comm_mat_combined %>%
  filter(`Plot Code` %in% plots_with_matches) 

distance_matrix <- data.frame(Plot = plots_with_matches,
                              Dist = numeric(length(plots_with_matches)))
  
for(plot in plots_with_matches){
  plots_sp <- distance_isro_fia[distance_isro_fia$`Plot Code` == plot, ]
  dis <- vegan::vegdist(plots_sp[, -1], method = "jaccard")
  
  distance_matrix[distance_matrix$Plot == plot, "Dist"] <- dis[1]
}

distance_matrix #not a very good match at all, really. Very little overlap in species composition

################################################################################
# Make LANDIS IC for each vegetation type

#vegetation types
vegtypes <- inventory_shape$TNC_DESCRI
plot_vegtypes <- inventory_points$CLASS_NAME

plot_vegtypes %in% vegtypes


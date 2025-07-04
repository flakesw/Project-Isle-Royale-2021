library(sf)
library(tidyverse)
library(data.table)

sf::sf_use_s2(FALSE)

#geomac data are in EPSG:4269 or EPSG:4326
region <- sf::st_read("D:/Data/epa_ecoregions/us_eco_l3/us_eco_l3.shp") %>%
  filter(US_L3CODE %in% c(50,51)) %>%
  sf::st_union()%>%
  sf::st_transform("EPSG:5070")


#import and clean up 2020 perimeter data
op_data_2020 <- sf::st_read("D:/Data/nifc_op_data/Public_EventDataArchive_2020.gdb", layer = "EventPolygon") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::filter(featurecategory == "Wildfire Daily Fire Perimeter") %>%
  dplyr::filter(!is.na(polygondatetime)) %>%
  dplyr::filter(isvisible %in% c("Yes", "YES")) %>%
  sf::st_set_geometry("shape") %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(region)) %>%
  sf::st_intersection(region) %>%
  sf::st_set_geometry("shape") %>%
  dplyr::rename(incidentna = incidentname,
                perimeterd = polygondatetime,
                complexnam = complexname,
                uniquefire = irwinid
                ) %>%
  dplyr::select(c("incidentna", "perimeterd", "gisacres", "complexnam", "uniquefire", "comments")) %>%
  dplyr::mutate(fireyear = format(as.Date(perimeterd), "%Y"))


sf::write_sf(op_data_2020, "./Parameterization/Parameterization data/geomac_data/2020_perimeters_clean.shp")
  
#2021 data doesn't have complex name or comments. Inconsistent data formatting between years for sure
# but at least 2021 data has a lot more entries for polygondatetime
op_data_2021 <- sf::st_read("D:/Data/nifc_op_data/Public_EventDataArchive_2021.gdb", layer = "Event_Polygon_2021") %>%
    dplyr::filter(FeatureCategory == "Wildfire Daily Fire Perimeter") %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs = sf::st_crs(region)) %>%
    sf::st_intersection(region)%>%
    dplyr::rename_with(tolower) %>%
    sf::st_set_geometry("shape")%>%
    dplyr::filter(!is.na(polygondatetime)) %>%
    dplyr::filter(isvisible %in% c("Yes", "YES")) %>%
    dplyr::rename(incidentna = incidentname,
                  perimeterd = polygondatetime,
                  uniquefire = irwinid
                  ) %>%
    dplyr::select(c("incidentna", "perimeterd", "gisacres", "uniquefire")) %>%
    dplyr::mutate(fireyear = format(as.Date(perimeterd), "%Y"))
                  
                  
                    
sf::write_sf(op_data_2021, "./Parameterization/Parameterization data/geomac_data/2021_perimeters_clean.shp")


#2022 data
op_data_2022 <- sf::st_read("D:/Data/nifc_op_data/Operational_Data_Archive_2022/EventPolygon2022.shp") %>%
    dplyr::filter(FeatureCat == "Wildfire Daily Fire Perimeter") %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs = sf::st_crs(region)) %>%
    sf::st_intersection(region)%>%
    dplyr::rename_with(tolower) %>%
    sf::st_set_geometry("shape")%>%
    dplyr::rename(perimeterd = polygondat,
                  uniquefire = irwinid) %>%
    dplyr::filter(!is.na(perimeterd)) %>%
    dplyr::filter(isvisible %in% c("Yes", "YES")) %>%
    dplyr::select(c("incidentna", "perimeterd", "gisacres", "uniquefire")) %>%
    dplyr::mutate(fireyear = format(as.Date(perimeterd), "%Y"))

sf::write_sf(op_data_2022, "./Parameterization/Parameterization data/geomac_data/2022_perimeters_clean.shp")



#combined daily perimeter dataset (former GEOMAC dataset)
geomac_dsn <- "D:/Data/geomac_daily/Historic_Geomac_Perimeters_All_Years_2000_2018.gdb"
geomac_layers <- sf::st_layers(geomac_dsn)$name[-1]

#import each layer and do some preprocessing
daily_perims_2000_2018  <- purrr::map(geomac_layers,
                             ~st_read(dsn=geomac_dsn,layer=.)%>%
                               sf::st_transform(crs = sf::st_crs(region)) %>%
                               sf::st_intersection(region) %>%
                               dplyr::rename_with(tolower) %>%
                               dplyr::mutate(gisacres = as.numeric(gisacres), 
                                             perimeterd = ifelse(is.character(perimeterdatetime), 
                                                                 as.character(as.Date(clock::year_month_day_parse(perimeterdatetime, format = c("%m/%d/%y", "%m/%d/%y %I:%M:%S %p")))),
                                                                 as.character(as.Date(perimeterdatetime)))) %>%
                               dplyr::rename(c(incidentna = incidentname, uniquefire = uniquefireidentifier)) %>%
                               dplyr::select(c("incidentna", "perimeterd", "fireyear", "gisacres", "uniquefire", "comments")))

op_data_2020$perimeterd <- as.character(op_data_2020$perimeterd)
op_data_2021$perimeterd <- as.character(op_data_2021$perimeterd)
op_data_2022$perimeterd <- as.character(op_data_2022$perimeterd)
op_data_2020$fireyear <- as.integer(op_data_2020$fireyear)
op_data_2021$fireyear <- as.integer(op_data_2021$fireyear)
op_data_2022$fireyear <- as.integer(op_data_2022$fireyear)

daily_perims_all <- bind_rows(daily_perims_2000_2018, op_data_2020, op_data_2021, op_data_2022)

write_sf(daily_perims_all, "./Parameterization/Parameterization data/geomac_data/geomac_all_years.gpkg")

#import all perimeter shapefiles in the folder, including cleaned up 2020 and 2021 data
perims_file_list <- list.files(path = "./Parameterization/calibration data/geomac_all_years", pattern = "*.shp$", #the dollar sign matches *.shp at the end of the string
             full.names = TRUE, recursive = TRUE)

shapefile_list <- lapply(perims_file_list, sf::st_read) %>%
  purrr::map(., ~ sf::st_set_precision(., 1000000)) %>%
  purrr::map(., ~ sf::st_make_valid(., NA_on_exception)) %>%
  purrr::map(., ~ sf::st_intersection(., sierra))

#The shapefiles have numbers of columns, names, column orders.
#just select important columns; all these are shared between the different shapefiles
shapefile_list_subset <- shapefile_list %>%
  purrr::map_if(., ~ "complexnam" %in% names(.), ~ dplyr::select(., c("incidentna", "perimeterd", "fireyear", "gisacres", "complexnam", "uniquefire", "comments"))) %>%
  purrr::map(., ~ dplyr::mutate(., perimeterd = as.character(perimeterd)))

perims_2000_2022 <- sf::st_as_sf(data.table::rbindlist(shapefile_list_subset, use.name = TRUE, fill = TRUE))

sf::write_sf(st_collection_extract(perims_2000_2022, "POLYGON"), "./Parameterization/calibration data/geomac_all_years/perims_2000_2022.shp")


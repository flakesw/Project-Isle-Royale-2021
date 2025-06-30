#compare species compositions

library("tidyverse")
library("terra")



template <- rast("./Models/LANDIS inputs/input rasters/ecoregions.tif")

theme_set(theme_bw())

#what folder do all the runs to be analyzed live in?
scenario_folder <- "E:/ISRO LANDIS/Model runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/"
scenario_folder <- "./Models/Model templates"
# scenario_folder <- "./Models/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) #%>%
# `[`(!(grepl("canesm", .)))
scenarios <- scenarios[c(1)]

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm)))) 
  
}

get_browse <- function(scenario){
  list.files(scenario, pattern = "Scenario") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1)
}

get_climate <- function(scenario){
  list.files(scenario, pattern = "NECN_Succession") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1)
}


#-----------------------------------------------
# Process tabular data
#-----------------------------------------------

scenario_type <- data.frame(run_name = character(length(scenarios)), 
                            browse = character(length(scenarios)),
                            climate = character(length(scenarios)))


scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  mutate(browse = ifelse(grepl(pattern = "pred1", run_name), "Low", 
                         ifelse(grepl(pattern = "pred2", run_name), "Medium",
                                "High"))) %>%
  # mutate(browse = c("Low pred", "Hi pred", "Low pred", "Hi pred")) %>%
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "Very Hot (MIROC-ESM-CHEM 8.5)", 
                          ifelse(grepl(pattern = "canesm", run_name), "Hot/Dry (CanESM2 8.5)",
                                 ifelse(grepl(pattern = "ccsm", run_name), "Warm (CCSM4 4.5)", 
                                        ifelse(grepl(pattern = "mri_cgm", run_name), "Hot/Wet (MRI-CGCM3 8.5)", "Present Climate"))))) %>%
  mutate(browse = factor(browse, levels = c("Low", "Medium", "High")),
         climate = factor(climate, levels = unique(climate)[c(3,2,1,4,5)]))


func_types <- data.frame(FunctionalGroupIndex = seq(1, 10),
                         Type = c("Northern hardwood/conifer",
                                  "Boreal conifer",
                                  "Temperate hardwood/conifer",
                                  "Boreal conifer",
                                  "Boreal conifer",
                                  "Northern hardwood/conifer",
                                  "Boreal hardwood",
                                  "Temperate hardwood/conifer",
                                  "Shrubs",
                                  "Shrubs"))
spp_table <- read.csv("./Models/LANDIS inputs/NECN files/NECN_Spp_Table_inv.csv") %>%
  left_join(func_types)
spp_table[spp_table$SpeciesCode == "BEPA", "Type"] <- "Boreal hardwood"
spp_table[spp_table$SpeciesCode == "FRNI", "Type"] <- "Boreal hardwood"
# spp_table[spp_table$SpeciesCode == "ABBA", "Type"] <- "Balsam fir"

comm_mat_orig <- read.csv(paste0(scenarios, "/community-input-file-0.csv")[1]) %>%
  group_by(MapCode, SpeciesName) %>%
  summarize(Biomass = sum(CohortBiomass), .groups = "drop")

comm_mat_ft <- comm_mat_orig %>%
  left_join(dplyr::select(spp_table, SpeciesCode, Type), by = c("SpeciesName" = "SpeciesCode")) %>%
  group_by(MapCode, Type) %>%
  summarise(Biomass = sum(Biomass))%>% 
  slice(which.max(Biomass))

comm_mat_ft$Type2 <- as.numeric(as.factor(comm_mat_ft$Type))

map <- rast(paste0(scenarios, "/output-community-0.img")[1]) %>%
  terra::classify(rcl = comm_mat_ft[, c(1,4)])
map <- as.factor(map)

levels(map) = data.frame(value=c(1:5), desc=levels(as.factor(comm_mat_ft$Type)))
plot(map)

the_palette_fc <- leaflet::colorNumeric(palette = "Accent", 
                                        # domain = c(-max(abs(ras[]), na.rm = TRUE), max(abs(ras[]), na.rm = TRUE)),
                                        domain = c(1,10),
                                        reverse = FALSE)
the_colors <- data.frame(values = c(1:5), col = the_palette_fc(c(2,4,6,8,10)))

coltab(map) <- the_colors
plot(map)

map2 <- template
map2[]<-map[]

levels(map2) = data.frame(value=c(1:5), desc=levels(as.factor(comm_mat_ft$Type)))
coltab(map2) <- the_colors
plot(map2)

writeRaster(map2, "./Analysis/Plots/initial_landcover.tiff")

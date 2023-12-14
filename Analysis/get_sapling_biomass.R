#ordination
library(tidyverse)
library(lemon)
library(cowplot)
# library(vegan)
# library("BiodiversityR")

func_types <- data.frame(FunctionalGroupIndex = seq(1, 10),
                         Type = c("Temperate conifer", 
                                  "Boreal conifer",
                                  "Temperate conifer",
                                  "Boreal conifer",
                                  "Boreal conifer",
                                  "Temperate hardwood",
                                  "Boreal hardwood",
                                  "Temperate hardwood",
                                  "Wetland",
                                  "Temperate hardwood"))
spp_table <- read.csv("./Models/LANDIS inputs/NECN files/NECN_Spp_Table_inv.csv") %>%
  left_join(func_types)
spp_table[spp_table$SpeciesCode == "BEPA", "Type"] <- "Boreal hardwood"
spp_table[spp_table$SpeciesCode == "ABBA", "Type"] <- "Balsam fir"

#what folder do all the runs to be analyzed live in?
scenario_folder <- "E:/ISRO LANDIS/Model runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/"
# scenario_folder <- "./Models/Model templates"
# scenario_folder <- "./Models/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) #%>%
# `[`(!(grepl("canesm", .)))
# scenarios <- scenarios[c(1, 4, 5, 6)]

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm)))) 
  
}

read_comm_mats <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm))),
           timestep = str_extract(flnm, "[0-9]+(?=\\.)"))
  
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


comm_mat_orig <- read.csv(paste0(scenarios, "/community-input-file-0.csv")[49]) %>%
  group_by(MapCode, SpeciesName) %>%
  summarize(Biomass = sum(CohortBiomass), .groups = "drop")

spp_to_use <- comm_mat_orig %>%
  group_by(SpeciesName) %>%
  summarize(biomass = sum(Biomass)) %>%
  slice_max(order_by = biomass, n = 15)

comm_mat_orig <- comm_mat_orig %>%
  filter(SpeciesName %in% spp_to_use$SpeciesName) %>%
  pivot_wider(names_from = SpeciesName, values_from = Biomass, values_fill = 0) %>%
  slice_sample(n = 200) %>%
  arrange(MapCode)



comm_mat2 <- scenario_type %>% 
  group_by(browse, climate) %>% 
  slice(1) %>% #select 1 replicate per scenario combo
  ungroup() %>%
  pull(run_name) %>%
  paste0("E:/ISRO LANDIS/Model runs/", ., "/community-input-file-80.csv") %>%
  purrr::map_df(~read_comm_mats(.))


saplings <- comm_mat2 %>%
  filter(CohortAge <= 10) %>%
  group_by(run_name, MapCode, SpeciesName) %>%
  summarize(Biomass = sum(CohortBiomass), .groups = "drop") %>%
  filter(SpeciesName %in% spp_to_use$SpeciesName) %>%
  group_by(run_name) %>%
  pivot_wider(names_from = SpeciesName, values_from = Biomass, values_fill = 0) %>%
  # filter(MapCode %in% comm_mat_orig$MapCode) %>%#match map codes to original
  arrange(MapCode)

sapling_type <- saplings %>%
  pivot_longer(cols = Shrub:PIMA, names_to = "SpeciesName", values_to = "Biomass") %>%
  left_join(spp_table %>% select(SpeciesCode, Type), by = c("SpeciesName" = "SpeciesCode")) %>%
  group_by(run_name, MapCode, Type) %>%
  summarize(Biomass = sum(Biomass)) %>%
  left_join(scenario_type, by = "run_name")


# 
#   pivot_wider(names_from = Type, values_from = Biomass, values_fill = 0) %>%
#   arrange(MapCode) %>%
  

sapling_comp <- ggplot(data = sapling_type, 
                  mapping = aes(x = Type, y = Biomass, color = climate)) + 
  # geom_area(position = "stack") +
  geom_boxplot() +
  labs(y = "Aboveground biomass (g m-2)", x = "Simulation Year") +
  facet_wrap(facets = "browse") + 
  guides(colour=guide_legend(title="Functional Group"))
plot(sapling_comp)



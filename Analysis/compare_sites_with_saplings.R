#ordination
library(tidyverse)
library(lemon)
library(cowplot)
# library(vegan)
# library("BiodiversityR")
source("./Analysis/r_functions.R")

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


comm_files <- grep("community-input-file-[[:digit:]]+.csv", list.files(scenarios, full.names = TRUE),
                   value = TRUE)


sapling_counts <- comm_files %>% 
  purrr::map_df(~read_comm_mats(.) %>%
                  filter(CohortAge <= 10) %>%
                  group_by(run_name, timestep, SpeciesName) %>%
                  summarize(count = n(), .groups = "drop") %>%
                  group_by(run_name) %>%
                  pivot_wider(names_from = SpeciesName, values_from = count, values_fill = 0)
                )

sapling_type <- sapling_counts %>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = !c("run_name", "timestep"), names_to = "SpeciesName", values_to = "count") %>%
  left_join(spp_table %>% select(SpeciesCode, Type), by = c("SpeciesName" = "SpeciesCode")) %>%
  group_by(run_name, timestep, Type) %>%
  summarize(count = sum(count)) %>%
  left_join(scenario_type, by = "run_name") %>%
  mutate(timestep = as.numeric(timestep))

  
#all combinations of scenarios
sapling_comp <- ggplot(data = sapling_type, 
                  mapping = aes(x = timestep + 2020, y = count, color = Type)) + 
  # geom_area(position = "stack") +
  geom_point() +
  geom_smooth() +
  labs(y = "Number of cohorts", x = "Simulation Year") +
  facet_wrap(facets = c("browse", "climate"), ncol = 5) #+ 
  # guides(colour=guide_legend(title="Functional Group"))
plot(sapling_comp)



# sapling_comp <- ggplot(data = sapling_type[sapling_type$climate != "Present Climate", ], 
#                        mapping = aes(x = timestep + 2020, y = count, color = Type)) + 
#   # geom_area(position = "stack") +
#   geom_point() +
#   geom_smooth() +
#   labs(y = "Number of cohorts", x = "Simulation Year") +
#   facet_wrap(facets = c("browse"), ncol = 5) + 
#   guides(colour=guide_legend(title="Functional Group"))
# plot(sapling_comp)

sapling_comp_clim <- ggplot(data = sapling_type[sapling_type$browse == "High", ], 
                  mapping = aes(x = timestep +2019, y = count, color = Type)) + 
  # geom_area(position = "stack") +
  geom_point() +
  geom_smooth() +
  labs(y = "Number of cohorts", x = "Simulation Year") +
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Functional Group"))

# sp_comp <- tag_facet(sp_comp)
sapling_comp_clim <- shift_legend2(sapling_comp_clim)
plot(sapling_comp_clim)



sapling_comp_pred <- ggplot(data = sapling_type[sapling_type$climate == "Present Climate", ], 
                       mapping = aes(x = timestep + 2020, y = count, color = Type)) + 
  # geom_area(position = "stack") +
  geom_point() +
  geom_smooth() +
  labs(y = "Number of cohorts", x = "Simulation Year") +
  facet_wrap(facets = c("browse"), ncol = 5) + 
  guides(colour=guide_legend(title="Functional Group")) +
  theme(legend.position = "none")
plot(sapling_comp_pred)



c_grid <- cowplot::plot_grid(sapling_comp_clim, sapling_comp_pred,
                             align = "v", 
                             nrow = 2, ncol = 1,
                             labels = "auto",
                             rel_heights = c(1,0.6))
plot(c_grid)


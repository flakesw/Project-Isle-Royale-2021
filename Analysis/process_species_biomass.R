# make maps and trajectories for important species


library("tidyverse")
library("terra")

#what folder do all the runs to be analyzed live in?
scenario_folder <- "E:/ISRO LANDIS/"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) #%>%
# `[`(grep("Scenario", .))

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm)))) 
  
}

# get_browse <- function(scenario){
#   list.files(scenario, pattern = "Scenario") %>%
#     pluck(1) %>%
#     as.character() %>%
#     strsplit(x = ., split = "[.]") %>%
#     pluck(1, 1)
# }

get_climate <- function(scenario){
  list.files(scenario, pattern = "NECN_Succession") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1)
}

scenario_type <- data.frame(run_name = character(length(scenarios)), 
                            browse = character(length(scenarios)),
                            climate = character(length(scenarios))) %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  mutate(browse = ifelse(grepl(pattern = "no browse", run_name), "No Browse", "Browse")) %>%
  mutate(climate = ifelse(grepl(pattern = "no cc", run_name), "MIROC", "Historical"))


scen <- scenarios[1]

biomass_layers <- list.files(paste0(scen, "/", "biomass"))

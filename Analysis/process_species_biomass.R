# make maps and trajectories for important species


library("tidyverse")
library("terra")

#what folder do all the runs to be analyzed live in?
scenario_folder <- "E:/ISRO LANDIS/new runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) #%>%
# `[`(grep("Scenario", .))

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm)))) 
  
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
                            climate = character(length(scenarios))) %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  mutate(browse = ifelse(grepl(pattern = "no browse", run_name), "No Browse", "Browse")) %>%
  mutate(climate = ifelse(grepl(pattern = "historical", run_name), "MIROC", "Historical"))


biomass_summaries <- paste0(scenarios, "/spp-biomass-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  filter(EcoName == "eco1") %>%
  pivot_longer(cols =  starts_with("AboveGroundBiomass"),
               names_prefix = "AboveGroundBiomass_",
               names_to = "Species",
               values_to = "Biomass")
  

spp <- unique(biomass_summaries$Species)
for(sp in spp){
  
  print(ggplot(data = filter(biomass_summaries, Species == sp), mapping = aes(x = Time, y = Biomass)) + 
    geom_point(color="steelblue") + 
    labs(title = paste("Aboveground biomass", sp),
         subtitle = "by browse scenario and climate scenario",
         y = "Average AGB (g m-2)", x = "Timestep") + 
    geom_smooth( color = "black") + 
    facet_wrap(~ browse + climate, nrow = 3, ncol = 2) )
  
}



## process rasters

biomass_layers <- list.files(paste0(scen, "/", "output_20"), full.names = TRUE)

biomass_df <- data.frame(biomass_layers = biomass_layers,
                         species = map(strsplit(biomass_layers, split = "-"), pluck(3,1)) %>% #revise this for other naming conventions
                           unlist(),
                         years = str_extract_all(biomass_layers,"[0-9]+(?=\\.)") %>% unlist())

species_list <- unique(biomass_df$species)
year_list <- unique(biomass_df$years)
sp <- species_list[8]
for(sp in species_list){
  test <- rast(biomass_df[biomass_df$species == sp, "biomass_layers"])
}

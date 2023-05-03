# make maps and trajectories for important species


library("tidyverse")
library("terra")


theme_set(theme_bw())

#what folder do all the runs to be analyzed live in?
# scenario_folder <- "E:/ISRO LANDIS/new runs"
# scenario_folder <- "./Models/Model templates"
# scenarios <- list.dirs(scenario_folder, recursive = FALSE) #%>%
# # `[`(grep("Scenario", .))

scenario_folder <- "./Models/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE)
# scenarios <- scenarios[c(1, 4, 5, 6)]


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

# scenario_type <- data.frame(run_name = character(length(scenarios)), 
#                             browse = character(length(scenarios)),
#                             climate = character(length(scenarios))) %>%
#   mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
#   mutate(browse = ifelse(grepl(pattern = "no browse", run_name), "No Browse", "Browse")) %>%
#   mutate(climate = ifelse(grepl(pattern = "historical", run_name), "RCP8.5", "Present climate"))
scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  # mutate(browse = ifelse(grepl(pattern = "no browse", run_name), "No Browse", "Browse")) %>%
  mutate(browse = c("Low pred", "Hi pred", "Low pred", "Hi pred")) %>%
  # mutate(climate = ifelse(grepl(pattern = "historical", run_name), "Present Climate", "RCP8.5"))
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "RCP8.5", "Present Climate"))



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
  
p <- ggplot(data = filter(biomass_summaries, Species == sp), 
         mapping = aes(x = Time, y = Biomass/100, colour = browse)) + 
    geom_point(aes(shape = climate)) + 
    labs(title = paste(paste0(sp, " aboveground biomass")),
         subtitle = "by browse scenario and climate scenario",
         y = "Average AGB (Mg/ha)", x = "Timestep") + 
    geom_smooth(aes(linetype = climate))
plot(p)
  
}


abba_biomass <- ggplot(data = filter(biomass_summaries, Species == "ABBA"), 
                       mapping = aes(x = Time, y = Biomass/100, colour = browse)) + 
        geom_point(aes(shape = climate)) + 
        labs(title = paste("Balsam fir aboveground biomass"),
             subtitle = "by browse scenario and climate scenario",
             y = "Average AGB (Mg/ha)", x = "Timestep") + 
        geom_smooth(aes(linetype = climate))
plot(abba_biomass)
# ggsave(file="abba.svg", plot=abba_biomass, width=5, height=4)

pigl_biomass <- ggplot(data = filter(biomass_summaries, Species == "PIGL"), 
                       mapping = aes(x = Time, y = Biomass/100, colour = browse)) + 
  geom_point(aes(shape = climate)) + 
  labs(title = paste("Balsam fir aboveground biomass"),
       subtitle = "by browse scenario and climate scenario",
       y = "Average AGB (Mg/ha)", x = "Timestep") + 
  geom_smooth(aes(linetype = climate))
plot(abba_biomass)
# ggsave(file="abba.svg", plot=abba_biomass, width=5, height=4)

beal_biomass <- ggplot(data = filter(biomass_summaries, Species == "BEAL2"), 
                       mapping = aes(x = Time, y = Biomass/100, colour = browse)) + 
  geom_point(aes(shape = climate)) + 
  labs(title = paste("Yellow birch aboveground biomass"),
       subtitle = "by browse scenario and climate scenario",
       y = "Average AGB (Mg/ha)", x = "Timestep") + 
  geom_smooth(aes(linetype = climate))
plot(beal_biomass)
ggsave(file="bepa.svg", plot=abba_biomass, width=5, height=4)

pigl_biomass <- ggplot(data = filter(biomass_summaries, Species == "PIGL"), 
                       mapping = aes(x = Time+2020, y = Biomass/100, colour = browse)) + 
  geom_point(aes(shape = climate)) + 
  labs(title = paste("White spruce aboveground biomass"),
       subtitle = "by browse scenario and climate scenario",
       y = "Average AGB (Mg/ha)", x = "Simulation Year") + 
  geom_smooth(aes(linetype = climate))
plot(pigl_biomass)
ggsave(file="pigl.svg", plot=pigl_biomass, width=8, height=4)

potr_biomass <- ggplot(data = filter(biomass_summaries, Species == "POTR5"), 
                       mapping = aes(x = Time+2020, y = Biomass/100, colour = browse)) + 
  geom_point(aes(shape = climate)) + 
  labs(title = paste("Quaking aspen aboveground biomass"),
       subtitle = "by browse scenario and climate scenario",
       y = "Average AGB (Mg/ha)", x = "Simulation Year") + 
  geom_smooth(aes(linetype = climate))
plot(potr_biomass)
ggsave(file="potr.svg", plot=potr_biomass, width=8, height=4)




## process rasters

biomass_layers <- list.files(paste0(scenarios, "/", "biomass"), full.names = TRUE)

biomass_df <- data.frame(biomass_layers = biomass_layers,
                         species = basename(biomass_layers) %>%
                           str_extract(., "[^-]+"),
                         years = str_extract_all(biomass_layers,"[0-9]+(?=\\.)") %>% unlist())

species_list <- unique(biomass_df$species)
year_list <- unique(biomass_df$years)
sp <- species_list[8]
for(sp in species_list){
  test <- rast(biomass_df[biomass_df$species == sp, "biomass_layers"])
}
plot(test)

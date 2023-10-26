#compare species compositions

library("tidyverse")
library("terra")


theme_set(theme_bw())

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

regen_summaries <- paste0(scenarios, "/NECN-reproduction-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  mutate(TotalCohorts = NumCohortsPlanting + NumCohortsSerotiny + NumCohortsResprout + NumCohortsSeed)


biomass_summaries <- paste0(scenarios, "/spp-biomass-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  filter(EcoName == "eco1") %>%
  pivot_longer(cols =  starts_with("AboveGroundBiomass"),
               names_prefix = "AboveGroundBiomass_",
               names_to = "Species",
               values_to = "Biomass")

browsekill_summaries <- paste0(scenarios, "/browse-event-species-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(regen_summaries, by = c("run_name", "Time", "SpeciesName")) %>%
  mutate(NetRegen = TotalCohorts - TotalCohortsKilled)

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

biomass_summaries2 <- biomass_summaries %>%
  left_join(spp_table %>% select(SpeciesCode, Type), by = c("Species" = "SpeciesCode")) %>%
  filter(browse == "Low") %>%
  group_by(climate, Time, Species) %>%
  summarise(Biomass = mean(Biomass), #average over runs
            Type = Type[1]) %>%
  group_by(climate, Time, Type) %>%
  summarise(Biomass = sum(Biomass)) #sum within functional type
#---------------------------------
# Make figures of tabular data
#----------------------------------

sp_comp <- ggplot(data = biomass_summaries2, 
                   mapping = aes(x = Time+2019, y = Biomass, fill = Type, color = Type)) + 
  # geom_area(position = "stack") +
  geom_line() +
  labs(y = "Aboveground biomass (g m-2)", x = "Simulation Year") +
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Functional Group"))

sp_comp <- tag_facet(sp_comp)
sp_comp <- shift_legend2(sp_comp)
plot(sp_comp)

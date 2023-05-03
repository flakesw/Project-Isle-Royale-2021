#process browse

library(tidyverse)

#what folder do all the runs to be analyzed live in?
# scenario_folder <- "E:/ISRO LANDIS/new runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/"
# scenario_folder <- "./Models/Model templates"
scenario_folder <- "./Models/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) #%>%
# `[`(grep("Scenario", .))
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

scenario_type <- data.frame(run_name = character(length(scenarios)), 
                            browse = character(length(scenarios)),
                            climate = character(length(scenarios)))

scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  # mutate(browse = ifelse(grepl(pattern = "no browse", run_name), "No Browse", "Browse")) %>%
  mutate(browse = c("Low pred", "Hi pred", "Low pred", "Hi pred")) %>%
  # mutate(climate = ifelse(grepl(pattern = "historical", run_name), "Present Climate", "RCP8.5"))
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "RCP8.5", "Present Climate"))

browse_summaries <- scenario_type %>%
  # filter(scenario_type$browse == "Browse") %>%
  dplyr::select(run_name) %>%
  map(., .f = ~ paste0(scenario_folder, "/", ., "/browse-summary-log.csv"))  %>%
  unlist() %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

browse_summaries2 <- browse_summaries %>%
  group_by(run_name, Time) %>%
  summarise(TotalPopulation = weighted.mean(TotalPopulation, TotalSites),
            AverageForage =  weighted.mean(MeanForage, TotalSites),
            AverageBiomassKilled = weighted.mean(AverageBiomassKilled, TotalSites),
            AverageBiomassRemoved = weighted.mean(AverageBiomassRemoved, TotalSites),
            TotalK = TotalK,
            browse = browse[1],
            climate = climate[1])

# browse_summaries2 <- browse_summaries2[which(browse_summaries2$Time <= 21), ]


area <- browse_summaries$TotalSites[1] * 60 * 60 / 1000000
browse_summaries2$K_density <- browse_summaries2$TotalK/area
browse_summaries2$Pop_density <- browse_summaries2$TotalPopulation/area

historical_moose <- readxl::read_excel("./Models/LANDIS inputs/browse/moose_population_record_isro.xlsx",
                                       col_names = c("Year", "Wolf_pop", "Moose_pop"))

#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------

browse_summaries_melt <- pivot_longer(browse_summaries2, cols = c(TotalPopulation:TotalK, K_density, Pop_density),
                                      names_to = "Variable")
browse_summaries_melt
#AGB over time

moosepop <- ggplot(data = browse_summaries2, mapping = aes(x = Time+2019, y = TotalPopulation)) + 
  geom_point(aes(colour = browse, shape = climate)) + 
  labs(title = "Total Moose Population",
       subtitle = "by browse scenario and climate scenario",
       y = "Total Moose Population", x = "Simulation Year") +
  geom_smooth(aes(linetype = climate, colour = browse)) + 
  geom_point(data = historical_moose, mapping = aes(x = Year, y = Moose_pop)) + 
  geom_line(data = historical_moose, mapping = aes(x = Year, y = Moose_pop))
plot(moosepop)
# ggsave(file="moosepop.svg", plot=moosepop, width=5, height=4)


mooseK <- ggplot(data = browse_summaries_melt[browse_summaries_melt$Variable %in% c("Pop_density","K_density"),], 
                 mapping = aes(x = Time+1998, y = value)) + 
  geom_point(aes(colour = browse, shape = Variable)) + 
  labs(title = "Moose Population density and Carrying Capacity density",
       subtitle = "by browse scenario and climate scenario",
       y = "Number of moose per km2", x = "Simulation Year") + 
  geom_smooth(aes(colour = browse, linetype = Variable)) + 
  facet_wrap(facets = ~climate)
plot(mooseK)
ggsave(file="mooseK.svg", plot=mooseK, width=5, height=4)

ggplot(data = browse_summaries2, mapping = aes(x = Time, y = AverageForage)) + 
  geom_point(aes(colour = browse, shape = climate)) + 
  labs(title = "Average forage density (g m-2)",
       subtitle = "by browse scenario and climate scenario",
       y = "Average Forage (g m-2)", x = "Timestep") + 
  geom_smooth(aes(linetype = climate, colour = browse))

browse_kill <- ggplot(data = browse_summaries2, mapping = aes(x = Time + 2020, y = AverageBiomassKilled)) + 
  geom_point(aes(colour = browse)) + 
  labs(title = "Average biomass killed by moose",
       subtitle = "by climate scenario",
       y = expression(paste("Biomass killed (g ", m^{-2}, yr^{-1}, ")")), x = "Simulation Year") +
  geom_smooth(aes(linetype = climate, color = browse)) + 
  scale_color_manual(values=c("#56B4E9", "#E69F00"))
plot(browse_kill)
ggsave(file = "browsekill.svg", plot = browse_kill, width = 5, height = 4)

#-------------------------------------------------------------------------------
#Forage maps






# Wrangle the NECN biomass tables

# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

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

# scenario_type$fire_model <- rep(c("fixed", "mixed"), each = 3)

necn_summaries <- paste0(scenarios, "/NECN-succession-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  mutate(TotalC = SOMTC + C_LiveLeaf + C_LiveFRoot + C_LiveWood + C_LiveCRoot + C_DeadWood +
           C_DeadCRoot + C_DeadLeaf_Struc + C_DeadLeaf_Meta + C_DeadFRoot_Struc + C_DeadFRoot_Meta,
         SimulationYear = Time + 2020) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

necn_summaries2 <- necn_summaries %>%
  group_by(run_name, SimulationYear) %>%
  summarise(across(where(is.numeric), ~weighted.mean(.x, NumSites)),
            browse = browse[1],
            climate = climate[1])


#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------

#AGB over time

theme_set(theme_bw())

agb_over_time <- ggplot(data = necn_summaries2,
                        mapping = aes(x = SimulationYear, y = AGB, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Average aboveground biomass",
       subtitle = "by browse and climate",
       y = "Average AGB (Mg/ha)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank())
plot(agb_over_time)
#This actually save the plot in a image
ggsave(file="./docs/images/agb_nobrowse.svg", plot=agb_over_time, width=5, height=4)


#TOtal C
totalc_over_time <- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = TotalC/1000, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Average ecosystem carbon density",
       subtitle = "Present climate vs RCP8.5 scenario",
       y = "Average Total C (Mg/ha)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank())
plot(totalc_over_time)
ggsave(file="totalc.svg", plot=totalc_over_time, width=5, height=4)

#SOM over time

somtc_over_time <- ggplot(data = necn_summaries2, 
                          mapping = aes(x = SimulationYear, y = TotalSOMTC, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Average soil organic matter carbon stocks",
       subtitle = "by browse and climate",
       y = "Average SOM C (Mg/ha)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank())
plot(somtc_over_time)
ggsave(file="somtc.svg", plot=somtc_over_time, width=5, height=4)

#SoilN over time
n_over_time <- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = TotalSoilN, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "mineral n",
       subtitle = "by browse and climate scenario",
       y = "N", x = "Timestep") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank())
plot(n_over_time)

#AG NPP over time
npp_over_time <- ggplot(data = necn_summaries2, 
                        mapping = aes(x = SimulationYear, y = AG_NPPC + BG_NPPC, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Average aboveground net primary productivity",
       subtitle = "by browse and climate scenario",
       y = "Average NPP (g/m2/yr)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank())
plot(npp_over_time)
ggsave(file="npp.svg", plot=npp_over_time, width=5, height=4)


#C surf over time
surface_c_over_time <- ggplot(data = necn_summaries2, 
                              mapping = aes(x = SimulationYear, y = TotalCSurf, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Soil surface C",
       subtitle = "by browse and climate scenario",
       y = "Average surface C (Mg/ha)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank())
plot(surface_c_over_time)
ggsave(file="surfc.svg", plot=surface_c_over_time, width=5, height=4)


#NEE over time
nee_over_time <- ggplot(data = necn_summaries2, 
                              mapping = aes(x = SimulationYear, y = TotalNEE, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Net ecosystem exchange",
       subtitle = "by browse and climate scenario",
       y = "Average net ecosystem exchange (Mg/ha/yr)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0)
plot(nee_over_time)
ggsave(file="nee.svg", plot=nee_over_time, width=5, height=4)




c_inputs_over_time <- ggplot(data = necn_summaries2, 
                        mapping = aes(x = SimulationYear, y = Litterfall + AgeMortality, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Detrital inputs",
       subtitle = "by browse and climate scenario",
       y = "Detrital inputs (g m-2 yr-1)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0)
plot(c_inputs_over_time)

c_inputs_over_time <- ggplot(data = necn_summaries2, 
                             mapping = aes(x = SimulationYear, y = , colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Detrital inputs",
       subtitle = "by browse and climate scenario",
       y = "Detrital inputs (g m-2 yr-1)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0)

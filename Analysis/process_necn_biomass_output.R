# Wrangle the NECN biomass tables

# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

library(tidyverse)

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
                            climate = character(length(scenarios)))

scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  mutate(browse = ifelse(grepl(pattern = "no browse", run_name), "No Browse", "Browse")) %>%
  mutate(climate = ifelse(grepl(pattern = "historical", run_name), "Historical", "MIROC"))

# scenario_type$fire_model <- rep(c("fixed", "mixed"), each = 3)

necn_summaries <- paste0(scenarios, "/NECN-succession-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

necn_summaries2 <- necn_summaries %>%
  group_by(run_name, Time) %>%
  summarise(TotalAGB = weighted.mean(AGB, NumSites),
            TotalSOMTC =  weighted.mean(SOMTC, NumSites),
            browse = browse[1],
            climate = climate[1])




# #---------------------
# #do it manually if needed
# scenarios <- c("./Analysis/Test/scen1/scrapple-summary-log.csv",
#                "./Analysis/Test/scen1/scrapple-summary-log (1).csv",
#                "./Analysis/Test/scen1/scrapple-summary-log (2).csv",
#                "./Analysis/Test/scen1/scrapple-summary-log (3).csv",
#                "./Analysis/Test/scen1/scrapple-summary-log (4).csv",
#                "./Analysis/Test/scen6/scrapple-summary-log.csv",
#                "./Analysis/Test/scen6/scrapple-summary-log (1).csv",
#                "./Analysis/Test/scen6/scrapple-summary-log (2).csv",
#                "./Analysis/Test/scen6/scrapple-summary-log (3).csv",
#                "./Analysis/Test/scen6/scrapple-summary-log (4).csv",
#                "./Analysis/Test/scen1miroc/scrapple-summary-log.csv",
#                "./Analysis/Test/scen1miroc/scrapple-summary-log (1).csv",
#                "./Analysis/Test/scen1miroc/scrapple-summary-log (2).csv",
#                "./Analysis/Test/scen1miroc/scrapple-summary-log (3).csv",
#                "./Analysis/Test/scen1miroc/scrapple-summary-log.csv",
#                "./Analysis/Test/scen6miroc/scrapple-summary-log.csv",
#                "./Analysis/Test/scen6miroc/scrapple-summary-log (1).csv",
#                "./Analysis/Test/scen6miroc/scrapple-summary-log (2).csv",
#                "./Analysis/Test/scen6miroc/scrapple-summary-log (3).csv",
#                "./Analysis/Test/scen6miroc/scrapple-summary-log (4).csv")
# 
# fire_summaries <- scenarios %>%
#   purrr::map_df(~read_plus(.))
# 
# scenario_type <- data.frame(filename = scenarios,
#                             mgmt = rep(c(1,1,1,1,1,6,6,6,6,6), times = 2),
#                             climate = rep(c("historical", "miroc"), each = 10))

#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------

#AGB over time

ggplot(data = necn_summaries2, mapping = aes(x = Time, y = TotalAGB)) + 
  geom_point(color="steelblue") + 
  labs(title = "Aboveground biomass",
       subtitle = "by management scenario and climate scenario",
       y = "Average AGB (g m-2)", x = "Timestep") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ browse + climate, nrow = 3, ncol = 2)


#SOM over time

ggplot(data = necn_summaries2, mapping = aes(x = Time, y = TotalSOMTC)) + 
  geom_point(color="steelblue") + 
  labs(title = "Soil total organic matter stocks",
       subtitle = "by management scenario and climate scenario",
       y = "Soil C (g m-2)", x = "Timestep") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ browse + climate, nrow = 3, ncol = 2)






# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

library(tidyverse)
library(lemon)
library("cowplot")
source("./Analysis/r_functions.R")

#what folder do all the runs to be analyzed live in?
scenario_folder <- "E:/ISRO LANDIS/Model runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/"
# scenario_folder <- "./Models/Model templates"
# scenario_folder <- "./Models/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE)
# scenarios <- scenarios[c(5:7)]

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "d", ClimateRegionName = "c")) %>% 
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

clim_summaries <- paste0(scenarios, "/Climate-annual-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  mutate(season_length = EndGrow - BeginGrow) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  mutate(Time = ifelse(climate == "Present Climate", Time + 41, Time))

clim_melt <- tidyr::pivot_longer(clim_summaries, cols = c("TAP", "MAT", "season_length"))

#----------------------------------------------
#multipanel figure

annual_clim <- ggplot(data = clim_melt, mapping = aes(x = Time, y = value)) + 
  geom_point(aes(colour = climate), alpha = 0.2) + 
  labs(y = expression(paste("Proportion of forage eaten (Browse Density Index)")), x = "Simulation Year") +
  geom_smooth(aes( color = climate))  + 
  facet_wrap(facets = ~name) + 
  guides(colour=guide_legend(title="Predation"),
         linetype = guide_legend(title="")) + 
  scale_fill_discrete(labels = c(""))
plot(annual_clim)


p1 <- ggplot(clim_summaries, aes(x = Time, y = MAT))+
  geom_point(aes(colour = climate), alpha = 0.2) + 
  geom_smooth(aes(colour = climate)) +
  ylab(expression(paste("Mean Annual Temperature (C)"))) +
  theme(plot.margin = margin(6, 6, 6, 6),
        axis.title.x=element_blank(),
        legend.position="none") 
plot(p1)

p2 <- ggplot(clim_summaries, aes(x = Time, y = TAP * 10))+
  geom_point(aes(colour = climate), alpha = 0.2) + 
  geom_smooth(aes(colour = climate)) +
  ylab(expression(paste("Total annual precipitation (mm)"))) +
  theme(plot.margin = margin(6, 6, 6, 6),
        axis.title.x=element_blank(),
        legend.position="none") 
plot(p2)

p3 <- ggplot(clim_summaries, aes(x = Time, y = season_length))+
  geom_point(aes(colour = climate), alpha = 0.2) + 
  geom_smooth(aes(colour = climate)) +
  ylab(expression(paste("Growing season length (days)"))) +
  theme(plot.margin = margin(6, 6, 6, 6),
        axis.title.x=element_blank(),
        legend.position="none") 
plot(p3)


c_grid <- cowplot::plot_grid(p1, p2, p3, 
                             align = "h", 
                             nrow = 1, ncol = 3,
                             labels = "auto",
                             rel_widths = c(1,1,1))

legend <- cowplot::get_legend(
  p1 + 
    theme(legend.position = "right") + 
    labs(colour = "Climate scenario")
)

c_grid_with_legend <- cowplot::plot_grid(c_grid, legend, ncol = 2, rel_widths = c(3, 1))

climate_figure <- ggdraw(add_sub(c_grid_with_legend, "Simulation year", 
               vpadding=grid::unit(0.7,"lines"),
               y=0.2, x=0.4, vjust=0, size = 12))
plot(climate_figure)


ggsave(file="./Analysis/plots/climate_timeseries.svg", plot=climate_figure, width=10, height=5)

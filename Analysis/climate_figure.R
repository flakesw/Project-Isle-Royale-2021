
# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

library(tidyverse)
library(lemon)
library("cowplot")

shift_legend2 <- function(p) {
  # check if p is a valid object
  if(!(inherits(p, "gtable"))){
    if(inherits(p, "ggplot")){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]), 
                               USE.NAMES = F)
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  
  # return repositioned legend
  reposition_legend(p, 'center', panel=names, plot = FALSE)
}

tag_facet <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  #from package egg
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE) 
}


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

ggdraw(add_sub(c_grid_with_legend, "Simulation year", 
               vpadding=grid::unit(0.7,"lines"),
               y=0.2, x=0.4, vjust=0, size = 12))

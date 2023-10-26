# Wrangle the NECN biomass tables

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

necn_monthly <- paste0(scenarios, "/NECN-succession-monthly-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  mutate(SimulationYear = Time + 2020) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  group_by(run_name, SimulationYear) %>%
  summarise(NPP = sum(AvgTotalNPP_C),
            Rh = sum(AvgHeteroRespiration),
            MAT = mean(AirTemp),
            browse = browse[1],
            climate = climate[1])


#-------------------------------------------------------------------------------
# Compare effects
#-------------------------------------------------------------------------------

test <- necn_summaries2 %>%
  filter(SimulationYear == 2095) 
test$browse <- factor(test$browse, levels = c("Low", "Medium", "High"))
test$climate <- factor(test$climate, levels = unique(test$climate)[c(3,2,1,4,5)])

mod <- lm(TotalC ~ climate*browse, data = test)
anova(mod)
summary(mod)
boxplot(TotalC ~ browse*climate, data = test)

mod2 <- lm(I(AGB*0.47) ~ climate*browse, data = test)
anova(mod2)
summary(mod2)
boxplot(AGB ~ browse*climate, data = test)

mod3 <- lm(SOMTC ~ browse*climate, data = test)
anova(mod3)
summary(mod3)
boxplot(SOMTC ~ browse*climate, data = test)

p1 <- ggplot(test, aes(x = climate, y = TotalC/100))+
  geom_boxplot(aes(fill = browse), linewidth = 0.25, outlier.size = 1) + 
  scale_x_discrete(labels = c("Present Climate", "Warm \n(CCSM4 4.5)",
                              "Hot/Dry \n(CanESM2 8.5)", "Very Hot \n(MIROC-ESM-CHEM 8.5)",
                              "Hot/Wet \n(MRI-CGM3 8.5)")) +
  xlab("") +
  ylab(expression(atop("Ecosystem C", paste("(Mg ha"^{-1}, ")")))) +
  theme(plot.margin = margin(6, 0, 6, 0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position="none") 
p2 <- ggplot(test, aes(x = climate, y = AGB/100*0.47))+
  geom_boxplot(aes(fill = browse), linewidth = 0.25, outlier.size = 1)+ 
  scale_x_discrete(labels = c("Present Climate", "Warm \n(CCSM4 4.5)",
                              "Hot/Dry \n(CanESM2 8.5)", "Very Hot \n(MIROC-ESM-CHEM 8.5)",
                              "Hot/Wet \n(MRI-CGM3 8.5)")) +
  xlab("")+
  ylab(expression(atop("Aboveground", paste("Biomass C (Mg ha"^{-1}, ")")))) +
  theme(plot.margin = margin(6, 0, 6, 0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position="none") 
p3 <- ggplot(test, aes(x = climate, y = SOMTC/100)) +
  geom_boxplot(aes(fill = browse), linewidth = 0.25, outlier.size = 1)+ 
  scale_x_discrete(labels = c("Present Climate", "Warm \n(CCSM4 4.5)",
                              "Hot/Dry \n(CanESM2 8.5)", "Very Hot \n(MIROC-ESM-CHEM 8.5)",
                              "Hot/Wet \n(MRI-CGM3 8.5)")) +
  xlab("")+
  ylab(expression(atop("Soil organic C", paste("(Mg ha"^{-1}, ")")))) +
  theme(plot.margin = margin(6, 0, 6, 0)) +
  theme(legend.position="none")


c_grid <- cowplot::plot_grid(p1, p2, p3, 
                   align = "v", 
                   nrow = 3, ncol = 1,
                   labels = "auto",
                   rel_heights = c(1,1,1.3))

legend <- cowplot::get_legend(
  # create some space to the left of the legend
  p1 + 
    theme(legend.position = "right") + 
    labs(fill = "Predation")
)

boxplots_combined <- cowplot::plot_grid(c_grid, legend, ncol = 2, rel_widths = c(3, 0.6))
plot(boxplots_combined)
ggsave(file="./Analysis/plots/boxplots_c_density.svg", plot=boxplots_combined, width=7, height=5)


#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------

#AGB over time

theme_set(theme_bw())
theme_update(panel.grid.minor = element_blank(),
             strip.background = element_rect(fill = "white"))

agb_over_time <- ggplot(data = necn_summaries2,
                        mapping = aes(x = SimulationYear, y = AGB/100*0.47, colour = browse)) + 
  geom_point(alpha = 0.2) + 
  # geom_line(aes(group = run_name)) + 
  labs(y = expression(paste("Average aboveground biomass C density (Mg ha"^{-1}, ")")), 
       x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate")+ 
  guides(colour=guide_legend(title="Predation"))
agb_over_time <- tag_facet(agb_over_time)
agb_over_time <- shift_legend2(agb_over_time)
plot(agb_over_time)
#This actually save the plot in a image
ggsave(file="./Analysis/plots/agb_over_time.svg", plot=agb_over_time, width=5, height=4)


#TOtal C
totalc_over_time <- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = TotalC/100, 
                                                                 colour = browse)) + 
  geom_point(alpha = 0.25) + 
  labs(y = expression(paste("Ecosystem C density (Mg ha" ^{-1}, ")")),
       x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") +
  guides(colour=guide_legend(title="Predation"))
totalc_over_time <- tag_facet(totalc_over_time)
totalc_over_time <- shift_legend2(totalc_over_time)
plot(totalc_over_time)
ggsave(file="./Analysis/plots/totalc_over_time.svg", plot=totalc_over_time, width=5, height=4)

#SOM over time

somtc_over_time <- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = SOMTC/100, 
                                                                colour = browse)) + 
  geom_point(alpha = 0.25) + 
  labs(y = expression(paste("Soil organic C density  (Mg ha" ^{-1}, ")")),
       x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
somtc_over_time <- tag_facet(somtc_over_time)
somtc_over_time <- shift_legend2(somtc_over_time)
plot(somtc_over_time)
ggsave(file="./Analysis/somtc_over_time.svg", plot=somtc_over_time, width=5, height=4)

#SoilN over time
n_over_time <- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = MineralN, 
                                                            colour = browse)) + 
  geom_point() + 
  labs(y = expression(paste("Mineral N (g m"^{-2}, ")")), x = "Timestep") + 
  geom_smooth()  + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
n_over_time <- tag_facet(n_over_time)
n_over_time <- shift_legend2(n_over_time)
plot(n_over_time)
ggsave(file="./Analysis/plots/mineral_n_over_time.svg", plot=mineral_n_over_time, width=5, height=4)

#NPP over time
npp_over_time <- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = AG_NPPC + BG_NPPC, 
                                                                   colour = browse)) + 
  geom_point(alpha = 0.25) + 
  labs(y = expression(paste("Net primary productivity (g m" ^{-2}, ")")),
       x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
npp_over_time <- tag_facet(npp_over_time)
npp_over_time <- shift_legend2(npp_over_time)
plot(npp_over_time)
ggsave(file="npp.svg", plot=npp_over_time, width=5, height=4)


#C surf over time
surface_c_over_time <- ggplot(data = necn_summaries2, 
                              mapping = aes(x = SimulationYear, y = C_SOM1surf, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Soil surface C",
       subtitle = "by browse and climate scenario",
       y = "Average surface C (Mg/ha)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
surface_c_over_time <- tag_facet(surface_c_over_time)
shift_legend2(surface_c_over_time)
plot(surface_c_over_time)
ggsave(file="surfc.svg", plot=surface_c_over_time, width=5, height=4)

#C soil over time
som1soil_c_over_time <- ggplot(data = necn_summaries2, 
                              mapping = aes(x = SimulationYear, y = C_SOM1soil, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Soil surface C",
       subtitle = "by browse and climate scenario",
       y = "Average surface C (Mg/ha)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
som1soil_c_over_time <- tag_facet(som1soil_c_over_time)
shift_legend2(som1soil_c_over_time)
plot(som1soil_c_over_time)
ggsave(file="surfc.svg", plot=surface_c_over_time, width=5, height=4)

#C SOM2 over time
som2_c_over_time <- ggplot(data = necn_summaries2, 
                              mapping = aes(x = SimulationYear, y = C_SOM2, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Soil surface C",
       subtitle = "by browse and climate scenario",
       y = "Average surface C (Mg/ha)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
som2_c_over_time <- tag_facet(som2_c_over_time)
shift_legend2(som2_c_over_time)
plot(som2_c_over_time)
ggsave(file="surfc.svg", plot=surface_c_over_time, width=5, height=4)

#C SOM3 over time
som3_c_over_time <- ggplot(data = necn_summaries2, 
                           mapping = aes(x = SimulationYear, y = C_SOM3, colour = browse, shape = climate, linetype = climate)) + 
  geom_point() + 
  labs(title = "Soil surface C",
       subtitle = "by browse and climate scenario",
       y = "Average surface C (Mg/ha)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
som3_c_over_time <- tag_facet(som3_c_over_time)
shift_legend2(som3_c_over_time)
plot(som3_c_over_time)
ggsave(file="surfc.svg", plot=surface_c_over_time, width=5, height=4)


n_loss_over_time <- ggplot(data = necn_summaries2, 
                             mapping = aes(x = SimulationYear, y = LeachedN + Nvol, color = browse)) + 
  geom_point(alpha = 0.25) +
  labs(title = "N loss (leaching and volatilization)",
       subtitle = "by browse and climate scenario",
       y = "N loss (g m-2 yr-1)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
plot(n_loss_over_time)

#NEE over time
nee_over_time<- ggplot(data = necn_summaries2, mapping = aes(x = SimulationYear, y = NEEC, 
                                                                            colour = browse)) + 
  geom_point(alpha = 0.25) + 
  labs(y = expression(paste("Net ecosytem exchange (g m" ^{-2}, ")")),
       x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation")) +
  geom_hline(yintercept = 0)
nee_over_time <- tag_facet(nee_over_time)
nee_over_time <-shift_legend2(nee_over_time)
plot(nee_over_time)
ggsave(file="nee.svg", plot=npp_over_time, width=5, height=4)


#how does browsing affect NEE?
mean(necn_summaries2[necn_summaries2$climate == "Present Climate" & necn_summaries2$browse == "High pred" & necn_summaries2$SimulationYear == "2030", ]$NEEC)
mean(necn_summaries2[necn_summaries2$climate == "Present Climate" & necn_summaries2$browse == "Low pred"& necn_summaries2$SimulationYear == "2030", ]$NEEC)
#approximately 40 g m-2 yr difference, at the beginning; then they get closer later on


c_inputs_over_time <- ggplot(data = necn_summaries2, 
                        mapping = aes(x = SimulationYear, 
                                      y = Litterfall + AgeMortality, 
                                      colour = browse)) + 
  geom_point() + 
  labs(title = "Detrital inputs",
       subtitle = "by browse and climate scenario",
       y = "Detrital inputs (g m-2 yr-1)", x = "Simulation Year") + 
  geom_smooth() + 
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation")) +
  geom_hline(yintercept = 0)
c_inputs_over_time <- tag_facet(c_inputs_over_time)
c_inputs_over_time <-shift_legend2(c_inputs_over_time)
plot(c_inputs_over_time)


#_--------------------

necn_monthly2 <- necn_monthly %>%
  pivot_longer(cols = c("NPP", "Rh", "MAT"))
npp_rh_over_time <- ggplot(data = necn_monthly2 %>% filter(name %in% c("NPP", "Rh")), 
                             mapping = aes(x = SimulationYear, y = value, colour =browse, linetype = name)) + 
  # geom_point(aes(y = Rh)) +
  labs(y = "C flux (g m-2 yr-1)", x = "Simulation Year") + 
  geom_smooth() +
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"),  
         linetype=guide_legend(title=""))
npp_rh_over_time <- tag_facet(npp_rh_over_time)
npp_rh_over_time <- shift_legend2(npp_rh_over_time)
plot(npp_rh_over_time)
ggsave(file="npp_rh.svg", plot=npp_rh_over_time, width=5, height=4)

npp_over_time <- ggplot(data = necn_monthly,
       mapping = aes(x = SimulationYear, y = NPP, color = browse)) + 
  # geom_point(aes(y = Rh)) +
  labs(y = expression(paste("Net primary productivity (g m"^{-2}," yr"^{-1}, ")")),
       x = "Simulation Year") + 
  geom_smooth() +
  theme(panel.grid.minor = element_blank()) + 
  facet_wrap(facets = "climate") + 
  guides(colour=guide_legend(title="Predation"))
npp_over_time <- tag_facet(npp_over_time)
npp_over_time <- shift_legend2(npp_over_time)
plot(npp_over_time)

ggplot(data = necn_monthly,
       mapping = aes(x = MAT, y = Rh, color = browse)) +
  geom_point() +
  geom_smooth()

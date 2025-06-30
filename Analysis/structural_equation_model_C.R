#structural equation modeling

library("lavaan")
library("semPlot")
library("sem")
remotes::install_github("maksimrudnev/LittleHelpers@3c7d669e74ec22ecaa4b56725ffc66dc759ecac4")
library("LittleHelpers")


# Wrangle the NECN biomass tables

# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

library(tidyverse)
library(lemon)
library("cowplot")
library("multcompView")
source("./Analysis/r_functions.R")

#what folder do all the runs to be analyzed live in?
scenario_folder <- "E:/ISRO LANDIS/Model runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/"
# scenario_folder <- "./Models/V2 Model templates"
# scenario_folder <- "./Models/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE)
# scenarios <- scenarios[c(5:7)]

scenarios <- scenarios %>%
  `[`(grep("ccsm|mri", .)) %>%
  `[`(grep("pred1|pred3", .))

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "d", ClimateRegionName = "c")) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm)))) 
  
}

scenario_type <- data.frame(run_name = character(length(scenarios)), 
                            browse = character(length(scenarios)),
                            climate = character(length(scenarios)))

scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  # mutate(browse = ifelse(grepl(pattern = "pred1", run_name), "Low", 
  #                 ifelse(grepl(pattern = "pred2", run_name), "Medium",
  #                 "High"))) %>%
  mutate(browse = ifelse(grepl(pattern = "pred1", run_name), "High", 
                         ifelse(grepl(pattern = "pred2", run_name), "Medium",
                                "Low"))) %>%
  # mutate(browse = c("Low pred", "Hi pred", "Low pred", "Hi pred")) %>%
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "Very Hot (MIROC-ESM-CHEM 8.5)", 
                          ifelse(grepl(pattern = "canesm", run_name), "Hot/Dry (CanESM2 8.5)",
                                 ifelse(grepl(pattern = "ccsm", run_name), "Warm (CCSM4 4.5)", 
                                        ifelse(grepl(pattern = "mri_cgm", run_name), "Hot/Wet (MRI-CGCM3 8.5)", "Present Climate"))))) %>%
  # mutate(browse = factor(browse, levels = c("Low", "Medium", "High")),
  #        climate = factor(climate, levels = unique(climate)[c(3,2,1,4,5)]))
  mutate(browse = factor(browse, levels = c("Low", "High")),
         climate = factor(climate, levels = unique(climate)[c(1,2)]))

necn_summaries <- paste0(scenarios, "/NECN-succession-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  mutate(TotalC = SOMTC + C_LiveLeaf + C_LiveFRoot + C_LiveWood + C_LiveCRoot + C_DeadWood +
           C_DeadCRoot + C_DeadLeaf_Struc + C_DeadLeaf_Meta + C_DeadFRoot_Struc + C_DeadFRoot_Meta,
         TotalNetMineralization = SurfStrucNetMin + SurfMetaNetMin + SoilStrucNetMin + SoilMetaNetMin +
           SOM1surfNetMin + SOM1soilNetMin + SOM2NetMin + SOM3NetMin,
         SimulationYear = Time + 2020) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

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

sem_data <- necn_summaries

sem_data <- sem_data %>%
  mutate(across(where(is.numeric), .fns = function(x) scale(x)))



# sem_data$FG <- as.numeric(as.factor(sem_data$FG)) - 1

leaf_model <- ' 

            AGB ~ a*AG_NPPC
            SOMTC ~ b*Litterfall + c*
            
            #SLA total effect
            SLA_total := c + (a*b)
            SLA_indirect := a*b
            
            #S3 total effect
            S3_total := e + (d*b)
            S3_indirect := d*b
            
            #C3H total effect
            C3H_total := g + (f*b)
            C3H_indirect := f*b
            
            '

fit <- lavaan::sem(leaf_model, data = sem_data, estimator = "ML", fixed.x=FALSE)
semPaths(object = fit, what = "std", layout = "tree", residuals = FALSE, nCharNodes = 10, 
         sizeMan = 12, sizeLat = 7, label.cex = 1.2, edge.label.cex = 1.2)
summary(fit, fit.measures = TRUE)
fitMeasures(fit)

resid(fit)
LittleHelpers::lav_to_graph(fit, file = "Fig 7 leaf_sem.svg")
#ordination
library(tidyverse)
library(lemon)
library(cowplot)
library(vegan)
library("BiodiversityR")

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

read_comm_mats <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm))),
           timestep = str_extract(flnm, "[0-9]+(?=\\.)"))
  
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

# scenario_type <- scenario_type %>%
# mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
# mutate(browse = ifelse(grepl(pattern = "no browse", run_name), "No Browse", "Browse")) %>%
# mutate(browse = c("Low pred", "Hi pred", "Low pred", "Hi pred")) %>%
# mutate(climate = ifelse(grepl(pattern = "historical", run_name), "Present Climate", "RCP8.5"))
# mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "RCP8.5", "Present Climate"))

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


comm_mat_orig <- read.csv(paste0(scenarios, "/community-input-file-0.csv")[49]) %>%
  group_by(MapCode, SpeciesName) %>%
  summarize(Biomass = sum(CohortBiomass), .groups = "drop")

spp_to_use <- comm_mat_orig %>%
  group_by(SpeciesName) %>%
  summarize(biomass = sum(Biomass)) %>%
  slice_max(order_by = biomass, n = 15)

comm_mat_orig <- comm_mat_orig %>%
  filter(SpeciesName %in% spp_to_use$SpeciesName) %>%
  pivot_wider(names_from = SpeciesName, values_from = Biomass, values_fill = 0) %>%
  slice_sample(n = 200) %>%
  arrange(MapCode)

comm_mat2 <- read.csv(paste0(scenarios, "/community-input-file-80.csv")[49]) %>%
  group_by(MapCode, SpeciesName) %>%
  summarize(Biomass = sum(CohortBiomass), .groups = "drop") %>%
  filter(SpeciesName %in% spp_to_use$SpeciesName) %>%
  pivot_wider(names_from = SpeciesName, values_from = Biomass, values_fill = 0) %>%
  filter(MapCode %in% comm_mat_orig$MapCode) %>%#match map codes to original
  arrange(MapCode)

combined <- bind_rows(comm_mat_orig %>% filter(MapCode %in% comm_mat2$MapCode), 
                      comm_mat2)


#combine data from multiple runs for PRC
#which model runs
mods <- c(13, 31, 49, 67, 85)
timesteps <- c(0, 80)

files_list <- paste0(scenarios[mods], "/community-input-file-", rep(timesteps, each = 5), ".csv")

prc_matrix <- files_list %>%
  purrr::map_df(~read_comm_mats(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))
spp_to_use <- prc_matrix %>%
  group_by(SpeciesName) %>%
  summarize(biomass = sum(CohortBiomass)) %>%
  slice_max(order_by = biomass, n = 15)
prc_matrix2 <- prc_matrix %>%
  filter(SpeciesName %in% spp_to_use$SpeciesName) %>%
  group_by(MapCode, SpeciesName, filename) %>%
  summarize(Biomass = sum(CohortBiomass),
            Time = timestep[1],
            browse = browse[1],
            climate = climate[1], .groups = "drop") %>%
  pivot_wider(names_from = SpeciesName, values_from = Biomass, values_fill = 0) 

prc_matrix3 <- prc_matrix2 %>%
  group_by(MapCode) %>%
  filter(n() == length(files_list)) %>%
  ungroup() %>%
  filter(MapCode %in% sample(unique(MapCode), 100))
  
covariates <- prc_matrix3[, c(1:5)]
prc_sp_mat <- prc_matrix3[, -c(1:5)]

#----------
# for young cohorts

prc_matrix2_young <- prc_matrix %>%
  filter(SpeciesName %in% spp_to_use$SpeciesName,
         CohortAge <= 20) %>%
  group_by(MapCode, SpeciesName, filename) %>%
  summarize(Biomass = sum(CohortBiomass),
            Time = timestep[1],
            browse = browse[1],
            climate = climate[1], .groups = "drop") %>%
  pivot_wider(names_from = SpeciesName, values_from = Biomass, values_fill = 0) 

prc_matrix3_young <- prc_matrix2_young %>%
  tidyr::complete(MapCode, Time, climate) %>%
  # group_by(MapCode) %>%
  # filter(n() == length(files_list)) %>%
  # ungroup() %>%
  filter(MapCode %in% sample(unique(MapCode), 100)) 

covariates_young <- prc_matrix3_young[, c(1:5)]
prc_sp_mat_young <- prc_matrix3_young[, -c(1:5)]%>%
  replace(is.na(.), 0)

#---------------------
#ordination exploration

nmds <- vegan::rda(comm_mat_orig[, -1], scale = TRUE)
nmds
scores(nmds)
biplot(nmds,
       choices = c(1,2),
       display = c("sites", 
                   "species"),
       type = c("text",
                "points"))

cmat <- as.data.frame(comm_mat_orig[, -1])
cmat2 <- as.data.frame(comm_mat2[, -1])
dist <- vegdist(cmat, method = "jaccard")
pcoa <- cmdscale(dist,
                         k = 5, 
                         eig=TRUE, 
                         add=FALSE)
pcoa_orig <- add.spec.scores(ordi = pcoa, 
                        comm = cmat, 
                        method='pcoa.scores', 
                        Rscale=TRUE, 
                        scaling=1, 
                        multi=1)

ordiplot(pcoa_orig, type = "text")
ordiplot(pcoa_orig, choice = c(1,3), type="text")
                                                    


nmds <- vegan::metaMDS(comm_mat_orig[, -1],
                       trymax = 100,
                       k = 4)
plot(nmds)
ordiplot(nmds, type = "none")
# points(nmds, "sites")
text(nmds, "species")

ordiplot(nmds, type = "none", choice = c(1,3))
# points(nmds, "sites")
text(nmds, "species", choice = c(1,3))

ordiplot(nmds, type = "none", choice = c(3,4))
# points(nmds, "sites")
text(nmds, "species", choice = c(3,4))




library(vegan)
data(dune)
distmatrix <- vegdist(dune, method="euc")
# Principal coordinates analysis with 19 axes to estimate total variance
Ordination.model1 <- cmdscale (distmatrix, k=19, eig=TRUE, add=FALSE)
# Change scores for second axis
Ordination.model1$points[,2] <- -1.0 * Ordination.model1$points[,2]
Ordination.model1 <- add.spec.scores(Ordination.model1, dune, 
                                     method='pcoa.scores', Rscale=TRUE, scaling=1, multi=1)
# Compare Ordination.model1 with PCA
Ordination.model2 <- rda(dune, scale=FALSE)
#
par(mfrow=c(1,2))
ordiplot(Ordination.model1, type="text")
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
plot(Ordination.model2, type="text", scaling=1)




#------------------------
# principal response curves

veg_prc <- prc_sp_mat
times <- as.factor(covariates$Time)
clim = as.factor(covariates$climate)

test <- vegan::prc(veg_prc, clim, time = times)
test.summ <- summary(test, axis = 2)

plot(test, select = abs(test.summ$sp) > 0.5, scaling = 1, xlab = "Year")

plot(test, select = abs(test.summ$sp) > 0.5, scaling = 2, xlab = "Year", legpos = NA, ylim = c(-0.5,2), cex.axis = 1.5, cex.lab = 1.5, lwd = 4, const = 0.5, cex = 1.5)

plot(test, axis = 2, select = abs(test.summ$sp) > 0.5, scaling = 0)



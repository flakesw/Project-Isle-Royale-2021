#look at regeneration patterns
library("terra")
library("tidyverse")

comm_map <- rast("./Models/Model runs/miroc - pred1/output-community-0.img")
comm_output_end <- read_csv("./Models/Model runs/miroc - pred1/community-input-file-80.csv")
comm_output_begin<- read_csv("./Models/Model runs/miroc - pred1/community-input-file-0.csv")

comm_nest_begin <- comm_output_begin %>%
  nest(comm = c(SpeciesName, CohortAge, CohortBiomass))
comm_nest_end <- comm_output_end %>%
  nest(comm = c(SpeciesName, CohortAge, CohortBiomass))

mapcode_list <- values(comm_map)

spp_list <- unique(comm_output_begin$SpeciesName)

mean_ages_begin <- comm_output_begin %>%
  group_by(MapCode, SpeciesName) %>%
  summarise(mean_age = weighted.mean(CohortAge, CohortBiomass),
            SaplingBiomass = sum(CohortBiomass[CohortAge <= 10])) %>%
  ungroup() %>%
  complete(MapCode, SpeciesName, fill = list(mean_age = 0, SaplingBiomass = 0))

mean_ages_end <- comm_output_end %>%
  group_by(MapCode, SpeciesName) %>%
  summarise(mean_age = weighted.mean(CohortAge, CohortBiomass),
            SaplingBiomass = sum(CohortBiomass[CohortAge <= 10])) %>%
  ungroup() %>%
  complete(MapCode, SpeciesName, fill = list(mean_age = 0, SaplingBiomass = 0))


mean_ages_combined <- left_join(mean_ages_begin, mean_ages_end, by = c("MapCode", "SpeciesName"))
mean_ages_combined

mean_ages_combined$diff <- mean_ages_combined$mean_age.y - mean_ages_combined$mean_age.x

#slow way:
# age_change_map <- comm_map

# age_change_map <- terra::classify(age_change_map, 
#                                   rcl = mean_ages_combined[mean_ages_combined$SpeciesName == "ABBA", 
#                                                            c("MapCode", "diff")])

# fast way:
age_change_map <- comm_map

mapcode_data <- values(comm_map) %>%
  as.data.frame() %>%
  rename(MapCode = Layer_1) %>%
  left_join(filter(mean_ages_combined, SpeciesName == "POTR5"))
values(age_change_map) <- mapcode_data$diff
plot(age_change_map, col = diverging_color_ramp(age_change_map))

plot(age_change_map_browse - age_change_map)
hist(values(age_change_map_browse - age_change_map)[values(age_change_map_browse - age_change_map) != 0])
mean(values(age_change_map_browse - age_change_map)[values(age_change_map_browse - age_change_map) != 0])


juv_biomass_map_begin <- comm_map
juv_biomass_map_begin <- terra::classify(juv_biomass_map_begin, 
                                   rcl = mean_ages_combined[mean_ages_combined$SpeciesName == "POTR5", 
                                                            c("MapCode", "SaplingBiomass.x")])
plot(juv_biomass_map_begin)

juv_biomass_map_end <- comm_map
juv_biomass_map_end <- terra::classify(juv_biomass_map_end, 
                                         rcl = mean_ages_combined[mean_ages_combined$SpeciesName == "POTR5", 
                                                                  c("MapCode", "SaplingBiomass.y")])
plot(juv_biomass_map_end)
juv_diff <- juv_biomass_map_end - juv_biomass_map_begin
NAflag(juv_diff) <- 0
plot(juv_diff, col = diverging_color_ramp(juv_diff))
mean(values(juv_diff), na.rm = TRUE)

#TODO redo by tracking cohorts

backup <- age_change_map
# age_change_map_browse <- age_change_map

# potr_change_map <- age_change_map
# abba_change_map <- age_change_map
# bepa_change_map <- age_change_map


#-----------------------
# age distribution

plot(density(mean_ages_combined[mean_ages_combined$SpeciesName == "ABBA" & mean_ages_combined$mean_age.x != 0, ]$mean_age.x))
lines(density(mean_ages_combined[mean_ages_combined$SpeciesName == "ABBA" & mean_ages_combined$mean_age.y != 0, ]$mean_age.y))


cc_nobrowse <- read_csv("./Models/landis_test/mc_test - linear/community-input-file-80.csv") %>%
  filter(SpeciesName == "ABBA")
cc_browse <- read_csv("./Models/landis_test/mc_test - linear/community-input-file-80.csv") %>%
  filter(SpeciesName == "ABBA")


#---regen log
regen <- read_csv("./Models/Model runs/current - pred1/NECN-reproduction-log.csv")
abba <- filter(regen, SpeciesName == "ABBA")
regen <- read_csv("./Models/Model runs/miroc - pred3/NECN-reproduction-log.csv")
abba2 <- filter(regen, SpeciesName == "ABBA") %>%
  filter(Time %in% abba$Time)

plot(abba$NumCohortsSeed ~ abba$Time)
plot(abba2$NumCohortsSeed ~ abba2$Time)



#---fia data




#------------Trying to do this as a tibble with nested tibbles


get_mean_age <- function(comm){
  mean_ages <- comm %>%
    group_by(SpeciesName) %>%
    summarise(mean_age = weighted.mean(CohortAge, CohortBiomass))
  return(mean_ages)
}
#no idea why we have to select the column from a one-column tibble
#but otherwise mapping is really annoying
comm_combined <- tibble(mapcode = mapcode_list[, "Layer_1"],
                       comm_data_begin = comm_nest_begin[match(mapcode_list, comm_nest_begin$MapCode), "comm"]$comm,
                       comm_data_end = comm_nest_end[match(mapcode_list, comm_nest_end$MapCode), "comm"]$comm) %>%
  filter(!is.na(mapcode) & mapcode != 0) %>%
  magrittr::set_colnames(c("mapcode",
         "comm_data_begin", 
         "comm_data_end")) 

comm2 <- comm_combined$comm_data_begin
test <- get_mean_age(comm2[[2]])

test <-  map_dfr(head(comm_combined), .x = "comm_data_begin", .f = get_mean_age)


get_mean_age(test$comm_data_begin)

test <- mapcode_combined[15, ] %>% unchop(cols = "mean_age_begin")



sp <- "ABBA"

test <- mapcode_combined %>%
  map(.x = .$mean_age_begin, .f = filter(.$SpeciesName %in% sp))


test <- mapcode_combined[15, ]

test[[3]][[1]]

test[[3]][[1]][[1]] %>%
  group_by(SpeciesName) %>%
  summarise(mean_age = weighted.mean(CohortAge, CohortBiomass))

test[[4]][[1]][[1]] %>%
  group_by(SpeciesName) %>%
  summarise(mean_age = weighted.mean(CohortAge, CohortBiomass))


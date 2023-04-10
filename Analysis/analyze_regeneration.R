#look at regeneration patterns
library("terra")
library("tidyverse")

comm_map <- rast("./Models/landis_test/mc_test - linear/output-community-0.img")
comm_output_end <- read_csv("./Models/landis_test/mc_test - linear/community-input-file-80.csv")
comm_output_begin<- read_csv("./Models/landis_test/mc_test - linear/community-input-file-0.csv")

comm_nest_begin <- comm_output_begin %>%
  nest(comm = c(SpeciesName, CohortAge, CohortBiomass))
comm_nest_end <- comm_output_end %>%
  nest(comm = c(SpeciesName, CohortAge, CohortBiomass))

mapcode_list <- values(comm_map)

spp_list <- unique(comm_output_begin$SpeciesName)

mean_ages_begin <- comm_output_begin %>%
  group_by(MapCode, SpeciesName) %>%
  summarise(mean_age = weighted.mean(CohortAge, CohortBiomass)) %>%
  ungroup() %>%
  complete(MapCode, SpeciesName, fill = list(mean_age = 0))

mean_ages_end <- comm_output_end %>%
  group_by(MapCode, SpeciesName) %>%
  summarise(mean_age = weighted.mean(CohortAge, CohortBiomass))%>%
  ungroup() %>%
  complete(MapCode, SpeciesName, fill = list(mean_age = 0))


mean_ages_combined <- left_join(mean_ages_begin, mean_ages_end, by = c("MapCode", "SpeciesName"))
mean_ages_combined

mean_ages_combined$diff <- mean_ages_combined$mean_age.y - mean_ages_combined$mean_age.x

age_change_map <- comm_map
age_change_map <- terra::classify(age_change_map, 
                                  rcl = mean_ages_combined[mean_ages_combined$SpeciesName == "ABBA", 
                                                           c("MapCode", "diff")])
plot(age_change_map)
plot(age_change_map_browse - age_change_map)
hist(values(age_change_map_browse - age_change_map)[values(age_change_map_browse - age_change_map) != 0])
mean(values(age_change_map_browse - age_change_map)[values(age_change_map_browse - age_change_map) != 0])



backup <- age_change_map
# age_change_map_browse <- age_change_map

# potr_change_map <- age_change_map
# abba_change_map <- age_change_map
# bepa_change_map <- age_change_map


#-----------------------
# age distribution

plot(density(mean_ages_combined[mean_ages_combined$SpeciesName == "ABBA" & mean_ages_combined$mean_age.x != 0, ]$mean_age.x))
plot(density(mean_ages_combined[mean_ages_combined$SpeciesName == "ABBA" & mean_ages_combined$mean_age.y != 0, ]$mean_age.y))

#---regen log
regen <- read_csv("./Models/landis_test/mc_test/NECN-reproduction-log.csv")
abba <- filter(regen, SpeciesName == "BEPA")


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


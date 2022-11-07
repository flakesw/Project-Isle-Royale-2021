library(tidyverse)

in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/young aspen - no browse"

#this aspen cohort should have 

comm_files <- list.files(in_folder, full.names = TRUE) %>%
  `[`(grepl("community-input-file", .)) %>%
  `[`(grepl("csv", .))

read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c", MapCode = "i", SpeciesName = "c",
                                  CohortAge = "i", CohortBiomass = "i")) %>% 
    mutate(filename = as.character(flnm),
           year = as.numeric(stringr::str_extract_all(flnm,"\\(?[0-9]+\\)?")[[1]])) 
  
}

browse_log <- read.csv(paste0(in_folder, "/browse-summary-log.csv"))
necn_log <- read.csv(paste0(in_folder, "/NECN-succession-log.csv"))
merge_log <- left_join(browse_log, necn_log, by = "Time")
merge_log$browse <- "No browse"

cohorts <-  purrr::map_df(comm_files, ~read_plus(.)) 
cohorts$year <- as.numeric(cohorts$year)

cohorts$year_added <- cohorts$year - cohorts$CohortAge

ggplot(cohorts, aes(x = year, y = CohortBiomass)) +
  geom_line(aes(group = year_added, col = year_added)) + 
  facet_wrap(facets = vars(SpeciesName))

ggplot(browse_log, aes(x = Time, y = MeanForage)) + 
  geom_line()
ggplot(necn_log, aes(x = Time, y = AGB)) + 
  geom_line()
ggplot(merge_log, aes(x = Time, y = MeanForage/AGB)) + 
  geom_line()


#----
in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/young aspen - linear"


comm_files <- list.files(in_folder, full.names = TRUE) %>%
  `[`(grepl("community-input-file", .)) %>%
  `[`(grepl("csv", .))

read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c", MapCode = "i", SpeciesName = "c",
                                  CohortAge = "i", CohortBiomass = "i")) %>% 
    mutate(filename = as.character(flnm),
           year = as.numeric(stringr::str_extract_all(flnm,"\\(?[0-9]+\\)?")[[1]])) 
  
}

browse_log2 <- read.csv(paste0(in_folder, "/browse-summary-log.csv"))
necn_log2 <- read.csv(paste0(in_folder, "/NECN-succession-log.csv"))
merge_log2 <- left_join(browse_log2, necn_log2, by = "Time")
merge_log2$browse <- "Linear"

cohorts2 <-  purrr::map_df(comm_files, ~read_plus(.)) 
cohorts2$year <- as.numeric(cohorts2$year)

cohorts2$year_added <- cohorts2$year - cohorts2$CohortAge

ggplot(cohorts2, aes(x = year, y = CohortBiomass)) +
  geom_line(aes(group = year_added, col = year_added)) + 
  facet_wrap(facets = vars(SpeciesName))

ggplot(browse_log2, aes(x = Time, y = MeanForage)) + 
  geom_line()
ggplot(necn_log2, aes(x = Time, y = AGB)) + 
  geom_line()
ggplot(merge_log2, aes(x = Time, y = MeanForage/AGB)) + 
  geom_line()



in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/young aspen - ordered"


comm_files <- list.files(in_folder, full.names = TRUE) %>%
  `[`(grepl("community-input-file", .)) %>%
  `[`(grepl("csv", .))

read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c", MapCode = "i", SpeciesName = "c",
                                  CohortAge = "i", CohortBiomass = "i")) %>% 
    mutate(filename = as.character(flnm),
           year = as.numeric(stringr::str_extract_all(flnm,"\\(?[0-9]+\\)?")[[1]])) 
  
}
browse_log3 <- read.csv(paste0(in_folder, "/browse-summary-log.csv"))
necn_log3 <- read.csv(paste0(in_folder, "/NECN-succession-log.csv"))
merge_log3 <- left_join(browse_log3, necn_log3, by = "Time")
merge_log3$browse <- "Ordered"

cohorts3 <-  purrr::map_df(comm_files, ~read_plus(.)) 
cohorts3$year <- as.numeric(cohorts3$year)
head(cohorts3)

cohorts3$year_added <- cohorts3$year - cohorts3$CohortAge

ggplot(cohorts3, aes(x = year, y = CohortBiomass)) +
  geom_line(aes(group = year_added, col = year_added)) + 
  facet_wrap(facets = vars(SpeciesName))

ggplot(browse_log3, aes(x = Time, y = MeanForage)) + 
  geom_line()
ggplot(necn_log3, aes(x = Time, y = AGB)) + 
  geom_line()
ggplot(merge_log3, aes(x = Time, y = MeanForage/AGB)) + 
  geom_line()

## combine

potr_merge <- rbind(merge_log, merge_log2, merge_log3)

ggplot(potr_merge) + 
  geom_line(aes(x = Time, y = AGB, linetype = browse))+ 
  scale_linetype_manual(values=c("No browse"="solid", "Linear" = "dotted", "Ordered" = "longdash"))

# ggplot() + 
#   geom_line(data = cohorts, aes(x = year, y = CohortBiomass, group = year_added, col = year_added), linetype = "solid") + 
#   geom_line(data = cohorts2, aes(x = year, y = CohortBiomass, group = year_added, col = year_added), linetype = "dotted") + 
#   geom_line(data = cohorts3, aes(x = year, y = CohortBiomass, group = year_added, col = year_added), linetype = "longdash") + 
#   facet_wrap(facets = vars(SpeciesName)) 

ggplot(potr_merge) + 
  geom_line(aes(x = Time, y = MeanForage, linetype = browse))+ 
  scale_linetype_manual(values=c("No browse"="solid", "Linear" = "dotted", "Ordered" = "longdash"))

# ggplot() +
#   geom_line(data =  browse_log, aes(x = Time, y = MeanForage), linetype = "solid") + 
#   geom_line(data =  browse_log2, aes(x = Time, y = MeanForage) , linetype = "dotted") + 
#   geom_line(data =  browse_log3, aes(x = Time, y = MeanForage) , linetype = "longdash") + 
#   scale_linetype_manual(values=c("No browse"="solid", "Linear" = "dotted", "Ordered" = "longdash"))

#################################################################################
# ABBA
#################################################################################

in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/young balsam fir - no browse"


comm_files <- list.files(in_folder, full.names = TRUE) %>%
  `[`(grepl("community-input-file", .)) %>%
  `[`(grepl("csv", .))

read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c", MapCode = "i", SpeciesName = "c",
                                  CohortAge = "i", CohortBiomass = "i")) %>% 
    mutate(filename = as.character(flnm),
           year = as.numeric(stringr::str_extract_all(flnm,"\\(?[0-9]+\\)?")[[1]])) 
  
}

browse_log <- read.csv(paste0(in_folder, "/browse-summary-log.csv"))
necn_log <- read.csv(paste0(in_folder, "/NECN-succession-log.csv"))
merge_log <- left_join(browse_log, necn_log, by = "Time")
merge_log$browse <- "No browse"

cohorts <-  purrr::map_df(comm_files, ~read_plus(.)) 
cohorts$year <- as.numeric(cohorts$year)

cohorts$year_added <- cohorts$year - cohorts$CohortAge

ggplot(cohorts, aes(x = year, y = CohortBiomass)) +
  geom_line(aes(group = year_added, col = year_added)) + 
  facet_wrap(facets = vars(SpeciesName))

ggplot(browse_log, aes(x = Time, y = MeanForage)) + 
  geom_line()
ggplot(necn_log, aes(x = Time, y = AGB)) + 
  geom_line()
ggplot(merge_log, aes(x = Time, y = MeanForage/AGB)) + 
  geom_line()


#----
in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/young balsam fir - linear"


comm_files <- list.files(in_folder, full.names = TRUE) %>%
  `[`(grepl("community-input-file", .)) %>%
  `[`(grepl("csv", .))

read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c", MapCode = "i", SpeciesName = "c",
                                  CohortAge = "i", CohortBiomass = "i")) %>% 
    mutate(filename = as.character(flnm),
           year = as.numeric(stringr::str_extract_all(flnm,"\\(?[0-9]+\\)?")[[1]])) 
  
}

browse_log2 <- read.csv(paste0(in_folder, "/browse-summary-log.csv"))
necn_log2 <- read.csv(paste0(in_folder, "/NECN-succession-log.csv"))
merge_log2 <- left_join(browse_log2, necn_log2, by = "Time")
merge_log2$browse <- "Linear"

cohorts2 <-  purrr::map_df(comm_files, ~read_plus(.)) 
cohorts2$year <- as.numeric(cohorts2$year)

cohorts2$year_added <- cohorts2$year - cohorts2$CohortAge

ggplot(cohorts2, aes(x = year, y = CohortBiomass)) +
  geom_line(aes(group = year_added, col = year_added)) + 
  facet_wrap(facets = vars(SpeciesName))

ggplot(browse_log2, aes(x = Time, y = MeanForage)) + 
  geom_line()
ggplot(necn_log2, aes(x = Time, y = AGB)) + 
  geom_line()
ggplot(merge_log2, aes(x = Time, y = MeanForage/AGB)) + 
  geom_line()



in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/young balsam fir - ordered"


comm_files <- list.files(in_folder, full.names = TRUE) %>%
  `[`(grepl("community-input-file", .)) %>%
  `[`(grepl("csv", .))

read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c", MapCode = "i", SpeciesName = "c",
                                  CohortAge = "i", CohortBiomass = "i")) %>% 
    mutate(filename = as.character(flnm),
           year = as.numeric(stringr::str_extract_all(flnm,"\\(?[0-9]+\\)?")[[1]])) 
  
}
browse_log3 <- read.csv(paste0(in_folder, "/browse-summary-log.csv"))
necn_log3 <- read.csv(paste0(in_folder, "/NECN-succession-log.csv"))
merge_log3 <- left_join(browse_log3, necn_log3, by = "Time")
merge_log3$browse <- "Ordered"

cohorts3 <-  purrr::map_df(comm_files, ~read_plus(.)) 
cohorts3$year <- as.numeric(cohorts3$year)
head(cohorts3)

cohorts3$year_added <- cohorts3$year - cohorts3$CohortAge

ggplot(cohorts3, aes(x = year, y = CohortBiomass)) +
  geom_line(aes(group = year_added, col = year_added)) + 
  facet_wrap(facets = vars(SpeciesName))

ggplot(browse_log3, aes(x = Time, y = MeanForage)) + 
  geom_line()
ggplot(necn_log3, aes(x = Time, y = AGB)) + 
  geom_line()
ggplot(merge_log3, aes(x = Time, y = MeanForage/AGB)) + 
  geom_line()

## combine

abba_merge <- rbind(merge_log, merge_log2, merge_log3)

ggplot(abba_merge) + 
  geom_line(aes(x = Time, y = AGB, linetype = browse))+ 
  scale_linetype_manual(values=c("No browse"="solid", "Linear" = "dotted", "Ordered" = "longdash"))

# ggplot() + 
#   geom_line(data = cohorts, aes(x = year, y = CohortBiomass, group = year_added, col = year_added), linetype = "solid") + 
#   geom_line(data = cohorts2, aes(x = year, y = CohortBiomass, group = year_added, col = year_added), linetype = "dotted") + 
#   geom_line(data = cohorts3, aes(x = year, y = CohortBiomass, group = year_added, col = year_added), linetype = "longdash") + 
#   facet_wrap(facets = vars(SpeciesName)) 

ggplot(abba_merge) + 
  geom_line(aes(x = Time, y = MeanForage, linetype = browse))+ 
  scale_linetype_manual(values=c("No browse"="solid", "Linear" = "dotted", "Ordered" = "longdash"))

# ggplot() +
#   geom_line(data =  browse_log, aes(x = Time, y = MeanForage), linetype = "solid") + 
#   geom_line(data =  browse_log2, aes(x = Time, y = MeanForage) , linetype = "dotted") + 
#   geom_line(data =  browse_log3, aes(x = Time, y = MeanForage) , linetype = "longdash") + 
#   scale_linetype_manual(values=c("No browse"="solid", "Linear" = "dotted", "Ordered" = "longdash"))



#################################################################################
# Mixed stand
#################################################################################


in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/mixed stand - no browse - ordered 750"

comm_files <- list.files(in_folder, full.names = TRUE) %>%
  `[`(grepl("community-input-file", .)) %>%
  `[`(grepl("csv", .))

read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c", MapCode = "i", SpeciesName = "c",
                                  CohortAge = "i", CohortBiomass = "i")) %>% 
    mutate(filename = as.character(flnm),
           year = as.numeric(stringr::str_extract_all(flnm,"\\(?[0-9]+\\)?")[[1]][2])) 
  
}

browse_log <- read.csv(paste0(in_folder, "/browse-summary-log.csv"))
necn_log <- read.csv(paste0(in_folder, "/NECN-succession-log.csv"))
merge_log <- left_join(browse_log, necn_log, by = "Time")
merge_log$browse <- "No browse"

cohorts <-  purrr::map_df(comm_files, ~read_plus(.)) 
cohorts$year <- as.numeric(cohorts$year)

cohorts$year_added <- cohorts$year - cohorts$CohortAge

ggplot(cohorts, aes(x = year, y = CohortBiomass)) +
  geom_line(aes(group = as.factor(year_added), col = as.factor(year_added))) + 
  facet_wrap(facets = vars(SpeciesName))

ggplot(browse_log, aes(x = Time, y = MeanForage)) + 
  geom_line()
ggplot(necn_log, aes(x = Time, y = AGB)) + 
  geom_line()

#-----

in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/mixed stand - no browse - linear 750"

comm_files <- list.files(in_folder, full.names = TRUE) %>%
  `[`(grepl("community-input-file", .)) %>%
  `[`(grepl("csv", .))

read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c", MapCode = "i", SpeciesName = "c",
                                  CohortAge = "i", CohortBiomass = "i")) %>% 
    mutate(filename = as.character(flnm),
           year = as.numeric(stringr::str_extract_all(flnm,"\\(?[0-9]+\\)?")[[1]][2])) 
  
}

browse_log4 <- read.csv(paste0(in_folder, "/browse-summary-log.csv"))
necn_log4 <- read.csv(paste0(in_folder, "/NECN-succession-log.csv"))
merge_log4 <- left_join(browse_log4, necn_log4, by = "Time")
merge_log4$browse <- "No browse -- linear"

cohorts <-  purrr::map_df(comm_files, ~read_plus(.)) 
cohorts$year <- as.numeric(cohorts$year)

cohorts$year_added <- cohorts$year - cohorts$CohortAge

ggplot(cohorts, aes(x = year, y = CohortBiomass)) +
  geom_line(aes(group = as.factor(year_added), col = as.factor(year_added))) + 
  facet_wrap(facets = vars(SpeciesName))

ggplot(browse_log, aes(x = Time, y = MeanForage)) + 
  geom_line()
ggplot(necn_log, aes(x = Time, y = AGB)) + 
  geom_line()


#----
in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/mixed stand - linear"


comm_files <- list.files(in_folder, full.names = TRUE) %>%
  `[`(grepl("community-input-file", .)) %>%
  `[`(grepl("csv", .))

read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c", MapCode = "i", SpeciesName = "c",
                                  CohortAge = "i", CohortBiomass = "i")) %>% 
    mutate(filename = as.character(flnm),
           year = as.numeric(stringr::str_extract_all(flnm,"\\(?[0-9]+\\)?")[[1]])) 
  
}

browse_log2 <- read.csv(paste0(in_folder, "/browse-summary-log.csv"))
necn_log2 <- read.csv(paste0(in_folder, "/NECN-succession-log.csv"))
merge_log2 <- left_join(browse_log2, necn_log2, by = "Time")
merge_log2$browse <- "Linear"
browse_cal_log2 <- read.csv(paste0(in_folder, "./browse-calibrate-log.csv"))

cohorts2 <-  purrr::map_df(comm_files, ~read_plus(.)) 
cohorts2$year <- as.numeric(cohorts2$year)

cohorts2$year_added <- cohorts2$year - cohorts2$CohortAge

ggplot(cohorts2, aes(x = year, y = CohortBiomass)) +
  geom_line(aes(group = year_added, col = year_added)) + 
  facet_wrap(facets = vars(SpeciesName))

ggplot(browse_cal_log2, aes(x = Year, y = FinalRemoval)) +
  geom_line(aes(group = CohortAge, col = CohortAge)) + 
  facet_wrap(facets = vars(CohortName))

ggplot(browse_log2, aes(x = Time, y = MeanForage)) + 
  geom_line()
ggplot(necn_log2, aes(x = Time, y = AGB)) + 
  geom_line()





in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/mixed stand - ordered"


comm_files <- list.files(in_folder, full.names = TRUE) %>%
  `[`(grepl("community-input-file", .)) %>%
  `[`(grepl("csv", .))

read_plus <- function(flnm) {
  read_csv(flnm, col_types = cols(.default = "c", MapCode = "i", SpeciesName = "c",
                                  CohortAge = "i", CohortBiomass = "i")) %>% 
    mutate(filename = as.character(flnm),
           year = as.numeric(stringr::str_extract_all(flnm,"\\(?[0-9]+\\)?")[[1]])) 
  
}
browse_log3 <- read.csv(paste0(in_folder, "/browse-summary-log.csv"))
necn_log3 <- read.csv(paste0(in_folder, "/NECN-succession-log.csv"))
merge_log3 <- left_join(browse_log3, necn_log3, by = "Time")
merge_log3$browse <- "Ordered"
browse_cal_log3 <- read.csv(paste0(in_folder, "./browse-calibrate-log.csv"))

cohorts3 <-  purrr::map_df(comm_files, ~read_plus(.)) 
cohorts3$year <- as.numeric(cohorts3$year)
head(cohorts3)

cohorts3$year_added <- cohorts3$year - cohorts3$CohortAge

ggplot(cohorts3, aes(x = year, y = CohortBiomass)) +
  geom_line(aes(group = year_added, col = year_added)) + 
  facet_wrap(facets = vars(SpeciesName))

ggplot(browse_cal_log3, aes(x = Year, y = FinalRemoval)) +
  geom_line(aes(group = CohortAge, col = CohortAge)) + 
  facet_wrap(facets = vars(CohortName))

ggplot(browse_log3, aes(x = Time, y = MeanForage)) + 
  geom_line()
ggplot(necn_log3, aes(x = Time, y = AGB)) + 
  geom_line()


mixed_merge <- rbind(merge_log, merge_log2, merge_log3, merge_log4)

ggplot(mixed_merge) + 
  geom_line(aes(x = Time, y = AGB, linetype = browse))+ 
  scale_linetype_manual(values=c("No browse"="solid", "No browse -- linear" = "dashed", "Linear" = "dotted", "Ordered" = "longdash"))

ggplot(mixed_merge) + 
  geom_line(aes(x = Time, y = MeanForage, linetype = browse))+ 
  scale_linetype_manual(values=c("No browse"="solid", "No browse -- linear" = "dashed", "Linear" = "dotted", "Ordered" = "longdash"))


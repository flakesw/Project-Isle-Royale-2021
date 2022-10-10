in_folder <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/test_browse_one_cell/young aspen - no browse"

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


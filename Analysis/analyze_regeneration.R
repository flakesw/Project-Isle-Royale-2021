# look at regeneration patterns
# from regeneration logs
library("terra")
library("tidyverse")

#---regen log
regen <- read_csv("./Models/Model runs/current - pred3/NECN-reproduction-log.csv")
abba <- filter(regen, SpeciesName == "")
regen <- read_csv("./Models/Model runs/miroc - pred3/NECN-reproduction-log.csv")
abba2 <- filter(regen, SpeciesName == "") %>%
  filter(Time %in% abba$Time)

plot(abba$NumCohortsSeed ~ abba$Time)
plot(abba2$NumCohortsSeed ~ abba2$Time)




#---maple disaster
regen <- read_csv("./Models/Model runs/current - pred3/NECN-reproduction-log.csv")
acru <- filter(regen, SpeciesName == "ACRU")
browse <- read_csv("./Models/Model runs/miroc - pred3/browse-event-species-log.csv")
acru_browse <- filter(browse, SpeciesName == "ACRU")

acru <- left_join(acru, acru_browse)

ggplot(acru) +
  # geom_line(mapping = aes(x = Time, y = NumCohortsSeed)) +
  # geom_line(mapping = aes(x = Time, y = TotalCohortsKilled)) + 
  geom_line(mapping = aes(x = Time, y = NumCohortsSeed - TotalCohortsKilled))

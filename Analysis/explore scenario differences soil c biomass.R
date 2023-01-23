library(terra)
library(tidyverse)

soilc_init <- rast("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/NECN/SOMTC-20.img")

soilc1 <- rast("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/NECN/SOMTC-80.img")
plot(soilc1)


# soilcdiff <- soilc1 - soilc2
# NAflag(soilcdiff) <- 0
# plot(soilcdiff)

soilc_change <- soilc1 - soilc_init

NAflag(soilc_change) <- 0

plot(soilc_change)



som1surf_init <- rast("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/NECN/SOMTC-20.img")

soilc1 <- rast("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/NECN/SOMTC-80.img")
plot(soilc1)


# soilcdiff <- soilc1 - soilc2
# NAflag(soilcdiff) <- 0
# plot(soilcdiff)

soilc_change <- soilc1 - soilc_init

NAflag(soilc_change) <- 0

plot(soilc_change)

zoom(soilc_change)
click(soilc_change)








## biomass over time

biomass_stack <- list.files("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/biomass",
                          full.names = TRUE) %>%
  `[`(grepl("TotalBiomass", .)) %>%
  rast()

plot(biomass_stack, range = c(0, 30000))



##

biomass_init <- rast("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/biomass/bio2-TotalBiomass-0.img")
plot(biomass_init)
hist(values(biomass_init)[values(biomass_init) > 0])

biomass1 <- rast("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/biomass/bio2-TotalBiomass-80.img")
plot(biomass1)
hist(values(biomass1)[values(biomass1) > 0])



# biomassdiff <- biomass1 - biomass2
# NAflag(biomassdiff) <- 0
# plot(biomassdiff)

biomass_change <- biomass1 - biomass_init
NAflag(biomass_change) <- 0
hist(values(biomass_change))
plot(biomass_change)
zoom(biomass_change)
click(biomass_change)


biomass_init <- rast("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/biomass/bio2-TotalBiomass-0.img")
plot(biomass_init)
hist(values(biomass_init)[values(biomass_init) > 0])

biomass1 <- rast("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/biomass/bio2-TotalBiomass-80.img")
plot(biomass1)
hist(values(biomass1)[values(biomass1) > 0])
biomass_change <- biomass1 - biomass_init
NAflag(biomass_change) <- 0
plot(biomass_change)

comm_output_end <- read_csv("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/community-input-file-80.csv")
comm_map_end <- rast("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/output-community-80.img")

sites_low_biomass <- values(comm_map_end)[which(values(biomass_change) < -20000)]
comm_low_biomass <- comm_output_end[comm_output_end$MapCode %in% sites_low_biomass, ]


sites_high_biomass <- values(comm_map_end)[which(values(biomass_change) > 10000)]
comm_high_biomass <- comm_output_end[comm_output_end$MapCode %in% sites_high_biomass, ]

comm_map_test <- comm_map_end
values(comm_map_test) <- 0
# values(comm_map_test)[values(comm_map) %in% comm_low_biomass$MapCode] <- 
#   values(comm_map)[values(comm_map) %in% comm_low_biomass$MapCode]
values(comm_map_test)[values(comm_map_end) %in% comm_high_biomass$MapCode] <- 
  values(comm_map_end)[values(comm_map_end) %in% comm_high_biomass$MapCode]
plot(comm_map_test)


### What sort of sites experienced greatest increases in biomass?
comm_output_init <- read_csv("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/community-input-file-0.csv")
comm_map_init <- rast("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/output-community-0.img")

sites_high_biomass_init <- values(comm_map_init)[which(values(biomass_change) > 10000)]
comm_high_biomass_init <- comm_output_init[comm_output_init$MapCode %in% sites_high_biomass_init, ]
sites_high_biomass_end <- values(comm_map_end)[which(values(biomass_change) > 10000)]
comm_high_biomass <- comm_output_end[comm_output_end$MapCode %in% sites_high_biomass_init, ]

#which species are most common in sites that gained biomass?
test <- comm_high_biomass_init %>%
  group_by(SpeciesName) %>%
  summarise(total_biomass = sum(CohortBiomass))
test2 <- comm_high_biomass %>%
  group_by(SpeciesName) %>%
  summarise(total_biomass = sum(CohortBiomass))

join <- left_join(test2, test, by = "SpeciesName")
join$change <- join$total_biomass.x - join$total_biomass.y

################

alder_stack <- list.files("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/biomass",
                          full.names = TRUE) %>%
  `[`(grepl("ALIN", .)) %>%
  rast()

plot(alder_stack)


###
eco <- rast("./Models/LANDIS Inputs/input rasters/ecoregions_inv.tif")

###
soil_drain <- rast("./Models/LANDIS Inputs/input rasters/soil_drain.tif")
plot(soil_drain)

##
soil_moisture_stack <-  list.files("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil and mc/necn",
                                   full.names = TRUE) %>%
  `[`(grepl("AvailableWater", .)) %>%
  rast()
plot(soil_moisture_stack)

sm_mean <- mean(soil_moisture_stack)
plot(sm_mean)
zoom(sm_mean)
click(sm_mean)

hist(values(sm_mean)[!is.na(values(eco))], xlim = c(0, 50), breaks = seq(0, 101, length.out = 100))


plot(values(soilc_change) ~ values(sm_mean))


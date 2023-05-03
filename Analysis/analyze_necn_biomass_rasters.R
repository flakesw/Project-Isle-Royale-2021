library(terra)
library(tidyverse)

diverging_color_ramp <- function(ras){
  the_palette_fc <- leaflet::colorNumeric(palette = "RdBu", 
                                          domain = c(-max(abs(ras[]), na.rm = TRUE), max(abs(ras[]), na.rm = TRUE)),
                                          reverse = TRUE)
  the_colors <- the_palette_fc(seq(min(ras[], na.rm = TRUE), max(ras[], na.rm = TRUE), length.out = 50))

}


soilc_init <- rast("./Models/Model runs/miroc - pred3/NECN/SOMTC-1.img")
plot(soilc_init)
hist(values(soilc_init)[values(soilc_init) != 0])
soilc1 <- rast("./Models/Model runs/miroc - pred3/NECN/SOMTC-80.img")
plot(soilc1)
hist(values(soilc1)[values(soilc1) != 0])


# soilcdiff <- soilc1 - soilc2
# NAflag(soilcdiff) <- 0
# plot(soilcdiff)

soilc_change <- soilc1 - soilc_init

NAflag(soilc_change) <- 0

plot(soilc_change, col = diverging_color_ramp(soilc_change))
hist(values(soilc_change))
median(values(soilc_change), na.rm = TRUE)
clamped_ratio_change <- clamp(soilc_change/soilc_init, upper = 1)
plot(clamp(soilc_change/soilc_init, upper = 1))
plot(values(soilc_change)/values(soilc_init) ~ values(soilc_init))
plot(values(soilc_change) ~ values(soilc_init))



























#exploring some explanatory variables-------------------------------------------
soilm <- rast("./Models/landis_test/mc_test - linear/NECN/SoilWater-60.img")
plot(soilm)

soil_drain <- rast("./Models/LANDIS Inputs/input rasters/soil_drain.tif")
plot(soil_drain)

soil_sand <- rast("./Models/LANDIS Inputs/input rasters/sand.tif")
plot(soil_sand)

soil_depth <- rast("./Models/LANDIS Inputs/input rasters/soil_depth.tif")
plot(soil_depth)

wilt <- rast("./Models/LANDIS Inputs/input rasters/wilt_point.tif")
fc <- rast("./Models/LANDIS Inputs/input rasters/field_capacity.tif") 

stormflow <- rast("./Models/LANDIS Inputs/input rasters/stormflow.tif")
baseflow <- rast("./Models/LANDIS Inputs/input rasters/baseflow.tif") 

total_n <- rast("./Models/landis_test/mc_test - linear/NECN-Initial-Conditions/SOM1Nsoil-5.img")+
  rast("./Models/landis_test/mc_test - linear/NECN-Initial-Conditions/SOM1Nsurface-5.img")+
  rast("./Models/landis_test/mc_test - linear/NECN-Initial-Conditions/SOM2N-5.img")+
  rast("./Models/landis_test/mc_test - linear/NECN-Initial-Conditions/SOM3N-5.img")


biomass_init <- rast("./Models/landis_test/mc_test - linear/biomass/bio2-TotalBiomass-0.img")

alder_biomass <- rast("./Models/landis_test/mc_test - linear/biomass/bio2-ALIN2-0.img")

anerb <-  rast("./Models/landis_test/mc_test - linear/NECN/AnaerobicEffect-10.img")


hist(values(soilc_init)[values(clamped_ratio_change) > 0.99])
hist(values(soilc_init)[values(clamped_ratio_change) < 0.99])
hist(values(biomass_init)[values(clamped_ratio_change) > 0.99])
hist(values(biomass_init)[values(clamped_ratio_change) < 0.99])
hist(values(soil_depth)[values(clamped_ratio_change) > 0.99])
hist(values(soil_depth)[values(clamped_ratio_change) < 0.99])
hist(values(soil_drain)[values(clamped_ratio_change) > 0.99])
hist(values(soil_drain)[values(clamped_ratio_change) < 0.99])
hist(values(soil_sand)[values(clamped_ratio_change) > 0.99])
hist(values(soil_sand)[values(clamped_ratio_change) < 0.99])
hist(values(wilt)[values(clamped_ratio_change) > 0.99])
hist(values(wilt)[values(clamped_ratio_change) < 0.99])
hist(values(fc)[values(clamped_ratio_change) > 0.99])
hist(values(fc)[values(clamped_ratio_change) < 0.99])
hist(values(stormflow)[values(clamped_ratio_change) > 0.99])
hist(values(stormflow)[values(clamped_ratio_change) < 0.99])
hist(values(baseflow)[values(clamped_ratio_change) > 0.99])
hist(values(baseflow)[values(clamped_ratio_change) < 0.99])
hist((values(soilc_init)/values(total_n))[values(clamped_ratio_change) > 0.99], xlim = c(0, 100))
hist((values(soilc_init)/values(total_n))[values(clamped_ratio_change) < 0.99], xlim = c(0, 100))
hist(values(alder_biomass)[values(clamped_ratio_change) > 0.99])
hist(values(alder_biomass)[values(clamped_ratio_change) < 0.99])
hist(values(anerb)[values(clamped_ratio_change) > 0.99])
hist(values(anerb)[values(clamped_ratio_change) < 0.99])

plot(values(biomass_init)[values(clamped_ratio_change) > 0.99] ~ values(soilc_change)[values(clamped_ratio_change) > 0.99])
plot(values(soil_depth)[values(clamped_ratio_change) > 0.99] ~ values(soilc_change)[values(clamped_ratio_change) > 0.99])
plot(values(soil_drain)[values(clamped_ratio_change) > 0.99] ~ values(soilc_change)[values(clamped_ratio_change) > 0.99])
plot(values(soil_sand)[values(clamped_ratio_change) > 0.99] ~ values(soilc_change)[values(clamped_ratio_change) > 0.99])
plot(values(wilt)[values(clamped_ratio_change) > 0.99] ~ values(soilc_change)[values(clamped_ratio_change) > 0.99])
plot(values(fc)[values(clamped_ratio_change) > 0.99] ~ values(soilc_change)[values(clamped_ratio_change) > 0.99])
plot(values(stormflow)[values(clamped_ratio_change) > 0.99] ~ values(soilc_change)[values(clamped_ratio_change) > 0.99])
plot(values(baseflow)[values(clamped_ratio_change) > 0.99] ~ values(soilc_change)[values(clamped_ratio_change) > 0.99])
plot(values(alder_biomass)[values(clamped_ratio_change) > 0.99] ~ values(soilc_change)[values(clamped_ratio_change) > 0.99])
plot(values(anerb)[values(clamped_ratio_change) > 0.99] ~ values(soilc_change)[values(clamped_ratio_change) > 0.99])


#there's some weird wetland-ish plots with 200cm soil, near-0% sand, 0 drain that have no soil C to start
#there's a gradient that's affecting productivity (probably) for a given initial stand composition, which causes diagonal lines in the initial biomass ~ soilc_change plot
#mostly (but not entirely) higher baseflow areas
#more low and high C:N ratio sites than expected
#very few have anaerobic effect

#som1Csurf
som1surf_init <- rast("./Models/landis_test/mc_test - linear/NECN-Initial-Conditions/SOM1Csurface-20.img")
plot(som1surf_init)
som1surf_80 <- rast("./Models/landis_test/mc_test - linear/NECN-Initial-Conditions/SOM1Csurface-80.img")
plot(som1surf_80)

soilc_change <- som1surf_80 - som1surf_init
soilc_change <- soilc_change/60

NAflag(soilc_change) <- 0
plot(soilc_change, col = diverging_color_ramp(soilc_change))
zoom(soilc_change)
click(soilc_change)

##som1Csoil
som1soil_init <- rast("./Models/landis_test/mc_test/NECN-Initial-Conditions/SOM1Csoil-20.img")
plot(som1soil_init)
som1soil_80 <- rast("./Models/landis_test/mc_test/NECN-Initial-Conditions/SOM1Csoil-80.img")
plot(som1soil_80)

soilc_change <- som1soil_80 - som1soil_init
soilc_change <- soilc_change/60 #grams per meter squared per year

NAflag(soilc_change) <- 0
plot(soilc_change, col = diverging_color_ramp(soilc_change))
zoom(soilc_change)
click(soilc_change)

#SOM2----------------------
som2_init <- rast("./Models/landis_test/mc_test/NECN-Initial-Conditions/SOM2C-20.img")
plot(som2_init)
som2_80 <- rast("./Models/landis_test/mc_test/NECN-Initial-Conditions/SOM2C-80.img")
plot(som2_80)

soilc_change <- som2_80 - som2_init
soilc_change <- soilc_change/60 #grams per meter squared per year

NAflag(soilc_change) <- 0
plot(soilc_change, col = diverging_color_ramp(soilc_change))
zoom(soilc_change)
click(soilc_change)

#SOM3----------------------
som3_init <- rast("./Models/landis_test/mc_test/NECN-Initial-Conditions/SOM3C-20.img")
plot(som3_init)
som3_80 <- rast("./Models/landis_test/mc_test/NECN-Initial-Conditions/SOM3C-80.img")
plot(som3_80)

soilc_change <- som1soil_80 - som1soil_init
soilc_change <- soilc_change/80 #grams per meter squared per year

NAflag(soilc_change) <- 0
plot(soilc_change, col = diverging_color_ramp(soilc_change))
zoom(soilc_change)
click(soilc_change)


#-----------------------------------------------------

## biomass over time

biomass_stack <- list.files("./Models/Model templates/spinup model/biomass",
                          full.names = TRUE) %>%
  `[`(grepl("TotalBiomass", .)) %>%
  rast()

plot(biomass_stack, range = c(0, 30000))


## ANPP ------------------------------------------------------------------------

anpp_5 <- rast("./Models/landis_test/mc_test/NECN/AG_NPP-5.img")
plot(anpp_5)
mean(values(anpp_5)[values(anpp_5) > 0])
anpp_80 <- rast("./Models/landis_test/mc_test/NECN/AG_NPP-80.img")
plot(anpp_80)
mean(values(anpp_80)[values(anpp_80) > 0])

anpp_change <- anpp_80 - anpp_5

NAflag(anpp_change) <- 0

plot(anpp_change, col = diverging_color_ramp(anpp_change))

## NEE ------------------------------------------------------------------------

nee_5 <- rast("./Models/landis_test/mc_test/NECN/ANEE-5.img") - 1000
NAflag(nee_5) <- -1000
plot(nee_5, col = diverging_color_ramp(nee_5))
nee_80 <- rast("./Models/landis_test/mc_test/NECN/ANEE-80.img") - 1000
NAflag(nee_80) <- -1000
plot(nee_80, col = diverging_color_ramp(nee_80))

nee_change <- nee_80 - nee_5

NAflag(nee_change) <- 0

plot(nee_change, col = diverging_color_ramp(nee_change))



#---------------------------- wind
wind <- rast("./Models/landis_test/mc_test - linear/wind/severity-30.img")
plot(wind)

## Change in biomass ----------------------

biomass_init <- rast("./Models/Model templates/spinup model/biomass/bio2-TotalBiomass-0.img")
plot(biomass_init)
hist(values(biomass_init)[values(biomass_init) > 0])
mean(values(biomass_init)[values(biomass_init) > 0])

biomass1 <- rast("./Models/Model templates/spinup model/biomass/bio2-TotalBiomass-20.img")
plot(biomass1)
hist(values(biomass1)[values(biomass1) > 0])
mean(values(biomass1)[values(biomass1) > 0])

biomass_change <- biomass1 - biomass_init
NAflag(biomass_change) <- 0
hist(values(biomass_change))
plot(biomass_change, col = diverging_color_ramp(biomass_change))
zoom(biomass_change)
click(biomass_change)

#compare biomass change and soil c change
plot(values(biomass_change) ~ values(soilc_change))
#there is a group of similar plots that lost a ton of biomass and gained a lot of soil C
#Most sites that lost a lot of soil C had little change in biomass


#what sites had big changes in biomass? ----------------------------------------
#using community input files
comm_output_end <- read_csv("./Models/landis_test/mc_test - linear/community-input-file-80.csv")
comm_map_end <- rast("./Models/landis_test/mc_test - linear/output-community-80.img")
comm_output_begin<- read_csv("./Models/landis_test/mc_test - linear/community-input-file-0.csv")
comm_map_begin <- rast("./Models/landis_test/mc_test - linear/output-community-0.img")
comm_output_init <- read_csv("./Models/LANDIS inputs/NECN files/initial_communities_inv.csv")
comm_map_init <- rast("./Models/LANDIS inputs/input rasters/initial_communities_inv.tif")

sites_low_biomass <- values(comm_map_end)[which(values(biomass_change) < -10000)]
comm_low_biomass <- comm_output_end[comm_output_end$MapCode %in% sites_low_biomass, ]
sites_low_biomass_begin <- values(comm_map_begin)[which(values(biomass_change) < -10000)]
comm_low_biomass_begin <- comm_output_begin[comm_output_begin$MapCode %in% sites_low_biomass_begin, ]
sites_low_biomass_init <- values(comm_map_init)[which(values(biomass_change) < -10000)]
comm_low_biomass_init <- comm_output_init[which(comm_output_init$MapCode %in% sites_low_biomass_init), ]
table(sites_low_biomass_init)


sites_high_biomass <- values(comm_map_end)[which(values(biomass_change) > 10000)]
comm_high_biomass <- comm_output_end[comm_output_end$MapCode %in% sites_high_biomass, ]

#where are cells with big losses in biomass?
comm_map_test <- comm_map_init
values(comm_map_test) <- 0
# values(comm_map_test)[values(comm_map) %in% comm_low_biomass$MapCode] <- 
#   values(comm_map)[values(comm_map) %in% comm_low_biomass$MapCode]
values(comm_map_test)[values(comm_map_end) %in% comm_low_biomass$MapCode] <- 
  values(comm_map_init)[values(comm_map_end) %in% comm_low_biomass$MapCode]
plot(comm_map_test)



#site to investigate
sites <- which(values(clamped_ratio_change) >0.5)
sites <- which(values(soilc_change) > 4000) 
sites <- which(values(biomass_change) < -5000) 
table(comm_map_init[sites])
site <- sites[4]
comm_map_init[site]
sites[which(values(comm_map_init) == 100) & which(values(biomass_change) < -20000)]


#single out one mapcode
test <- biomass_change
values(test) <- 0
values(test)[values(comm_map_init) == 115] <- values(biomass_change)[values(comm_map_init) == 115]
NAflag(test) <- 0
plot(test)

table(values(comm_map_init))
which(values(comm_map_init) == 28 & values(soilc_change) > 6000)

### What sort of sites experienced greatest increases in biomass?
comm_output_init <- read_csv("./Models/landis_test/mc_test/community-input-file-0.csv")
comm_map_init <- rast("./Models/landis_test/mc_test/output-community-0.img")

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
plot(alder_stack[[1]])
plot(alder_stack[[2]])
plot(alder_stack[[1]] -alder_stack[[2]])

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






#------------------------------------------------------------------------------
# Compare models --------------------------------------------------------------


biomass_nobrowse <- rast("./Models/Model runs/current - pred3/biomass/TotalBiomass-80.img")
plot(biomass_nobrowse)
hist(values(biomass_nobrowse)[values(biomass_nobrowse) > 0])

biomass_browse <- rast("./Models/Model runs/miroc - pred3/biomass/TotalBiomass-80.img")
plot(biomass_browse)
# zoom(biomass_browse)
hist(values(biomass_browse)[values(biomass_browse) > 0])



diff <- biomass_nobrowse - biomass_browse
plot(diff, col = diverging_color_ramp(diff))
zoom(diff)

plot(values(diff) ~ values(soil_drain))
plot(values(diff) ~ values(sm_mean))

summary(lm(values(diff) ~ values(soil_drain)))
summary(lm(values(diff) ~ values(sm_mean)))
#moose slow wetland encroachment, beacuse there is a lot of establishment in wet areas

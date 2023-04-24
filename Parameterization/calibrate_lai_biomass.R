biomass_data <- read.csv("./Parameterization/necn_parameterize/biomass and lai/biomass_anpp_lai_empirical_data.csv")
biomass_data$biomass_c <- biomass_data$Total.aboveground.biomass..g.m.2. * 0.47

table(biomass_data[!is.na(biomass_data$LAI) & !is.na(biomass_data$Total.aboveground.biomass..g.m.2.), "Species"])
table(biomass_data[!is.na(biomass_data$LAI) & !is.na(biomass_data$Annual.production..g.m.2.), "Species"])


biomass_data <- biomass_data[grepl("Populus", biomass_data$Species), ]
plot(biomass_data$LAI ~ biomass_data$biomass_c, 
     xlim = c(0, 10000), 
     ylim = c(0, 12))


#base_LAI
#base_LAI is calculated from cohort biomass, using the KLAI and MaxLAI parameters
#base_LAI = maxlai * woodC/(klai + woodC)
woodc = seq(0, 10000, length.out = 200)
maxlai = 12
klai = 12000
base_LAI = maxlai * woodc / (klai + woodc)
lines(base_LAI ~ woodc)

#biomass-to-LAI parameter
# This parameter controls how much leaf area is present when trees are leafing out.
# Each year, new leaf biomass is grown, which is translated to LAI by:
# 1 - exp(btolai * LeafC)
# which is a "seasonal adjustment" parameter that gets multiplied by cohort base_LAI
# to downscale to early-season LAI

plot(biomass_data$Annual.production..g.m.2.*0.47 ~ biomass_data$Leaf.biomass..g.m.2.)

btolai <- -0.1

leaf_bio <- seq(0, 200, length.out = 200)
seasonal_adjustment = 1-exp(btolai * leaf_bio)
plot(seasonal_adjustment ~ leaf_bio)

#lai-to-growth parameter
#once base_LAI and the scaling for leaf biomass have been done, 
#LAI_Growth_limit = Math.Max(0.0, 1.0 - Math.Exp(lai_to_growth * lai))
plot(biomass_data$Annual.production..g.m.2. * 0.47 ~ biomass_data$LAI)
max_npp <- 400
plot(biomass_data$Annual.production..g.m.2.*0.47/max_npp ~ biomass_data$LAI, xlim = c(0,12), ylim = c(0,1))

lai_to_growth <- 1
lai <- seq(0, 12, length.out = 200)
lai_growth_limit = 1 - exp(-lai_to_growth * lai)
lines(lai_growth_limit ~ lai)



#competition LAI limit
#the competition LAI limit is the same for all species (k = -0.14)

lai <- seq(0, 12, length.out = 200)
comp_limit = exp(-0.14 * lai)
plot(comp_limit ~ lai)


# npp ~ cohort biomass
plot(biomass_data$Annual.production..g.m.2.*0.47 ~ biomass_data$Total.aboveground.biomass..g.m.2.)



# biomass ~ age
plot(biomass_data$Total.aboveground.biomass..g.m.2. ~ biomass_data$Age)



#TODO we can calibrate LANDIS model runs with the lai ~ age data


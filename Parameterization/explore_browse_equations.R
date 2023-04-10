##test regressions from persson

#pine with no forage 
ln(browse) = -2.87 + 0.44*(ln(h)) + 1.62*(ln(D)) #(Persson 2007)
ln(browse) = -1.64 +1.92 * log(diam) #Persson 2005

#birch forage (Persson 2005)
log(browse) = -3.14 + 2.06*log(D)

#birch with no forage (twig)
ln(browse) = -3.18 + 0.06*ln(H) + 1.98 * ln(D)

#birch with no forage (leaves)
ln(browse) = -7.26 + 1.29 * ln(H) + 1.42 * ln(D)


#jenkins equation for pine
bm = exp(-2.5356 + 2.4349 * ln(D))

#jenkins equation for birch
bm = exp(-1.9123 + 2.3651 * ln(D))


#jenkins foliage component for hardwoods
bm = exp(-4.0813 + 5.8816 * ln(D))

#jenkins foliage component for softwoods
bm = exp(-2.9583 + 4.4766 * ln(D))

#---------------------------
#range of diameters, using 80:1 ratio of height:diam
data = data.frame(D = seq(1, 10, length.out = 100),
                  H = seq(80, 800, length.out = 100))


data$pine_browse <- exp(-1.64 +1.92 * log(data$D))
plot(pine_browse ~ data$D)

data$pine_biomass <- exp(-2.5356 + 2.4349 * log(data$D)) * 1000

plot(pine_browse ~ pine_biomass)
#proportion of biomass that is browse
plot(I(pine_browse / pine_biomass) ~ pine_biomass)

#same for birch
data$birch_browse <- exp(-3.14 + 2.06*log(data$D))
plot(birch_browse ~ data$D)

data$birch_biomass <- exp(-4.0813 + 5.8816 * log(data$D)) * 1000

plot(birch_browse ~ birch_biomass)
#proportion of biomass that is browse
plot(I(birch_browse / birch_biomass) ~ birch_biomass)


#############



plot_biomass <- 10000
dia <- 2

#a 60x60m plot with 4000 g m-2 of 5-cm pine biomass
total_biomass <- 60*60*plot_biomass
biomass_per_tree <- exp(-2.5356 + 2.4349 * log(dia)) * 1000
n_trees <- total_biomass / biomass_per_tree
browse_per_tree <-  exp(-1.64 + 1.92 * log(dia))
total_browse <- n_trees*browse_per_tree
browse_per_m2 <- total_browse / 60/60
browse_per_m2
browse_per_tree / biomass_per_tree


#birch
data$birch_browse <- exp(-3.14 + 2.06*log(data$D))

data$birch_biomass <- exp(-1.9123 + 2.3651 * log(data$D)) * 1000
plot(birch_browse ~ birch_biomass)
plot(I(birch_browse / birch_biomass) ~ birch_biomass)

total_biomass <- 60*60*plot_biomass
biomass_per_tree <- exp(-1.9123 + 2.3651 * log(dia)) * 1000
n_trees <- total_biomass / biomass_per_tree
browse_per_tree <- exp(-3.14 + 2.06*log(dia))
  #exp(-3.18 + 0.06*log(5*80) + 1.98 * log(5)) +
  #exp(-7.26 + 1.29 * log(5*80) + 1.42 * log(5))
total_browse <- n_trees*browse_per_tree
browse_per_m2 <- total_browse / 60/60
browse_per_m2
browse_per_tree / biomass_per_tree


#------------------- try to match Persson plots




#---------------------
#calculate forage for a range of biomass and diameters

calculate_forage_pine <- function(plot_biomass, dia){
  total_biomass <- 60*60*plot_biomass
  biomass_per_tree <- exp(-2.5356 + 2.4349 * log(dia)) * 1000
  n_trees <- total_biomass / biomass_per_tree
  browse_per_tree <-  exp(-1.64 + 1.92 * log(dia))
  total_browse <- n_trees*browse_per_tree
  browse_per_m2 <- total_browse / 60/60
  browse_per_m2
  return(browse_per_m2)
}

calculate_forage_birch <- function(plot_biomass, dia){
  total_biomass <- 60*60*plot_biomass
  biomass_per_tree <- exp(-1.9123 + 2.3651 * log(dia)) * 1000
  n_trees <- total_biomass / biomass_per_tree
  browse_per_tree <- exp(-3.14 + 2.06*log(dia))
  total_browse <- n_trees*browse_per_tree
  browse_per_m2 <- total_browse / 60/60
  browse_per_m2
  return(browse_per_m2)
}

#----------------
#landis forage
get_forage_in_reach <- function(biomass, 
                                max_bio, 
                                MinBrowsePropinReach, 
                                BrowseBiomassThresholdMin, 
                                BrowseBiomassThresholdMax){
  forage <- biomass * 0.04 * 0.2
  prop_max <- biomass/26900
  max_threshold <- BrowseBiomassThresholdMax*max_bio
  min_threshold <- BrowseBiomassThresholdMin * max_bio
  
  prop_in_reach <- ifelse(biomass < min_threshold, 1, 
                          ifelse(biomass > max_threshold, 0,
                          1 - biomass/max_threshold))
  
  if(prop_in_reach < 0.3) prop_in_reach <- 0
  
  forage_in_reach <- forage * prop_in_reach
  return(forage_in_reach)
}

#------------calculate values for comparison
dia <- 20

forage_biomass <- data.frame(biomass = seq(1:2000),
                             pine_browse = numeric(4000),
                             pine_browse_landis = numeric(4000),
                             birch_browse = numeric(4000),
                             birch_browse_landis = numeric(4000))

forage_biomass$pine_browse <- calculate_forage_pine(forage_biomass$biomass, dia = dia)
forage_biomass$birch_browse <- calculate_forage_birch(forage_biomass$biomass, dia = dia)

forage_biomass$pine_browse_landis <- sapply(forage_biomass$biomass, FUN = function(x) get_forage_in_reach(biomass = x,
                                                                                 max_bio =26000,
                                                                                 MinBrowsePropinReach = 0.3,
                                                                                 BrowseBiomassThresholdMin = 0.015,
                                                                                 BrowseBiomassThresholdMax = 0.04))


forage_biomass$birch_browse_landis <- sapply(forage_biomass$biomass, FUN = function(x) get_forage_in_reach(biomass = x,
                                                                                   max_bio = 11324,
                                                                                   MinBrowsePropinReach = 0.3,
                                                                                   BrowseBiomassThresholdMin = 0.015,
                                                                                   BrowseBiomassThresholdMax = 0.04))




##proportion that is forage
forage_biomass$pine_browse_prop <- forage_biomass$pine_browse / forage_biomass$biomass
forage_biomass$pine_browse_prop_landis <- forage_biomass$pine_browse_landis / forage_biomass$biomass
forage_biomass$birch_browse_prop <- forage_biomass$birch_browse / forage_biomass$biomass
forage_biomass$birch_browse_prop_landis <- forage_biomass$birch_browse_landis / forage_biomass$biomass



plot(forage_biomass$pine_browse_landis ~ forage_biomass$biomass)
lines(forage_biomass$pine_browse ~ forage_biomass$biomass)

plot(forage_biomass$birch_browse_landis ~ forage_biomass$biomass)
lines(forage_biomass$birch_browse ~ forage_biomass$biomass)

plot(forage_biomass$pine_browse_prop_landis ~ forage_biomass$pine_browse_prop)
plot(forage_biomass$birch_browse_prop_landis ~ forage_biomass$birch_browse_prop)

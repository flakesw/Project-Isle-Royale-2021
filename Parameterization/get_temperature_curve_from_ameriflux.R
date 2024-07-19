#get moisture curves from processed ameriflux data
#see process_ameriflux_gpp for processing steps
library("tidyverse")
library("minpack.lm")
library("sf")
library("terra")

options(warn = -1) #suppress warnings when reading in files; there's a better way to do this I bet

#edited csv
sites_to_use <- read.csv("./Parameterization/Parameterization data/ameriflux data/ameriflux_gpp_swc_edit.csv") %>%
  filter(Use)

site_files <- paste0("./Parameterization/necn_parameterize/ameriflux_processed/", sites_to_use$SITE_ID, "_processed.csv")

site_info <- read_tsv("./Parameterization/Parameterization data/ameriflux data/AmeriFlux-site-info.tsv")

sites_to_use <- left_join(sites_to_use, site_info, by = c("SITE_ID" = "Site ID"))

# biomass <- terra::rast("D:/Data/gedi/GEDI_L4B_Gridded_Biomass_2017/data/GEDI04_B_MW019MW138_02_002_05_R01000M_MU.tif")
biomass <- terra::rast("D:/Data/spawn_biomass/aboveground_biomass_carbon_2010.tif")

site_loc <- sites_to_use %>%
  dplyr::select(SITE_ID, `Latitude (degrees)`, `Longitude (degrees)`) %>%
  dplyr::rename(Lat = `Latitude (degrees)`,
                Lon = `Longitude (degrees)`) %>%
  sf::st_as_sf(coords = c("Lon", "Lat"), crs = "EPSG:4326") %>%
  sf::st_transform(crs(biomass))

biomass <- terra::crop(biomass, site_loc)
plot(biomass)
plot(vect(site_loc), add = TRUE)

sites_to_use$biomass <- terra::extract(biomass, vect(site_loc))$aboveground_biomass_carbon_2010 * 10 /0.47
sites_to_use$biomass <- ifelse(is.na(sites_to_use$biomass), 5000, sites_to_use$biomass)

#some helper functions
read_plus <- function(flnm) {
  print(flnm)
  
  read_csv(flnm) %>%
    group_by(YEAR, DOY) %>%
    summarise(tairmax = max(Tair, na.rm = TRUE),
              tairmin = min(Tair, na.rm = TRUE),
              gpp = mean(c(GPP_PI_F), na.rm = TRUE),
              site = str_split(basename(flnm), "_")[[1]][1]) %>%
    left_join(dplyr::select(sites_to_use, SITE_ID, biomass), by = c("site" = "SITE_ID")) %>%
    #calculate soil temperature like LANDIS, using biomass = 12000 g m^-2
    mutate(tsoilmax = tairmax + (25.4 / (1.0 + 18.0 * exp(-0.20 * tairmax))) * 
             (exp(-0.00350 * 4000) - 0.13),
           tsoilmin = tairmin + 0.00400 * 4000 - 1.78,
           tsoil = (tsoilmax + tsoilmin)/2) %>%
    ungroup() %>%
    group_by(site) %>%
    mutate(maxrelGPP = gpp/quantile(gpp, 0.999, na.rm = TRUE)) %>%
    mutate(maxrelGPP = ifelse(maxrelGPP < 0, NA, maxrelGPP)) 
}



ft <- unique(sites_to_use$Functional.group)
table(sites_to_use$Functional.group)
ft
#which of the sites should we use?
#note: for facultative wet conifer, used data from wet and dry conifers
#shrubs use data for aspen, northern hardwoods, mesic warm conifers, temperate hardwoods, dry/cold pines
i <- which(sites_to_use$Functional.group %in% ft[c(5)])
# i <- c(1, 18, 19, 20, 23)
base_daily <- site_files[i]  %>%
  purrr::map_df(~read_plus(.))


#calculate curve for only upper percentiles -- easier to fit curve to
Observed_Relative_production<- base_daily$maxrelGPP

tsoil<- seq(-40, 40, length.out = 160)

out_data <- base_daily[0, ]

for(j in 1:length(tsoil)){ 
  
  sub_data <- base_daily[which(base_daily$tsoil > tsoil[j] & base_daily$tsoil < tsoil[j+1]), ]
  
  val <- quantile(sub_data$maxrelGPP, .99, na.rm = TRUE)
  
  temp_data <- sub_data[which(sub_data$maxrelGPP > val), ]
  out_data <- rbind(out_data, temp_data)
}

Observed_Relative_production <- out_data$maxrelGPP
tsoil <- out_data$tsoil
tair <- I((out_data$tairmax + out_data$tairmin)/2)

plot(tsoil ~ tair)
summary(lm(tsoil ~ tair))

plot(out_data$tairmax ~ out_data$tairmin)
abline(0,1)

#plot raw data
plot(tair, Observed_Relative_production, col="blue", type="p",
     ylim = c(0,1),
     xlim = c(-40, 40),
     ylab="Maximum Relative GPP", xlab="Air T (C)")
plot(tsoil, Observed_Relative_production, col="blue", type="p",
     ylim = c(0,1),
     xlim = c(-40, 40),
     ylab="Maximum Relative GPP", xlab="Soil T (C)")


# Function to fit coefficients to water curve. 

#original coefficients from Shelby
# moisture_1<- 0.13 #soilwater with maximum gpp
# moisture_2<- 1 #maximum soilwater with any GPP -- changes steepness of decline in gpp
# moisture_3<- 3 #higher values -- sharper dip down towards zero
# moisture_4<- 15 #smaller numbers -- broader curve

#starting coefficients
moisture_1<- 25 #temp with maximum gpp
moisture_2<- 40 #maximum or minimum tempwith any GPP -- changes steepness of decline in gpp
moisture_3<- 1.8186 #higher values -- sharper dip down towards zero
moisture_4<- 2.0308 #smaller numbers -- broader curve

#TSWC goes from 0 to 1
tsoil_val<-sort(runif(1000, -40, 40))

#Calculate fraction here
fraction_val <-(moisture_2 - tsoil_val)/(moisture_2 - moisture_1)

#This is the curve based on the coefficients listed above
Landis_Relative_production<-ifelse(fraction_val>0, (exp(moisture_3/moisture_4*(1-fraction_val^moisture_4))*(fraction_val^moisture_3)), 0)
lines(tsoil_val, Landis_Relative_production, type="l", lwd=3)


#------- Fit curve
# Algorithm for calculating fitted relative production based on soil temp and the 4 coefficients.

calculate_fitted_RP <- function(tsoil, coef_1, coef_2, coef_3, coef_4)
{
  # coef_1 <- moisture_1
  # coef_2 <- moisture_2
  fraction<-(coef_2 - tsoil)/(coef_2- coef_1)

  pred <- ifelse(fraction>0, (exp(coef_3/coef_4*(1-fraction^coef_4))*(fraction^coef_3)), 0)
}

#Non-linear curve fitting with the equaton above.
tsoil_dataframe<-data.frame(out_data$tsoil, out_data$maxrelGPP)
# swc_dataframe<-data.frame(realdat$swc, realdat$maxrelGPP)
names(tsoil_dataframe) <- c("tsoil", "maxrelGPP")


#first use nlsLM to get good parameter starting points
curve.nlslrc = nlsLM(maxrelGPP ~ calculate_fitted_RP(tsoil, coef_1, coef_2, coef_3, coef_4), data = tsoil_dataframe,
                     start = list(coef_1 = moisture_1, coef_2 = moisture_2, coef_3= moisture_3, coef_4= moisture_4),
                     control = list(maxiter = 1024))

coef(curve.nlslrc)  #gives good starting coefs


#reset parameters here: 
moisture_1<-coef(curve.nlslrc)[1]
moisture_2<-coef(curve.nlslrc)[2]
moisture_3<-coef(curve.nlslrc)[3]
moisture_4<-coef(curve.nlslrc)[4]

print(c(moisture_1, moisture_2, moisture_3, moisture_4))

#Calculate fraction here
fraction_val <-(moisture_2 - tsoil_val)/(moisture_2 - moisture_1)
#This is the curve based on the coefficients listed above
Landis_Relative_production<-ifelse(fraction_val>0, (exp(moisture_3/moisture_4*(1-fraction_val^moisture_4))*(fraction_val^moisture_3)), 0)
lines(tsoil_val, Landis_Relative_production, type="l", lwd=3, ylab="Maximum Relative GPP", xlab="Soil temperature", col = "red")


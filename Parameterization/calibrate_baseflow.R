#calibrate baseflow for uscrn sites

#-----------------------------------------------------------------------------
# Pedotransafer functions

peterson <- function(h, clay){
  clay <- clay*100
  if(h == 330){
    theta <- 0.01 * (11.83 + 0.96 * clay - 0.008*(clay^2))
  }
  if(h == 15000){
    theta <- 0.01 * (1.74 + 0.76 * clay - 0.005 * (clay^2))
  }
  
  return(theta)
}


#calculate field capacity and pwp for different models
brookscorey <- function(qr, f, hb, lambda, h){
  if(h <= hb){return(1)}else{
    return((hb/h)^lambda)
  }
}

vangenuchten <- function(qr, qs, a, n, m, h){
  return(1/((1 + (a * h)^n)^m))
}

#-------------------------------------------------------------------------------

library("tidyverse")
library("raster")

orig_wd <- ("C:/Users/Sam/Documents/Research/Isle Royale")
setwd(orig_wd)
test_wd <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/necn_1_cell_test_mc/"

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
  dplyr::select("WBAN_NO", "DATE_TIME", "LONGITUDE", "LATITUDE", "SMVWC_COLUMN") %>%
    mutate(WBAN_NO = as.character(WBAN_NO),
           DATE_TIME = strptime(as.character(DATE_TIME), format = c("%Y%m%d%H")))
  
}

#baseflow is notoriously hard to parameterize. The idea here is that instead of
#parameterizing it frm data, we'll just calibrate the NECN water balance model
#so that it matches empirical data. There are a few sources for soil moisture data:
#USCRN
#Ameriflux sites

#import USCRN data

uscrn_files <- list.files("./Parameterization/necn_parameterize/soil moisture",
                          full.names = TRUE,
                    pattern = "CRNS")
uscrn_swc <- map_df(uscrn_files, .f = read_plus)
site_full_name <- basename(uscrn_files)

sites <- unique(uscrn_swc$WBAN_NO)

uscrn_monthly_swc <- uscrn_swc %>%
  mutate(month = format(DATE_TIME, "%m"),
         year = format(DATE_TIME, "%Y")) %>%
  group_by(WBAN_NO, year, month) %>%
  summarise(swc = mean(SMVWC_COLUMN, na.rm = TRUE),
            year = year[1],
            month = month[1])

#CHANGE for new site
i <- 2
uscrn_site <- uscrn_monthly_swc[uscrn_monthly_swc$WBAN_NO == sites[i], ]

#for climate data
uscrn_monthly_files <- list.files("./Parameterization/necn_parameterize/soil moisture",
                            full.names = TRUE,
                            pattern = "CRNM") %>%
  subset(grepl("txt", .))

headers <- scan("./Parameterization/necn_parameterize/soil moisture/headers_monthly_clim.txt", 
                what = "character",
                skip = 1,
                nlines = 1)

#make sure climate and soil data match!
#TODO figure out what the WBAN_NOs are; we need those to link tables
uscrn_monthly_clim <- read_table(uscrn_monthly_files[i],  col_names = headers) %>%
  rename(WBAN_NO = WBANNO) %>%
  mutate(across(everything(), ~na_if(., "-9999"))) %>%
  mutate(across(everything(), ~as.double(.)))

soil_data <- readxl::read_xls("./Parameterization/necn_parameterize/soil moisture/baseflow_params.xls")
soil_data <- as.data.frame(apply(soil_data, 2, function(y) gsub("±.*","",y)))
soildat <- soil_data[72, ] #CHANGE FOR EACH SITE -- needs to match site_full_name for the site

#Make a one-cell NECN dataset for our test site
# Using code from create_custom_test_cell.R
# We need to change the climate and soils inputs, and perhaps the IC.
# We'll iterate through several values of baseflow and try to calibrate to the observed soil 
# moisture from the USCRN site.


#helper functions
#function to write one-cell rasters (or potentially larger rasters, I guess!)
write_cell <- function(name){
  if(new_folder){
    writeRaster(setValues(template, name), paste0("./", new_folder_name, raster_folder, deparse(substitute(name)), ext),
                overwrite = TRUE)} else{
                  writeRaster(setValues(template, name), 
                              paste0("./", raster_folder, deparse(substitute(name)), ext),
                              format = "GTiff",
                              overwrite = TRUE,
                              datatype = "FLT4S", 
                              NAvalue = 0)
                }
  
}



#####################
# Start here to tune

#some parameters for the script
setwd(test_wd)

#what is the original scenario file name? (should be in the working directory)
scenario <- readLines("Scenario1.txt")
batch <- readLines("Scenario1.bat")

#what is the NECN succession file name? (should be in the working directory)

#what subfolder should the newly created rasters go in?
raster_folder <- "rasters/"
ext <- ".tif"

new_folder <- TRUE
new_folder_name <- "gaylord_baseflow_test/"
if(!new_folder) new_folder_name <- ""

#make the folders if we want to
if(new_folder){
  if(dir.exists(new_folder_name)) print("Writing to existing folder. Pick a new folder name if you want a new folder.") else{
    dir.create(new_folder_name)
    dir.create(paste0(new_folder_name, raster_folder))
  }
}


#ecoregion map
#just one ecoregion for one cell -- make sure that the climate you're using matches!
template <- raster(nrows = 1, ncols = 1, vals = 1) #vals should match the climate region you want to use
if(new_folder){
  writeRaster(template, 
              paste0("./", new_folder_name, raster_folder, "ecoregions", ext), 
              overwrite = TRUE,
              datatype = "INT4S",
              NAvalue = 0)
}

#variables we need:
#Total soil C
#TODO warn or adjust if soil C or soil N compartments will be too large

SOMtot <- 20000 #soildat$`Total C` * mean(soildat$BD...16, soildat$BD...19, soildat$BD...22)
SOM1surfC <- 0.01 * SOMtot
write_cell(SOM1surfC)
SOM1soilC <- 0.02 * SOMtot
write_cell(SOM1soilC)
SOM2C <- 0.59 * SOMtot
write_cell(SOM2C)
SOM3C <- 0.38 * SOMtot
write_cell(SOM3C)

#this doesn't work -- TODO figure out how to write all the rasters at once
#lapply(list(SOM1surfC, SOM1soilC, SOM2C, SOM3C), FUN = write_cell)

#Total soil N (or C:N ratio)
SOM1surfN <- 0.1 * SOM1surfC
SOM1surfN <- 0.5 * SOM1surfN
write_cell(SOM1surfN)
SOM1soilN <- 0.1 * SOM1soilC
SOM1soilN <- 0.5 * SOM1soilN
write_cell(SOM1soilN)
SOM2N <- 0.04 * SOM2C
SOM2N <- 0.5 * SOM2N
write_cell(SOM2N)
SOM3N <- 0.118 * SOM3C
SOM3N <- 0.5 * SOM3N
write_cell(SOM3N)

#Soil Drain
soil_drain <- .66 #CHANGE THIS
write_cell(soil_drain)

#Soil Depth
soil_depth <- 100 #CHANGE THIS
write_cell(soil_depth)

#percent sand
sand <- as.numeric(soildat$Sand)/100 # 0.35 #CHANGE
write_cell(sand)

#percent clay
clay <- as.numeric(soildat$Clay)/100 # 0.04
write_cell(clay)

#Field capacity
# field_capacity <- mean(c(as.numeric(soildat$wc33kpa...15),
#                        as.numeric(soildat$wc33kpa...18),
#                        as.numeric(soildat$wc33kpa...21)), na.rm = TRUE)/100 #  0.36 #CHANGE
# field_capacity <- peterson(330, clay)
# #saxton
# field_capacity <- brookscorey(qr = 0, f = 0.5094, hb = 38.448, lambda = 0.2948, h = 330)
# #rawls 1985
#field_capacity <- brookscorey(qr = 0.0482, f = 0.5094, hb = 4.5981, lambda = 0.4348, h = 330)
# #williams 1992
# field_capacity <- brookscorey(qr = 0, f = 0.5094, hb = 9.629, lambda = 0.2215, h = 330)
field_capacity <- quantile(uscrn_site$swc, 0.99, na.rm = TRUE)
write_cell(field_capacity)

#permanent wilt point
# wilt_point <- mean(c(as.numeric(soildat$wc1500kpa...14),
#                      as.numeric(soildat$wc1500kpa...17),
#                      as.numeric(soildat$wc1500kpa...20)), na.rm = TRUE) / 100 # 0.1 #CHANGE
# wilt_point <- peterson(15000, clay)
# 
# wilt_point <- brookscorey(qr = 0.029, f = 0.5094, hb = 26.2769, lambda = .3755,h = 15000)
# #rawls 1985
#wilt_point <- brookscorey(qr = 0.0482, f = 0.5094, hb = 4.5981, lambda = 0.4348, h = 15000)
# #williams 1992
# wilt_point <- brookscorey(qr = 0, f = 0.5094, hb = 9.629, lambda = 0.2215, h = 15000)
wilt_point <- quantile(uscrn_site$swc, 0.01, na.rm = TRUE)
write_cell(wilt_point)

#baseflow
baseflow <- 0.35 #CHANGE -- TUNING PARAMETER
write_cell(baseflow)

# #stormflow
# #CHANGE THIS
# # See create_soil_and_hyrdo.R. This is based on soil texture, just input the parameters by hand
# from the soil runoff table
soil_runoff_table <- data.frame(
  soil_type = c("Sand", "Loamy sand", "Sandy loam", "Loam", "Silt loam", "Silt", "Sandy clay loam", "Clay loam", "Silty clay loam", "Sandy clay", "Silty clay", "Clay"),
  c0 = c(0.03, 0.07, 0.1, 0.13, 0.17, 0.20, 0.23, 0.27, 0.3, 0.33, 0.37, 0.4),
  s0 = c(0.68, 0.65, 0.62, 0.59, 0.56, 0.53, 0.5, 0.47, 0.44, 0.41, 0.38, 0.35)
)

c0 <- 0.07
s0 <- 0.65
s <- 0 #slope steepness -- get from DEM or SSURGO
stormflow <- c0 + (1 - c0)*(s / (s + s0))
# stormflow <- 0
write_cell(stormflow)

# species biomass cohorts (initial communities)
# you can put your own initial communities here to test
# single cohort growth to calibrate against plantation data, for example
init_comm <- data.frame(MapCode = 1,
                        SpeciesName = "THOC2",
                        CohortAge = 10,
                        CohortBiomass = 10)

# #or import data from existing IC file
# init_comm <- read.csv("C:/Users/Sam/Documents/Research/Isle Royale/Models/LANDIS inputs/NECN files/initial_communities_update.csv") %>%
#   filter(MapCode == 26907)
# init_comm$MapCode <- 1

write.csv(init_comm, paste0("./", new_folder_name, "init_comm.csv"))

initial_communities <- 1
if(new_folder){
  initial_comm_raster <- setValues(template, initial_communities)
  writeRaster(template, 
              paste0("./", new_folder_name, raster_folder, "initial_communities", ext), 
              overwrite = TRUE,
              datatype = "INT4S",
              NAvalue = 0)
}

#   * Dead Wood on the Surface ^3^
dead_wood <- sum(init_comm$CohortBiomass) * 0.37 #"equilibrium" ratio from long LANDIS run
write_cell(dead_wood)

#   * Dead Wood of Coarse Roots ^3^
coarse_roots <- 0.33 * dead_wood #suggestion from Zachary's scripts; TODO replace with root:shoot and live:dead root ratios?
write_cell(coarse_roots)

#import the text files needed to run NECN
# from a template file provided in the directory
NECN_succession <- readLines("NECN_succession.txt")

#change/write NECN variables
NECN_succession[30:45] <- c("CalibrateMode yes",
                            "SmokeModelOutputs no",
                            "Version_Henne_SoilWater no",
                            "WaterDecayFunction Ratio <<Linear or Ratio",
                            "",
                            "ProbabilityEstablishAdjust 	1.0",
                            "InitialMineralN			1.0",
                            "InitialFineFuels		0.75",
                            "AtmosphericNSlope		-0.000109",
                            "AtmosphericNIntercept		0.0589",
                            "Latitude			48",
                            "DenitrificationRate		0.25 <<was 0.5",
                            "DecayRateSurf			0.88",
                            "DecayRateSOM1			0.95 << increased from 0.9 sf 2021-12-8",
                            "DecayRateSOM2			0.02 << changed back to 0.02 sf 2021-12-8 <<0.06 << Was 0.02 from Louise. changed 2/5",
                            "DecayRateSOM3			0.0002")

write(NECN_succession, file = paste0("./", new_folder_name, "NECN_succession.txt"))

#make the initial communities pointer file -- make sure this matches the name 
#of the initial communities csv file
#TODO have script check the NECN_succession file to make sure it knows the name of this text file
initial_communities_pointer <- c("LandisData	\"Initial Communities\"",
                                 "CSV_File \"init_comm.csv\"")
write(initial_communities_pointer, file = paste0("./", new_folder_name, "initial_communities_pointer.txt"))

#-----------------------------------
# Climate
#import climate from the input files
clim_reform <- uscrn_monthly_clim %>%
  mutate(TIMESTEP = lubridate::ym(as.character(LST_YRMO)),
         zeros = 0) %>%
  dplyr::filter(TIMESTEP >= as.POSIXct("2008-01-01", format = "%Y-%m-%d")) %>% #CHANGE DATES to match soil water data
  dplyr::filter(TIMESTEP <= as.POSIXct("2021-12-31", format = "%Y-%m-%d")) %>%
  mutate(TIMESTEP = paste0(as.character(TIMESTEP), "T00:00:00Z"))

first_year <- 2008

#names for LANDIS
var_rows <- c("#ppt",
              "#Tmax",
              "#Tmin")
#names from USCRN
var_columns <- c("P_MONTHLY_CALC",
                 "T_MONTHLY_MAX",
                 "T_MONTHLY_MIN")

units_means <- c("mm/month",
                 "C",
                 "C")
units_variance <- c("mm/d^2",
                    "C^2",
                    "C^2")
TIMESTEP <-character()
means <- character()
variances <-character()
stdev <- character()
for(i in 1:length(var_rows)){
  #the first column has timesteps but also the headers for each variable followed by a blank cell
  TIMESTEP <- c(TIMESTEP, var_rows[i], "", "TIMESTEP", clim_reform$TIMESTEP)
  
  #make each column separately, to avoid having to rbind a bunch of garbage
  #each column grows in length for each variable
  #if you had more ecoregions this would be a bad way to do this
  means <- c(means, "", "eco1", paste0("MEAN(", units_means[i], ")"), clim_reform[[var_columns[i]]])
  variances <- c(variances, "", "eco1", paste0("VARIANCE(", units_variance[i], ")"), clim_reform$zeros)
  stdev <- c(stdev, "", "eco1", paste0("STD_DEV(", units_means[i], ")"), clim_reform$zeros)
}

means <- ifelse(!is.na(means), means, 0)

output_data <- cbind(TIMESTEP, means, variances, stdev)

climate_filename <- "uscrn_clim.csv"
write.table(output_data,               # Write CSV file without header
            paste0("./", new_folder_name, climate_filename),
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE) # quote = false is important! Otherwise the CL can't read the file, 
# but it won't be apparent looking at the data in Excel


climate_generator_filename <- "climate-generator-gridmet.txt"
climate_generator <- readLines(climate_generator_filename)

climate_generator[3] <- paste0("ClimateTimeSeries\t\t\tMonthly_SequencedYears")
climate_generator[4] <- paste0("ClimateFile \"", climate_filename, "\"")
climate_generator[5] <- "ClimateFileFormat\t\t\tMonthly_Temp-C_Precip-mmMonth" 
climate_generator[7] <- paste0("SpinUpClimateTimeSeries\t\t\tMonthly_SequencedYears")
climate_generator[8] <- paste0("SpinUpClimateFile \"", climate_filename, "\"")
climate_generator[9] <-"SpinUpClimateFileFormat\t\t\tMonthly_Temp-C_Precip-mmMonth"
write(climate_generator, file = paste0("./", new_folder_name, climate_generator_filename))


#should species information be copied over?
copy_species <- TRUE
species_filename <- "species.txt"
necn_species_filename <- "NECN_Spp_Table_inv.csv"
functional_filename <- "NECN_Functional_Table_inv_moisture.csv"
# species
if(new_folder){
  spp <- readLines(species_filename)
  necn_spp <- read.csv(necn_species_filename)
  func <- read.csv(functional_filename)
  write(spp, file = paste0("./", new_folder_name, species_filename), sep = "tab")
  write.csv(necn_spp, paste0("./", new_folder_name, necn_species_filename))
  write.csv(func, paste0("./", new_folder_name, functional_filename))
}

#copy over the scenario file
writeLines(scenario, paste0("./", new_folder_name,"Scenario1.txt"))

#copy over the ecoregions file
ecoregions <- readLines("ecoregions.txt")
writeLines(ecoregions, paste0("./", new_folder_name,"ecoregions.txt"))

readme <- "Test with baseflow = 0.01"
write(readme, file = paste0("./", new_folder_name, "readme.txt"))


#make the batchfile and optionally run it
batch <- c("call landis-ii-7 Scenario1.txt", "pause")
write(batch, file = paste0("./", new_folder_name, "Scenario1.bat"))
setwd(new_folder_name)
shell.exec("Scenario1.bat")


Sys.sleep(30)

################################################################################
## Compare simulated to observed soil moisture
setwd(orig_wd)

sim_moisture <- read.csv(paste0("./Models/landis_test/necn_1_cell_test_mc/", new_folder_name, "/NECN-succession-monthly-log.csv"))
sim_moisture$Year <- sim_moisture$Time + as.numeric(first_year) - 1 #CHANGE TO MATCH INPUTS -- add one less than desired year
sim_moisture$Month <- ifelse(sim_moisture$Month < 10, paste0("0", sim_moisture$Month), sim_moisture$Month)
sim_moisture$time <- lubridate::parse_date_time(as.character(paste0(sim_moisture$Year, sim_moisture$Month)), "ym")
sim_moisture$vwc <- ifelse(sim_moisture$SoilWaterContent > 0, sim_moisture$SoilWaterContent/soil_depth, 0)
sim_moisture$avail_wat_landis <- ifelse(sim_moisture$vwc > field_capacity, field_capacity,
                                        ifelse(sim_moisture$vwc < wilt_point, 0,
                                               sim_moisture$vwc - wilt_point))

mean(sim_moisture$vwc)

uscrn_site$avail_wat_uscrn <- ifelse(uscrn_site$swc > field_capacity, field_capacity,
                                    ifelse(uscrn_site$swc < wilt_point, 0,
                                           uscrn_site$swc - wilt_point))

mean(uscrn_site$swc, na.rm = TRUE)

uscrn_site$time <- as.POSIXct(as.Date(paste0(uscrn_site$year, "-", uscrn_site$month, "-01 UTC")))
uscrn_site <- uscrn_site[uscrn_site$time %in% sim_moisture$time, ]
sim_moisture <- sim_moisture %>%
  filter(sim_moisture$time %in% uscrn_site$time)

test <- left_join(uscrn_site, sim_moisture, by = c("time"))


plot(sim_moisture$vwc ~ sim_moisture$time)
plot(uscrn_site$swc ~ uscrn_site$time)

hist(test$swc)
hist(sim_moisture$vwc)

plot(test$swc ~ test$vwc)
abline(0,1)

plot(test$avail_wat_landis ~ test$avail_wat_uscrn)
abline(0,1)


test_annual <- test %>%
  group_by(Year) %>%
  summarise(mean_swc_uscrn = mean(swc, na.rm = TRUE),
            mean_swc_landis = mean(vwc, na.rm = TRUE))
plot(test_annual$mean_swc_landis ~ test_annual$mean_swc_uscrn)
abline(0,1)

test_annual <- test %>%
  group_by(Year) %>%
  summarise(mean_swc_uscrn = mean(avail_wat_uscrn, na.rm = TRUE),
            mean_swc_landis = mean(avail_wat_landis, na.rm = TRUE))
plot(test_annual$mean_swc_landis ~ test_annual$mean_swc_uscrn)
abline(0,1)




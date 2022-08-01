library(terra)

biomass_init <- rast("./Models/landis_test/no browse cc/NECN/TotalC-5.img")

biomass1 <- rast("./Models/landis_test/no browse cc/NECN/TotalC-25.img")
plot(biomass1)

biomass2 <-  rast("./Models/landis_test/no browse no cc/NECN/TotalC-25.img")
plot(biomass2)

biomassdiff <- biomass1 - biomass2
NAflag(biomassdiff) <- 0
plot(biomassdiff)

biomass_change <- biomass1 - biomass_init

NAflag(biomass_change) <- 0

plot(biomass_change)



#climate data 

clim_hist <- read.csv("./Models/LANDIS inputs/NECN files/historical_gridmet.csv")
plot(as.numeric(clim_hist[3:10000, 2]) ~ as.POSIXct(clim_hist[3:10000, 1]))

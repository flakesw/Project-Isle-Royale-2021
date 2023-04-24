#calibrate anpp

#import from GEE
npp_gee <- rast("./Calibration/Calibration_data/average_monthly_npp_isro.tif")
plot(npp_gee)

template <- rast("./Models/LANDIS inputs/input rasters/ecoregions.tif")
anpp <- rast("./Models/landis_test/mc_test - linear/NECN/AG_NPP-80.img")
anpp_2 <- template
values(anpp_2) <- values(anpp)
plot(anpp_2)
NAflag(anpp_2) <- 0


npp_gee <- terra::project(npp_gee, anpp_2)
npp_gee <- terra::crop(npp_gee, anpp_2) %>% mask(anpp_2)
plot(npp_gee)
npp_annual <- sum(npp_gee)/10
plot(npp_annual)
plot(anpp_2)

plot(values(npp_annual) ~ values(anpp_2))
abline(0,1)

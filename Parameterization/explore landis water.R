#translate LANDIS water to volumetric water

water_cm_rasters <- list.files("./Models/landis_test/browse historical spinup - reduce est - anpp066 - fix soil/necn",
                               full.names = TRUE) %>%
  `[`(grepl("AvailableWater", .))

water_stack <- rast(water_cm_rasters)
plot(water_stack)

soil_depth <- rast("./Models/LANDIS Inputs/input rasters/soil_depth.tif")
soil_depth_landis <- water_stack[[1]]
values(soil_depth_landis) <- values(soil_depth)

water_vol <- water_stack/soil_depth_landis

water_test <- water_stack/100

plot(water_vol[[1]])
plot(water_test[[1]])
hist(values(water_test[[1]])[values(water_test[[1]])>0])

ecoregions <- rast("./Models/LANDIS INputs/input rasters/ecoregions_inv.tif")
plot(ecoregions)


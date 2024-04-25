#calibrate volumetric water
library(terra)
soil_water_cm <- rast("./Models/landis_test/mc_test/NECN/SoilWater-80.img")
mean(values(soil_water_cm)[values(soil_water_cm) > 0])
plot(soil_water_cm)
hist(values(soil_water_cm)[values(soil_water_cm) > 0])

avail_water <- rast("./Models/landis_test/mc_test/NECN/AvailableWater-80.img")
mean(values(avail_water)[values(avail_water) > 0])
plot(avail_water)
hist(values(avail_water)[values(avail_water) > 0])



fc <- rast("./Models/LANDIS inputs/input rasters/field_capacity.tif")
soil_depth <- rast("./Models/LANDIS inputs/input rasters/soil_depth.tif")
plot(soil_depth)
pwp <- rast("./Models/LANDIS inputs/input rasters/wilt_point.tif")
water_empty <- pwp*soil_depth
plot(water_empty)
water_full <- fc*soil_depth
plot(water_full)

stormflow <- rast("./Models/LANDIS inputs/input rasters/stormflow.tif")
plot(stormflow)
baseflow <- rast("./Models/LANDIS inputs/input rasters/baseflow.tif")
plot(baseflow)

vol_water <- soil_depth
values(vol_water) <- values(soil_water_cm)/values(soil_depth)
hist(values(vol_water)[values(vol_water) != 0])

plot(classify(vol_water, c(-5,0.001,0.03,1)))
plot(fc)

water_def <- fc - vol_water
plot(water_def)
water_surp <- vol_water - pwp
plot(water_surp)

test <- pwp
values(test) <- values(avail_water) - values(soil_water_cm)
plot(test)

plot(values(vol_water) ~ values(stormflow))


drain <- stormflow <- rast("./Models/LANDIS inputs/input rasters/soil_drain.tif")
plot(drain)

plot(values(vol_water) ~ values(drain))












soilWater = 0.0500459707733302
frac = 0.000656725333288342

  A1 = 0.12
  A2 = 0.05
  A3 = 131.0959
  A4 = 0.0011934

frac = (A2 - soilWater) / (A2 - A1);
waterLimit = exp(A3 / A4 * (1.0 - frac^A4)) * frac^A3

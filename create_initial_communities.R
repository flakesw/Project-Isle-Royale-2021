#create new initial communities
library("raster")
library("tidyverse")

orig_ic <- raster("./calibration_data/initial communities/init_comm_052318.img")

plot(orig_ic)

ecoreg <- raster("./calibration_data/ecoregions/ecoregions_09132021.tif")

pop_zone <- raster("./calibration_data/browse/pop_zone_052318.img")

values(orig_ic)[values(ecoreg) == 3] <- 135
values(orig_ic)[values(ecoreg) == 6] <- 134
values(orig_ic)[values(ecoreg) %in% c(0,1)] <- 0

writeRaster(orig_ic, filename = "./calibration_data/initial communities/ic_2021-09-16_test.tif",
            datatype = "INT2S", overwrite = TRUE)

ecoreg_updated <- ecoreg
values(ecoreg_updated)[values(orig_ic) == 0 & values(ecoreg_updated) != 1] <- 0

plot(ecoreg_updated - ecoreg)

writeRaster(ecoreg_updated, filename = "./calibration_data/ecoregions/ecoregions_test.tif",
            datatype = "INT2S", overwrite = TRUE)

# fix the pop_zone raster.
# It looks like I just made a new one? Need to figure out what I ended up doing

# pop_zone_new <- reclassify(orig_ic, c(0, 0, 0, 0, 135, 1))
# 
# writeRaster(pop_zone_new, "./calibration_data/browse/pop_zone_2021-09-16.tif",
#             datatype = "INT2S", overwrite = TRUE)


#To make the initial communities, we need to get cohort data with biomass for
#each cell. We can do this with FIA data.

fia_raster <- raster("./calibration_data/treemap/RDS-2019-0026_Data/Data/national_c2014_tree_list.tif")


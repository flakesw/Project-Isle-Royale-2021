# add ecoregreions for water, herbaceous wetlands, woody wetlands
# ecoregions for biomass succession
# This script demonstrates how to take an existing ecoregion file
# and add new ecoregions to what was previously NA ecoregions

library("raster")
library("tidyverse")

ecoregions <- raster("./calibration_data/ecoregions_051718.img")

nlcd <- raster("./calibration_data/nlcd/nlcd_clipped_isro.tif") %>%
        projectRaster(ecoregions, method = "ngb")

#set water to ecoregion 1 (inactive)
ecoregions[values(nlcd) == 11] <- 1

#set areas that are NA ecoregion and wetland on NLCD to appropriate ecoregion
ecoregions[values(ecoregions) == 0 & values(nlcd) == 90] <- 6
ecoregions[values(ecoregions) == 0 & values(nlcd) == 95] <- 3

plot(ecoregions)
table(values(ecoregions)) #check that the values seem reasonable and there are no NAs

#make sure to specify an integer when writing
writeRaster(ecoregions, filename = "./calibration_data/ecoregions_09132021.tif",
            filetype = "GTiff", datatype = "INT2S", overwrite = TRUE)




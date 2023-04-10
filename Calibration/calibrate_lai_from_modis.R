# compare LAI from LANDIS and MODIS

library("terra")
library("tidyverse")


project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file
  out_raster <- template
  values(out_raster) <- values(input_raster)
  
  return(out_raster)
}

template <- rast("./Models/LANDIS inputs/input rasters/ecoregions.tif")

lai_landis <- rast("./Models/landis_test/browse historical spinup - nobrowse/NECN/LAI-5.img") %>%
  project_to_template(template) %>%
  terra::classify(., c(0, NA)) #%>%
  # terra::project(lai_modis, method = "bilinear", mask = TRUE)
hist(values(lai_landis))

lai_modis <- rast("./Calibration/Calibration_data/average_monthly_LAI_isro.tif")%>%
#  terra::project(lai_landis, method = "bilinear", mask = TRUE) %>%
  `/`(10) %>%
  `[[`(8) #select August
hist(values(lai_modis))






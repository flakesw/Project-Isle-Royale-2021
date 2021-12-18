#testing an issue, resolved with patch by Rob
#randomizes ecoregions and initial communities
library(raster)

setwd("C:/Users/Sam/Documents/Research/Isle Royale/Isle Royale from Nate/Isle Royale LANDIS/A1_P0 - Copy/A")


eco_test <- raster("ecoregions_051718.img")
dataType(eco_test)
values(eco_test) <- sample(c(0,2,4,5), size = ncell(eco_test), replace = TRUE)
writeRaster(eco_test, "ecoregion_random_test.tif", datatype = "INT2S", overwrite = TRUE)
                         
ic_test <- raster("init_comm_052318.img")
dataType(ic_test)
values(ic_test) <- floor(runif(ncell(ic_test), 1, 134))
values(ic_test)[10000] <- 0
writeRaster(ic_test, "ic_random_test.tif", datatype = "INT2S", overwrite = TRUE)

setwd("C:/Users/Sam/Documents/Research/LANDIS training/Biomass v6 example/CoreV7.0-BiomassSuccession6.0/CoreV7.0-BiomassSuccession6.0")
test2 <- raster("ecoregions_s2e1.gis")
values(test2) <- sample(c(0,1,2), size = ncell(test2), replace = TRUE)
writeRaster(test2, "eco_test.tif", datatype = "INT2S", overwrite = TRUE)

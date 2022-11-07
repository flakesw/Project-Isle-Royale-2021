library("raster")
library("tidyverse")

in_folder <- "./Models/landis_test/small landscape test - linear/spp-bio-age"

project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file
  
  if(input_raster@ncols == template@ncols & input_raster@nrows == template@nrows){
    #replace values of template with values from input raster
    out_raster <- template %>%
      `values<-`(values(input_raster))
  } else print("Rasters have different numbers of rows or cols")
  
  # plot(out_raster)
  
  return(out_raster)
}

if(!dir.exists(paste0(in_folder, "/projected"))){
  dir.create(paste0(in_folder, "/projected"))
}

template <- raster("./Models/LANDIS inputs/input rasters subset east/ecoregions_subset.tif")

raster_list <- list.files(in_folder, full.names = TRUE)
raster_list <- raster_list[extension(raster_list) %in% c(".img", ".tif", ".gis")]

#select what kind of raster, if not all in folder
# raster_list <- raster_list[grepl("SiteForage", raster_list)]

rasters_stripped <- sub('\\..*$', '', basename(raster_list))

for(i in 1:length(raster_list)){
  oldrast <- raster(raster_list[i])
  newrast <- project_to_template(oldrast, template)
  writeRaster(newrast, 
              paste0(in_folder,"/projected/",rasters_stripped[i], ".tif"),
              filetype = "GEOTiff",
              datatype = dataType(oldrast),
              overwrite = TRUE)
}

#main effects maps

project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file
  
  #replace values of template with values from input raster
  out_raster <- template %>%
    `values<-`(values(input_raster))
  
  
  return(out_raster)
}

template <- terra::rast("./Models/LANDIS inputs/input rasters/initial_communities.tif")

orig_totalc <- terra::rast("./Models/Model templates/browse miroc/NECN/TotalC-5.img") %>%
  project_to_template(template)
miroc_nobrowse_totalc <- terra::rast("./Models/Model templates/no browse miroc/NECN/TotalC-80.img")%>%
  project_to_template(template)
hist_nobrowse_totalc <- terra::rast("./Models/Model templates/no browse historical/NECN/TotalC-80.img") %>%
  project_to_template(template)
change_hist <- hist_nobrowse_totalc - orig_totalc
change_miroc <- miroc_nobrowse_totalc - orig_totalc

writeRaster(orig_totalc/100, "orig_totalc2.tif", overwrite = TRUE)
writeRaster(miroc_browse_totalc, "browse_totalc_80.tif")
writeRaster(miroc_nobrowse_totalc/100, "nobrowse_totalc_80.tif", overwrite = TRUE)
writeRaster(hist_nobrowse_totalc/100, "hist_nobrowse_totalc_80.tif", overwrite = TRUE)

writeRaster(change_hist/100, "change_totalc_hist2.tif", overwrite = TRUE)
writeRaster(change_miroc/100, "change_totalc_miroc2.tif", overwrite = TRUE)

miroc_browse_change <- miroc_browse_totalc - orig_totalc
NAflag(miroc_browse_change) <- 0
plot(miroc_browse_change)

miroc_nobrowse_change <- miroc_nobrowse_totalc - orig_totalc
NAflag(miroc_nobrowse_change) <- 0
plot(miroc_nobrowse_change)

miroc_browsediff <- miroc_browse_change - miroc_nobrowse_change
NAflag(miroc_browsediff) <- 0
plot(miroc_browsediff)

miroc_browsediff <- project_to_template(miroc_browsediff, template)/100
writeRaster(miroc_browsediff, "soilc_moose_browse_climate_change_difference.tif",
            overwrite = TRUE)

#AGB
orig_totalc <- terra::rast("./Models/Model templates/browse miroc/biomass/TotalBiomass-0.img")
miroc_browse_agb <- terra::rast("./Models/Model templates/browse miroc/biomass/TotalBiomass-80.img")
miroc_nobrowse_agb <- terra::rast("./Models/Model templates/no browse miroc/biomass/TotalBiomass-80.img")

miroc_browse_change <- miroc_browse_agb - orig_totalc
NAflag(miroc_browse_change) <- 0
plot(miroc_browse_change)

miroc_nobrowse_change <- miroc_nobrowse_agb - orig_totalc
NAflag(miroc_nobrowse_change) <- 0
plot(miroc_nobrowse_change)

miroc_browsediff <- miroc_browse_change - miroc_nobrowse_change
NAflag(miroc_browsediff) <- 0
plot(miroc_browsediff)

miroc_browsediff <- project_to_template(miroc_browsediff, template)/100
writeRaster(miroc_browsediff, "agb_moose_browse_climate_change_difference.tif", overwrite = TRUE)

#soil N
# orig_totaln <- terra::rast("./Models/Model templates/browse miroc/NECN/SoilN-5.img")
# miroc_browse_totaln <- terra::rast("./Models/Model templates/browse miroc/NECN/SoilN-45.img")
# miroc_nobrowse_totaln <- terra::rast("./Models/Model templates/no browse miroc/NECN/SoilN-45.img")
# 
# miroc_browse_change <- miroc_browse_totaln - orig_totaln
# NAflag(miroc_browse_change) <- 0
# plot(miroc_browse_change)
# 
# miroc_nobrowse_change <- miroc_nobrowse_totaln - orig_totaln
# NAflag(miroc_nobrowse_change) <- 0
# plot(miroc_nobrowse_change)
# 
# miroc_browsediff <- miroc_browse_change - miroc_nobrowse_change
# NAflag(miroc_browsediff) <- 0
# plot(miroc_browsediff)
# 
# miroc_browsediff <- project_to_template(miroc_browsediff, template)
# writeRaster(miroc_browsediff, "agb_moose_browse_climate_change_difference.tif", overwrite = TRUE)

#climate change effect on C


orig_totalc <- terra::rast("./Models/Model templates/browse miroc/NECN/TotalC-5.img")
historical_nobrowse_totalc <- terra::rast("./Models/Model templates/no browse historical/NECN/TotalC-80.img")
miroc_nobrowse_totalc <- terra::rast("./Models/Model templates/no browse miroc/NECN/TotalC-80.img")

cc_totalc_diff <- miroc_nobrowse_totalc - historical_nobrowse_totalc
NAflag(cc_totalc_diff) <- 0
plot(cc_totalc_diff)

cc_totalc_diff <- project_to_template(cc_totalc_diff, template)/100
writeRaster(cc_totalc_diff, "totalc_climate_change_difference.tif", overwrite = TRUE)


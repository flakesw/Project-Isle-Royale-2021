library("geoknife")

all_webdata <- query("webdata")

fabric <- webdata("prism")
query(fabric, "variables")

variables(fabric) <- "ppt"

query(fabric, "times")

testjob <- geoknife(stencil, evap_fabric, knife)
check(testjob)


################ example
library("geoknife")
evap_fabric_info <- list(times = as.POSIXct(c("2005-01-01", "2015-01-01")),
                         variables = "et",
                         url = 'https://cida.usgs.gov/thredds/dodsC/ssebopeta/yearly')
evap_fabric <- webdata(evap_fabric_info)

# create stencil
evap_stencil <- webgeom('state::Indiana')

# create knife (which defaults to weighted)
evap_knife <- webprocess()
# find unweighted algorithm
all_algorithms <- query(evap_knife, 'algorithms')
unw_algorithm <- all_algorithms[grep('unweighted', names(all_algorithms))]
# set knife algorithm to unweighted
algorithm(evap_knife) <- unw_algorithm

# create the geojob
evap_geojob <- geoknife(evap_stencil, evap_fabric, evap_knife)
check(evap_geojob)


#------------------------------



library(tidyverse)
library(lubridate)
library(geoknife)

rm(list = ls())
all_webdata <- query("webdata")
evap_fabric <- webdata(all_webdata[314])
evap_fabric@times <- as.POSIXct(c('2000-01-01','2022-05-31'))
evap_fabric@variables <- 'et'

get_stencil <- function(fips_code) {
  county_stencil <- webgeom()
  geom(county_stencil) <- 'sample:Counties'
  query(county_stencil, "attributes")
  attribute(county_stencil) <- "FIPS"
  county_values <- query(county_stencil, "values")
  county_index <- which(county_values == fips_code)[[1]]
  values(county_stencil) <- county_values[c(county_index)]
  return(county_stencil)
}

bayco_stencil <- get_stencil("12005")

evap_knife <- webprocess()
all_algorithms <- query(evap_knife, 'algorithms')
wtd_algorithm <- all_algorithms["Area Grid Statistics (weighted)"]
algorithm(evap_knife) <- wtd_algorithm

bayco_geojob <- geoknife(bayco_stencil, evap_fabric, evap_knife, REQUIRE_FULL_COVERAGE = FALSE)

check(bayco_geojob)
bayco_df <- result(bayco_geojob)



#-------------------------
library(geoknife)

fabric <- webdata(url = 'https://cida.usgs.gov/thredds/dodsC/prism')
fabric
query(fabric, 'variables')
variables(fabric) <- 'ppt'

query(fabric, 'times')
times(fabric) <- as.POSIXct(c("1985-01-01","1992-01-01"))

# use ALL variables
variables(fabric) <- query(fabric, 'variables')

stencil <- simplegeom(data.frame('point1'=c(-89, 46), 'point2'=c(-78.6, 42.2)))

knife = webprocess(algorithm = list('OPeNDAP Subset'="gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"))

job <- geoknife(stencil,fabric, knife, wait = TRUE, OUTPUT_TYPE="netcdf")



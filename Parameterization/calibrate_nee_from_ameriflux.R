#get moisture curves from processed ameriflux data
#see process_ameriflux_gpp for processing steps
library("tidyverse")
library("minpack.lm")

#edited csv
sites_to_use <- read.csv("./Parameterization/Parameterization data/ameriflux data/ameriflux_gpp_swc_edit.csv") %>%
  filter(Use)

site_files <- paste0("./Parameterization/necn_parameterize/ameriflux_processed/", sites_to_use$SITE_ID, "_processed.csv")


#some helper functions
read_plus <- function(flnm) {
  print(flnm)
  
  read_csv(flnm) %>%
    group_by(YEAR, MONTH) %>%
    summarise(NEE = mean(c(NEE), na.rm = TRUE),
              gpp = mean(c(GPP_PI_F), na.rm = TRUE),
              site = str_split(basename(flnm), "_")[[1]][1]) %>%
    mutate(NEE_g = NEE * 2.628e+6 * 1e-6 * 12.011) %>% #convert from per seconds to per month, convert from umol to mol, convert from mol to g
    ungroup() %>%
    group_by(site)
}

ft <- unique(sites_to_use$Functional.group)
ft
#which of the sites should we use?
#note: for facultative wet conifer, used data from wet and dry conifers
#shrubs use data for aspen, northern hardwoods, mesic warm conifers, temperate hardwoods, dry/cold pines
i <- which(sites_to_use$Functional.group %in% ft[c(4)])
# i <- c(1, 18, 19, 20, 23)
nee_monthly <- site_files[i]  %>%
  purrr::map_df(~read_plus(.))

boxplot(nee_monthly$NEE_g ~ nee_monthly$MONTH)

necn_monthly <- read.csv("C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/mc_test/NECN-succession-monthly-log.csv") %>%
  group_by(Month) %>%
  summarise(NEE = mean(avgNEE))
lines(necn_monthly$NEE ~ necn_monthly$Month)


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
i <- which(sites_to_use$Functional.group %in% ft[c(1)])
# i <- c(1, 18, 19, 20, 23)
nee_monthly <- site_files[i]  %>%
  purrr::map_df(~read_plus(.))

boxplot(nee_monthly$NEE_g ~ nee_monthly$MONTH)

necn_monthly <- read.csv("C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/mc_test - one_cell/NECN-succession-monthly-log.csv") %>%
  group_by(Month) %>%
  summarise(NEE = mean(avgNEE))
lines(necn_monthly$NEE ~ necn_monthly$Month)

necn_monthly <- read.csv("./Models/Model templates/spinup model - final - Copy (2)/NECN-succession-monthly-log.csv")
# necn_monthly <- read.csv("E:/ISRO LANDIS/Model runs/current - pred1/NECN-succession-monthly-log.csv")
boxplot(necn_monthly$avgNEE ~ necn_monthly$Month)
boxplot(necn_monthly$AvgTotalNPP_C ~ necn_monthly$Month)

test <- necn_monthly %>%
  group_by(Month) %>%
  summarise(mean_NEE = mean(avgNEE),
            mean_NPP = mean(AvgTotalNPP_C))
boxplot(test$mean_NPP)
test <- necn_monthly %>%
  filter(Time>10) %>%
  group_by(Time) %>%
  summarise(total_NEE = sum(avgNEE),
            total_NPP = sum(AvgTotalNPP_C))
mean(test$total_NPP)
mean(test$total_NEE)

#v5 -- mean npp 439; slight biomass and soil C gain
  #normal NPP and maxbio
#v5 - copy -- mean npp 508; gain 1000 soil c and 1800 biomass
  #slightly higher npp, normal maxbio
#v5 - copy2 -- mean npp 738; gain 1900 soil C and 3000 biomass
 #double NPP, normal maxbio
#v5 copy 3 -- mean npp 713; gain 2400 soil c and 50 biomass
  #double npp, half maxbio
#v5 copy 4 -- mean npp 726; gain 2000 soil c and 1800 biomass
  #double npp, lower maxbio
#v5 copy 5 -- mean npp 755; gain 1700 soil c and 2200 biomass
  #double npp, higher maxbio
#v5 copy 6 -- mean npp 517; gain gain 200 soil c and 3200 biomass
  #higher npp, double maxbio

#test7 -- adjust up NPP, no change to maxbio
#test8 -- adjust up maxbio, no change to npp
#test9 -- adjust up NPP, adjust down maxbio
#test10 -- start from test3, reduce npp; reduce npp again



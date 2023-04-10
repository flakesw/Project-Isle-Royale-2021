#explore browse and forage amounts
library("tidyverse")
library("terra")

project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file
  
    out_raster <- template %>%
      `values<-`(values(input_raster))
  
  # plot(out_raster)
  
  return(out_raster)
}

template <- rast("./Models/LANDIS inputs/input rasters/ecoregions.tif")

browse <- rast("./Models/landis_test/mc_test - browse/browse/SiteForage_40.gis")
plot(browse)
crs(browse) <- ""
hist(values(browse)[values(browse) > 0])
mean(values(browse)[values(browse) > 0])

biomass <- rast("./Models/landis_test/mc_test - browse/biomass/bio2-TotalBiomass-40.img")
plot(biomass)

test <- browse/biomass
test <- clamp(test, upper = 0.005)
hist(values(test))
plot(test)

biomass <- rast("./Models/landis_test/mc_test - browse/biomass/bio2-QURU-40.img")
plot(biomass)



#hodgson data, from Table 1
hodgson <- read.csv("./Parameterization/Parameterization data/browse/hodgson_data.csv")

sum <- hodgson %>%
  mutate(m = as.numeric(m)) %>%
  group_by(Location, Year) %>%
  summarise(browse = sum(m, na.rm = TRUE))
sum

browse_landis <- read.csv("./Models/landis_test/mc_test - browse - linear - annual - subset/browse-summary-log.csv") %>%
  mutate(Location = "Landis",
         browse = MeanForage)
browse_landis$Year = browse_landis$Time + 1998
sum <- bind_rows(sum, browse_landis[browse_landis$Year %in% sum$Year, c("Location", "Year", "browse")])
means <- sum %>% group_by(Location) %>% summarise(mean = mean(browse))

ggplot(sum, mapping = aes(x = Year, y = browse, colour = Location)) +
  geom_point() + 
  geom_line()


#pull out forage data for two basins
shapes <- sf::st_read("./Parameterization/Parameterization data/browse/loc_shape.shp")
shapes$id <- c("Moskey Basin", "Lane Cove")
# plot(shapes)

forage_list <- paste0("./Models/landis_test/mc_test - browse - linear - annual/browse/SiteForage_",
                      seq(1,20), #seq(1, 9), 
                      ".gis")
forage <- rast(forage_list)
forage2 <- rast(lapply(forage, FUN = function(x) project_to_template(x, template)))
forage2 <- subst(forage2, 0, NA)
forage_extract <- terra::extract(forage2, vect(shapes), fun = function(x) mean(x, na.rm = TRUE))

forage_extract
names(forage_extract) <- c("Location", paste0("Year", seq(1, 20))) #seq(1999,2007)
forage_extract[, 1] <- c("Moskey Basin Landis", "Lane Cove Landis")

forage_extract2 <- pivot_longer(forage_extract, 
                               cols = contains("Year"),
                               names_to = "Year",
                               names_pattern = "Year(.*)",
                               values_to = "browse") %>%
  mutate(Year = as.numeric(Year)) %>%
 mutate(Year = as.numeric(Year) + 1998)

all_browse <- bind_rows(sum, forage_extract2)
all_browse$Location2 <- ifelse(all_browse$Location %in% c("Lane Cove", "Lane Cove Landis"),
                               "Lane Cove",
                               "Moskey Basin")
all_browse$Source <- ifelse(grepl("Landis", all_browse$Location),
                            "LANDIS",
                            "Observed")

ggplot(all_browse[all_browse$Location != "Landis", ], mapping = aes(x = Year, y = browse, colour = Location2, linetype = Source, shape = Source)) +
  geom_point() + 
  geom_line() + 
  ylab(label = expression(paste("Available forage (g ", m^{-2}, ")")))

means2 <- all_browse %>%
  filter(Source == "Observed" | Year > 2005) %>%
  group_by(Location2, Source) %>%
  summarise(mean_forage = mean(browse))
means2


##compare species percent of diet
spp_log <- read.csv("./Models/landis_test/mc_test - browse - linear - annual - subset/browse-event-species-log.csv")
spp_log_avg <- spp_log %>%
  mutate(SpeciesName = gsub(" ", "", .$SpeciesName)) %>%
  filter(Time < 40) %>%
  group_by(Time) %>%
  mutate(ProportionTotalBrowse = AverageBiomassBrowsed / sum(AverageBiomassBrowsed))
spp_log_avg

spp_log_avg$Type <- ifelse(spp_log_avg$SpeciesName == "ABBA", "Balsam Fir",
                           ifelse(spp_log_avg$SpeciesName %in% c("POTR5", "POBA2", "POGR4"), "Aspen",
                           "Other Deciduous"))
spp_log_avg_type <- spp_log_avg %>%
  group_by(Time, Type) %>%
  summarise(ProportionTotalBrowse = sum(ProportionTotalBrowse, na.rm = TRUE))

spp_log_avg_summary <- spp_log_avg %>%
  group_by(Time, Type) %>%
  summarise(mean_prop = sum(ProportionTotalBrowse, na.rm = TRUE)) %>%
  group_by(Type) %>%
  summarise(mean_prop = mean(mean_prop))



#proportion of diet over time
library("directlabels")
ggplot(spp_log_avg, aes(x = Time, y = ProportionTotalBrowse, colour = SpeciesName)) + 
  geom_line() + 
  scale_colour_discrete(guide = 'none') +
  scale_x_continuous(limits = c(1, 7), expand = c(0.5, 0)) +
  geom_dl(aes(label = SpeciesName), method = list(dl.trans(x = x + .3), "last.points")) +
  geom_dl(aes(label = SpeciesName), method = list(dl.trans(x = x - .3), "first.points")) + 
  theme_bw() 

ggplot(spp_log_avg_type, aes(x = Time, y = ProportionTotalBrowse, colour = Type)) + 
  geom_line() + 
  scale_colour_discrete(guide = 'none') +
  scale_x_continuous(expand = c(0.2, 0)) +
  geom_dl(aes(label = Type), method = list(dl.trans(x = x + .3), "last.points")) +
  geom_dl(aes(label = Type), method = list(dl.trans(x = x - .3), "first.points")) + 
  theme_bw() 
  
#boxplot LANDIS data
ggplot(spp_log_avg, aes(x = SpeciesName, y = ProportionTotalBrowse)) + 
  geom_boxplot()

prop_hodgson <- hodgson %>%
  mutate(m = as.numeric(m)) %>%
  group_by(Year, Location) %>%
  mutate(prop_browse = m / sum(m, na.rm = TRUE))

prop_summary_year <- prop_hodgson %>%
  group_by(Year, Type) %>%
  summarise(mean_prop = mean(prop_browse, na.rm = TRUE))

ggplot(prop_summary_year, aes(x = Year, y = mean_prop, colour = Type)) +
  geom_line()

prop_summary_all <- prop_hodgson %>%
  group_by(Type) %>%
  summarise(mean_prop = mean(prop_browse, na.rm = TRUE))

prop_summary_year$Source <- "Observed"
spp_log_avg_type$Source <- "Landis"
spp_log_avg_type$mean_prop <- spp_log_avg_type$ProportionTotalBrowse
spp_log_avg_type$Year <- spp_log_avg_type$Time + 1998

combine <- bind_rows(prop_summary_year, spp_log_avg_type) %>%
  ungroup() %>%
  as_tibble() %>%
  dplyr::select(c("Type", "mean_prop", "Source", "Year"))

ggplot(na.omit(combine), mapping = aes(x = Type, y = mean_prop, fill = Source)) + 
  geom_boxplot() + 
  ylab(label = "Proportion of total available forage")
ggplot(na.omit(combine), mapping = aes(x = Year, y = mean_prop)) + 
  geom_point(mapping = aes(col = Source, shape = Type)) + 
  geom_smooth(mapping = aes(col = Source, linetype = Type), se = FALSE) + 
  ylab(label = "Proportion of total available \n forage by mass")




#-----------------------------
# variability of browse
forage_list <- paste0("./Models/landis_test/mc_test - browse - linear - annual/browse/SiteForage_",
                      seq(1,20), #seq(1, 9), 
                      ".gis")
forage <- rast(forage_list)
forage2 <- rast(lapply(forage, FUN = function(x) project_to_template(x, template)))
forage2 <- subst(forage2, 0, NA)
forage_extract <- terra::extract(forage2, vect(shapes), fun = function(x) var(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

forage_extract
names(forage_extract) <- c("Location", paste0("Year", seq(1, 20))) #seq(1999,2007)
forage_extract[, 1] <- c("Moskey Basin Landis", "Lane Cove Landis")

forage_extract3 <- pivot_longer(forage_extract, 
                                cols = contains("Year"),
                                names_to = "Year",
                                names_pattern = "Year(.*)",
                                values_to = "variance") %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Year = as.numeric(Year) + 1998)

sum2 <- hodgson %>%
  mutate(m = as.numeric(m),
         s2.m = as.numeric(s2.m)) %>%
  group_by(Location, Year) %>%
  summarise(browse = sum(m, na.rm = TRUE),
            variance = mean(s2.m, na.rm = TRUE))
sum2


all_var <- bind_rows(sum2, forage_extract3)
all_var$Location2 <- ifelse(all_var$Location %in% c("Lane Cove", "Lane Cove Landis"),
                               "Lane Cove",
                               "Moskey Basin")
all_var$Source <- ifelse(grepl("Landis", all_var$Location),
                            "LANDIS",
                            "Observed")

ggplot(all_var[all_var$Location != "Landis", ], mapping = aes(x = Year, y = variance, colour = Location2, linetype = Source, shape = Source)) +
  geom_point() + 
  geom_line() + 
  ylab(label = expression(paste("Variance/mean (g ", m^{-2}, ")")))

means2 <- all_var  %>%
  filter(Source == "Observed" | Year > 2005) %>%
  group_by(Location2, Source) %>%
  summarise(mean_variance = mean(variance))
means2


#-------------------------------
# Full landscape

dir <- "C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/mc_test - browse - linear/browse/"
rasters <- list.files(dir, full.names = TRUE)
rasters <- rasters[grepl("SiteForage", rasters)]

test <- rast(rasters[13])
values(test)[values(test) == 0] <- NA
mean(values(test), na.rm = TRUE)
var(values(test), na.rm = TRUE) / mean(values(test), na.rm = TRUE)
plot(terra::clamp(test, upper = 20), ylim = c(200, 1000), xlim = c(200, 1500))
hist(values(test), xlim = c(0,50), breaks = seq(0, round(max(values(test), na.rm = TRUE)) + 1))




browse_summary <- read.csv("C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/mc_test - browse - linear/browse-summary-log.csv")
ggplot(browse_summary, aes(x = Time, y = MeanForage)) +
  geom_point() + 
  geom_smooth()


#not much effect on recruitment
regen_browse <- read.csv("C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/mc_test - linear/NECN-reproduction-log.csv") %>%
  filter(SpeciesName == " ABBA")

ggplot(regen_browse, aes(x = Time, y = NumCohortsSeed)) +
  geom_point() + 
  geom_smooth()


#species biomass over time
biomass <- read.csv("C:/Users/Sam/Documents/Research/Isle Royale/Models/landis_test/mc_test - linear/spp-biomass-log.csv")

ggplot(biomass, aes(x = Time, y = AboveGroundBiomass_POTR5)) +
  geom_point() + 
  geom_smooth()
 
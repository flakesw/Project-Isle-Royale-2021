library("tidyverse")

station1 <- read.csv("./Parameterization/Parameterization data/weather_stations/roam4_thresholds.csv")

station1$Date <- strsplit(station1$DateHour, " : ") %>%
  purrr::map(pluck(1)) %>%
  unlist() %>%
  gsub(" ", "", .) %>%
  as.Date(., format = "%m/%d/%Y")

station1 <- station1 %>% 
  group_by(Date) %>%
  summarise(Tmin = min(ATMP, na.rm = TRUE),
            Tmax = max(ATMP, na.rm = TRUE),
            site = "ROAM4")
#----------------
station2 <- read.csv("./Parameterization/Parameterization data/weather_stations/pilm4_thresholds.csv")

station2$Date <- strsplit(station2$DateHour, " : ") %>%
  purrr::map(pluck(1)) %>%
  unlist() %>%
  gsub(" ", "", .) %>%
  as.Date(., format = "%m/%d/%Y")

station2 <- station2 %>% 
  group_by(Date) %>%
  summarise(Tmin = min(ATMP, na.rm = TRUE),
            Tmax = max(ATMP, na.rm = TRUE),
            site = "PLIM4")

#-----------
station3 <- read.csv("./Parameterization/Parameterization data/weather_stations/mott_island_thresholds.csv")

station3$Date <- as.Date(station3$Date, format = "%m/%d/%Y")
station3 <- station3 %>%
  mutate(Tmax = (Tmax..F. - 32)/1.8,
         Tmin = (Tmin..F. - 32)/1.8,
         Ppt = (Precipitation..in. * 25.4),
         site = "Mott")

#-----------------
station4 <- read.csv("./Parameterization/Parameterization data/weather_stations/WINM4.2023-08-26.csv")

station4$Date_Time <- as.POSIXct(station4$Date_Time)
station4$Date <- as.Date(station4$Date_Time, format = "%m/%d/%Y")
station4 <- station4 %>%
  group_by(Date) %>%
  summarise(Tmax = max(air_temp_set_1, na.rm = TRUE),
            Tmin = min(air_temp_set_1, na.rm = TRUE)) %>%
  mutate(site = "Windigo")

#-----------------
station5 <- read.csv("./Parameterization/Parameterization data/weather_stations/OJIM4.2023-08-26.csv")

station5$Date_Time <- as.POSIXct(station5$Date_Time)
station5$Date <- as.Date(station5$Date_Time, format = "%m/%d/%Y")
station5 <- station5 %>%
  group_by(Date) %>%
  summarise(Tmax = max(air_temp_set_1, na.rm = TRUE),
            Tmin = min(air_temp_set_1, na.rm = TRUE)) %>%
  mutate(site = "Ojibway")

#-------------

station_all <- bind_rows(station1, station2, station3, station4, station5) %>%
  select(Date, site, Tmin, Tmax, Ppt)
is.na(station_all) <- sapply(station_all, is.infinite)
is.na(station_all) <- sapply(station_all, is.nan)


gridmet_tmax <- read.csv("./Parameterization/Parameterization data/weather_stations/gridmet_tmax.csv") %>%
  mutate(Date = as.Date(TIMESTEP),
         Tmax = MEAN.C.,
         site = "gridmet") %>%
  select(c(Date, Tmax, site))
gridmet_tmin <- read.csv("./Parameterization/Parameterization data/weather_stations/gridmet_tmin.csv") %>%
  mutate(Date = as.Date(TIMESTEP),
         Tmin = MEAN.C.,
         site = "gridmet") %>%
  select(c(Date, Tmin, site))

gridmet <- left_join(gridmet_tmax, gridmet_tmin, by = c("Date", "site"))

station_all <- bind_rows(station_all, gridmet)

station_all_wide <- station_all %>%
  pivot_wider(names_from = site,
              values_from = c(Tmin, Tmax, Ppt))

ggplot(data = station_all, mapping = aes(x = Date, y = Tmax)) +
  geom_line(aes(colour = site)) +
  # geom_line(data = gridmet, aes(x = Date, y = Tmax)) +
  scale_x_date(limits = as.Date(c("1980-01-01", "2020-01-01")))

ggplot(data = station_all_wide, mapping = aes(x = Tmax_gridmet, y = Tmax_ROAM4)) +
  geom_hex() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1)

ggplot(data = station_all_wide, mapping = aes(x = Tmax_gridmet, y = Tmax_PLIM4)) +
  geom_hex() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1)

ggplot(data = station_all_wide, mapping = aes(x = Tmax_gridmet, y = Tmax_Mott)) +
  geom_hex() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1)

ggplot(data = station_all_wide, mapping = aes(x = Tmax_gridmet, y = Tmax_Windigo)) +
  geom_hex() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1)

station_all_wide$Tmax_mean <- rowMeans(station_all_wide[c("Tmax_Windigo", "Tmax_Ojibway")], na.rm = TRUE)
hist(station_all_wide$Tmax_mean - station_all_wide$Tmax_gridmet)

ggplot(data = station_all_wide, mapping = aes(x = Tmax_gridmet, y = Tmax_mean)) +
  geom_hex() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1)

ggplot(data = station_all_wide, mapping = aes(x = Tmin_PLIM4, y = Tmin_Windigo)) +
  geom_hex() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1)

station_all_wide$Tmin_mean <- rowMeans(station_all_wide[c("Tmin_Windigo", "Tmin_Ojibway", "Tmin_ROAM4", "Tmin_PLIM4")], na.rm = TRUE)
hist(station_all_wide$Tmin_mean - station_all_wide$Tmin_gridmet)

ggplot(data = station_all_wide, mapping = aes(x = Tmin_gridmet, y = Tmin_mean)) +
  geom_hex() +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1)
model <- 


station_summ <- station %>%
  group_by(Date) %>%
  summarise(sdTmin = sd(Tmin),
            sdTmax = sd(Tmax))

boxplot(station_summ$sdTmax)
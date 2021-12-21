#other calibration

setwd("C:/Users/Sam/Documents/Research/Isle Royale/")

# N deposition should be around 0.7 g/m2 annually (Li et al. 2016)
# minimum is around 0.04 in winter and maximum is around 0.09 in summer
# annual should be around 0.3-0.5 (Du et al. 2014)
# annual should be around 0.3 (NPS assessment)

# Actual deposition sites can be accessed here: https://nadp.slh.wisc.edu/networks/national-trends-network/

# Our study site (Isle Royale) has two sites; one with data from 1985-2006. We'll 
# download the data and fit the regression needed by NECN

#LANDIS data (uncalibrated)
monthly_precip <- read.csv("C:/Users/Sam/Documents/Research/Isle Royale/landis_test/necn_1_cell/test/NECN-succession-monthly-log.csv") %>%
  group_by(Month) %>%
  summarise(mean_ppt = mean(Precipitation))

# calibration data (NADP sites)
nadp <- read.csv("./calibration_data/nadp/NTN-mi97-s-mgl.csv")
nadp$totalN <- (nadp$NH4/1.28 + nadp$NO3/4.43)/10 #convert to Nitrogen equivalent, and from from kg/ha to g/m2
nadp <- subset(nadp, Criteria1 > 0 & Criteria2 > 0 & Criteria3 > 0 & totalN >= 0)
nadp$ppt <- nadp$ppt*10 #convert from cm to mm
plot(nadp$totalN ~ nadp$ppt)
summary(lm(nadp$totalN ~ nadp$ppt))

# this site shows a decrease in totalN with increased ppt, but it only includes
# growing season months
# But in any case, we see that in the summer, only about 1.5 kg/ha (0.15 g/m2) 
# are deposited. This is about half of the deposition for the mainland site
# nearby in minnesota, so it's important to have local data

nadp_mn <- read.csv("./calibration_data/nadp/NTN-mn08-i-mgl.csv")
nadp_mn$totalN <- (nadp_mn$NH4/1.28 + nadp_mn$NO3/4.43)/10
nadp_mn <- subset(nadp_mn, Criteria1 > 0 & Criteria2 > 0 & Criteria3 > 0 & totalN >= 0)

plot(nadp_mn$totalN ~ nadp_mn$ppt)
summary(lm(nadp_mn$totalN ~ nadp_mn$ppt))

#even stronger decrease in N deposition with ppt! Weird! 

#and we have data from year-round here; there really is a negative relationship
table(nadp_mn$seas)

#this being the case, I'll use the relationship from the actual ISRO site

# However, these data do not match other sources, which find much less N deposition


sum(nadp_mn$ppt / 24) #mean precip is slightly lower on the mainland?

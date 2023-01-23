#get moisture curves from processed ameriflux data
#see process_ameriflux_gpp for processing steps
library("tidyverse")
library("minpack.lm")

#edited csv
sites_to_use <- read.csv("ameriflux_gpp_swc_edit.csv") %>%
  filter(Use)

site_files <- paste0("./Parameterization/necn_parameterize/ameriflux_processed/", sites_to_use$SITE_ID, "_processed.csv")


#some helper functions
read_plus <- function(flnm) {
  print(flnm)
  
  read_csv(flnm) %>%
    group_by(YEAR, DOY) %>%
    summarise(swc = mean(c(SWC), na.rm = TRUE),
              gpp = mean(c(GPP_PI_F), na.rm = TRUE),
              site = str_split(basename(flnm), "_")[[1]][1]) %>%
    ungroup() %>%
    group_by(site) %>%
    mutate(maxrelGPP = gpp/quantile(gpp, 0.999, na.rm = TRUE)) %>%
    mutate(maxrelGPP = ifelse(maxrelGPP < 0, NA, maxrelGPP)) %>%
    mutate(swc = swc/100)
}


#which of the sites should we use?
i <- which(sites_to_use$Functional.group %in% c("Wet broadleaf"))
# i <- c(1, 18, 19, 20, 23)
base_daily <- site_files[i]  %>%
  purrr::map_df(~read_plus(.))


#pull out data for convenience (this is sloppy, TODO)
# Observed_Relative_production <- base_daily$maxrelGPP
# soilwat <- base_daily$swc


#calculate curve for only upper percentiles -- easier to fit curve to
Observed_Relative_production<- base_daily$maxrelGPP

swc<-sort(runif(1000, 0, 1))

out_data <- base_daily[0, ]
for(j in 1:200){ #each half-percent of volumetric water content
  sub_data <- base_daily[which(base_daily$swc > ((j-1) * 0.005) & base_daily$swc < j*0.05), ]
  
  val <- quantile(sub_data$maxrelGPP, .95, na.rm = TRUE)
  
  temp_data <- sub_data[which(sub_data$maxrelGPP > val), ]
  out_data <- rbind(out_data, temp_data)
}

Observed_Relative_production<- out_data$maxrelGPP
soilwat <- out_data$swc


#plot raw data
plot(soilwat, Observed_Relative_production, col="blue", type="p",
     ylim = c(0,1),
     xlim = c(0,1))

# Function to fit coefficients to water curve. 

#original coefficients from Shelby
# moisture_1<- 0.13 #soilwater with maximum gpp
# moisture_2<- 1 #maximum soilwater with any GPP -- changes steepness of decline in gpp
# moisture_3<- 3 #higher values -- sharper dip down towards zero
# moisture_4<- 15 #smaller numbers -- broader curve

#starting coefficients
moisture_1<- 0.4 #soilwater with maximum gpp
moisture_2<- 0.05 #maximum soilwater with any GPP -- changes steepness of decline in gpp
moisture_3<- 3 #higher values -- sharper dip down towards zero
moisture_4<- 0.05 #smaller numbers -- broader curve

#TSWC goes from 0 to 1
swc_val<-sort(runif(1000, 0, 1))

#Calculate fraction here
fraction_val <-(moisture_2 - swc_val)/(moisture_2 - moisture_1)

#This is the curve based on the coefficients listed above
Landis_Relative_production<-ifelse(fraction_val>0, (exp(moisture_3/moisture_4*(1-fraction_val^moisture_4))*(fraction_val^moisture_3)), 0)
lines(swc_val, Landis_Relative_production, type="l", lwd=3, ylab="Maximum Relative GPP", xlab="Soil Water Content")


#Algorithm for calculating fitted relative production based on swc and the 4 coefficients.
calculate_fitted_RP <- function(swc, coef_1, coef_2, coef_3, coef_4)
{
  fraction<-(coef_2 - swc)/(coef_2- coef_1)
  
  pred <- ifelse(fraction>0, (exp(coef_3/coef_4*(1-fraction^coef_4))*(fraction^coef_3)), 0)
}

#Non-linear curve fitting with the equaton above.
swc_dataframe<-data.frame(out_data$swc, out_data$maxrelGPP)
# swc_dataframe<-data.frame(realdat$swc, realdat$maxrelGPP)
names(swc_dataframe) <- c("swc", "maxrelGPP")
# swc_dataframe$coef_1 <- moisture_1
# swc_dataframe$coef_2 <- moisture_2
# swc_dataframe$fraction <- (coef_2 - swc_dataframe$swc)/(swc_dataframe$coef_2- swc_dataframe$coef_1)

#first use nlsLM to get good parameter starting points
curve.nlslrc = nlsLM(maxrelGPP ~ calculate_fitted_RP(swc, coef_1, coef_2, coef_3, coef_4), data = swc_dataframe,
                     start = list(coef_1= moisture_1, coef_2= moisture_2, coef_3= moisture_3, coef_4= moisture_4),
                     control = list(maxiter = 1024))

coef(curve.nlslrc)  #gives good starting coefs


#reset parameters here: 
moisture_1<-coef(curve.nlslrc)[1]
moisture_2<-coef(curve.nlslrc)[2]
moisture_3<-coef(curve.nlslrc)[3]
moisture_4<-coef(curve.nlslrc)[4]

#Calculate fraction here
fraction_val <-(moisture_2 - swc_val)/(moisture_2 - moisture_1)
#This is the curve based on the coefficients listed above
Landis_Relative_production<-ifelse(fraction_val>0, (exp(moisture_3/moisture_4*(1-fraction_val^moisture_4))*(fraction_val^moisture_3)), 0)
lines(swc_val, Landis_Relative_production, type="l", lwd=3, ylab="Maximum Relative GPP", xlab="Soil Water Content", col = "red")


library(nls2)  
#install.packages("minpack.lm")
library(minpack.lm)

us_prr <- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvTS plots/us_prr_maxrelGPPvTS_2cm.csv")
head(us_prr)
us_uaf <- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvTS plots/us_uaf_maxrelGPPvTS_2cm.csv")
head(us_uaf)
us_fcr <- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvTS plots/us_fcr_maxrelGPPvTS_10cm.csv")
head(us_fcr)
us_umb <- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvTS plots/us_umb_maxrelGPPvTS_10cm.csv")
head(us_umb)
bs_combo <- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvTS plots/bs_combo_maxrelGPPvTS_10cm.csv")
us_rpf <- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvTS plots/us_rpf_maxrelGPPvTS_2cm.csv")
head(us_rpf)

shrub_combo <- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvTS plots/shrub_combo_maxrelGPPvTS_2cm.csv")

realdat <- shrub_combo
realdat[realdat$maxrelGPP == -Inf,] <- NA
summary(realdat)
realdat <- na.omit(realdat) 
head(realdat)


# Function to fit coefficients to temp curve. 

#starting coefficients
ppdf_1<- 21.9
ppdf_2<- 278.3
ppdf_3<- 23
ppdf_4<- 11.7


#Temp values, go from 0 to 50oC
temp<-sort(runif(1000, -40, 100))

#Calculate fraction here
fraction<-(ppdf_2 -temp)/(ppdf_2- ppdf_1)

#This is the curve based on the coefficients listed above
Landis_Relative_production<-ifelse(fraction>0, (exp(ppdf_3/ppdf_4*(1-(fraction ^ ppdf_4)))*(fraction ^ ppdf_3)),0)
plot(temp, Landis_Relative_production, type="l", lwd=3, ylab="Maximum Relative GPP", xlab="Soil temp (C) at 10cm")

##Observed_Relative_production will be your input data here from the flux tower
Observed_Relative_production<- realdat$maxrelGPP
temp <- realdat$temp
lines(temp, Observed_Relative_production, col="blue", type="p")  #Plotted it with blue circles so you could compare it to the curve above.

#Algorithm for calculating fitted relative production based on temp and the 4 coefficients.
calculate_fitted_RP <- function(temp, coef_1, coef_2, coef_3, coef_4)
{
  fraction<-(coef_2 -temp)/(coef_2- coef_1)
  
  pred <- ifelse(fraction>0, (exp(coef_3/coef_4*(1-(fraction ^ coef_4)))*(fraction ^ coef_3)),0)
}

#Non-linear curve fitting with the equaton above.
temp_dataframe<-data.frame(realdat$temp, realdat$maxrelGPP)
names(temp_dataframe) <- c("temp", "maxrelGPP")

Fitted_RelativeProduction <- nls2(maxrelGPP ~ calculate_fitted_RP(temp, coef_1, coef_2, coef_3, coef_4), data = temp_dataframe,
                                  start = list(coef_1= ppdf_1, coef_2= ppdf_2, coef_3= ppdf_3, coef_4= ppdf_4), 
                                  trace=T, nls.control(maxiter = 10000,
                                  printEval = F, warnOnly = T)
)

summary(Fitted_RelativeProduction)
coef(Fitted_RelativeProduction)

#plot results to see how it looks.
lines(temp, predict(Fitted_RelativeProduction), col="red")


######################################################################################################################################                  
# Function to fit coefficients to water curve. 

# # black spruce dataset 1
# us_prr<- read.csv("C:/Users/13146/Documents/ReburnsAK/Modeling/Growth_and_soiltemp/GPPvSWC plots/us_prr_maxrelGPPvSWC_2cm.csv")
# head(us_prr)
# us_prr$swc <- us_prr$swc/100
# 
# #black spruce dataset 2
# us_uaf<- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvSWC plots/us_uaf_maxrelGPPvSWC_2cm.csv")
# head(us_uaf)
# us_uaf$swc <- us_uaf$swc/100

#shrub dataset
us_fcr<- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvSWC plots/us_fcr_maxrelGPPvSWC_2cm.csv")
head(us_fcr)
us_fcr$swc <- us_fcr$swc/100

#hardwood dataset
us_umb<- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvSWC plots/us_umb_maxrelGPPvSWC_2cm.csv")
head(us_umb)
us_umb$swc <- us_umb$swc/100

us_rpf <- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvSWC plots/us_rpf_maxrelGPPvSWC_2cm.csv")
head(us_rpf)
us_rpf$swc <- us_rpf$swc/100

#both black spruce datasets
bs_combo <- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvSWC plots/bs_combo_maxrelGPPvSWC_2cm.csv")
head(bs_combo)
bs_combo$swc <- bs_combo$swc/100

#both shrub datasets
shrub_combo <- read.csv("C:/Users/13146/Documents/ReburnsAK/DGS/GPP_soilmoisture_and_soiltemp/GPPvSWC plots/shrub_combo_maxrelGPPvSWC_2cm.csv")
head(shrub_combo)
shrub_combo$swc <- shrub_combo$swc/100


#choose which dataset to focus on 
realdat <- shrub_combo


# Function to fit coefficients to water curve. 

#starting coefficients
moisture_1<- 0.13
moisture_2<- 1
moisture_3<- 3
moisture_4<- 15
  
#Temp values, go from 0 to 50oC
swc<-sort(runif(1000, 0, 1))

#Calculate fraction here
fraction<-(moisture_2 -swc)/(moisture_2- moisture_1)

#This is the curve based on the coefficients listed above
Landis_Relative_production<-ifelse(fraction>0, (exp(moisture_3/moisture_4*(1-fraction^moisture_4))*(fraction^moisture_3)), 0)
plot(swc, Landis_Relative_production, type="l", lwd=3, ylab="Maximum Relative GPP", xlab="Soil Water Content")

##Observed_Relative_production will be your input data here from the flux tower
Observed_Relative_production<- realdat$maxrelGPP
swc <- realdat$swc
lines(swc, Observed_Relative_production, col="blue", type="p")  #Plotted it with blue circles so you could compare it to the curve above.

#Algorithm for calculating fitted relative production based on swc and the 4 coefficients.
calculate_fitted_RP <- function(swc, coef_1, coef_2, coef_3, coef_4)
{
  fraction<-(coef_2 -swc)/(coef_2- coef_1)
  
  pred <- ifelse(fraction>0, (exp(coef_3/coef_4*(1-fraction^coef_4))*(fraction^coef_3)), 0)
}

#Non-linear curve fitting with the equaton above.
swc_dataframe<-data.frame(realdat$swc, realdat$maxrelGPP)
names(swc_dataframe) <- c("swc", "maxrelGPP")

#first use nlsLM to get good parameter starting points
curve.nlslrc = nlsLM(maxrelGPP ~ calculate_fitted_RP(swc, coef_1, coef_2, coef_3, coef_4), data = swc_dataframe,
                     start = list(coef_1= moisture_1, coef_2= moisture_2, coef_3= moisture_3, coef_4= moisture_4))

coef(curve.nlslrc)  #gives good starting coefs


#reset parameters here: 
moisture_1<-coef(curve.nlslrc)[1]
moisture_2<-coef(curve.nlslrc)[2]
moisture_3<-coef(curve.nlslrc)[3]
moisture_4<-coef(curve.nlslrc)[4]

Fitted_RelativeProduction <- nls2(maxrelGPP ~ calculate_fitted_RP(swc, coef_1, coef_2, coef_3, coef_4), data = swc_dataframe,
                                   start = list(coef_1= moisture_1, coef_2= moisture_2, coef_3= moisture_3, coef_4= moisture_4), 
                                  trace=T, nls.control(maxiter = 10000,
                                  printEval = T, warnOnly = T)
 )
 
 summary(Fitted_RelativeProduction)
 coef(Fitted_RelativeProduction)
 
 #plot results to see how it looks.
 moisture_1<-coef(Fitted_RelativeProduction)[1]
 moisture_2<-coef(Fitted_RelativeProduction)[2]
 moisture_3<-coef(Fitted_RelativeProduction)[3]
 moisture_4<-coef(Fitted_RelativeProduction)[4]

#Now plot with new fitted values
swc<-sort(runif(1000, 0, 1))
fraction<-(moisture_2 -swc)/(moisture_2- moisture_1)
Landis_Relative_production<-ifelse(fraction>0, (exp(moisture_3/moisture_4*(1-fraction^moisture_4))*(fraction^moisture_3)), 0)
plot(swc, Landis_Relative_production, type="l", lwd=3, ylab="Maximum Relative GPP", xlab="Soil Water Content")
Observed_Relative_production<- realdat$maxrelGPP
swc <- realdat$swc
lines(swc, Observed_Relative_production, col="blue", type="p")  #Plotted it with blue circles so you could compare it to the curve above.


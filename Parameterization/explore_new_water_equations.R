
#starting coefficients
moisture_1<- 0.15 #soilwater with maximum gpp
moisture_2<- 0.03 #maximum or minimum soilwater with any GPP -- changes steepness of decline in gpp
moisture_3<- 1.40E-05
 #higher values -- sharper dip down towards zero
moisture_4<- 8.1033
 #smaller numbers -- broader curve


#TSWC goes from 0 to 1
swc_val<-sort(runif(1000, 0, 1))

#Calculate fraction here
fraction_val <-(moisture_2 - swc_val)/(moisture_2 - moisture_1)

#This is the curve based on the coefficients listed above
Landis_Relative_production<-ifelse(fraction_val>0, (exp(moisture_3/moisture_4*(1-fraction_val^moisture_4))*(fraction_val^moisture_3)), 0)
plot(swc_val, Landis_Relative_production, lwd=3, ylab="Maximum Relative GPP", xlab="Soil Water Content", type = "l")
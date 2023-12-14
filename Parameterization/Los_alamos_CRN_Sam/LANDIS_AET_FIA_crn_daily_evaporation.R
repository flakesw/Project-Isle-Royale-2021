#Landis AET. Adapted from SoilWater.cs in NECN

stormFlowFraction <- 0.0
WaterLossFactor1 <- 0.8
WaterLossFactor2 <- 0.8
baseFlowFraction <- 0.0

LANDIS_availableWater <- NULL
LANDIS_AET <- NULL
LANDIS_PET <- NULL

clay.pct <- 9.7#21.3#Los Alamos
sand.pct <- 65.9#41.8#Los Alamos

acoef <- exp(-4.396 - 0.0715 * clay.pct - 0.000488 * (sand.pct^2) - 0.00004285 * (sand.pct^2) * clay.pct)
bcoef <- (-3.14 - 0.00222 * (clay.pct^2) - 0.00003484 * (sand.pct^2) * clay.pct)
WiltingPoint <- (15/acoef)^(1/bcoef)
FieldCapacity <- (0.333/acoef)^(1/bcoef) #xx set to reasonable field condition
soil_depth <- 100

# setwd("./Los_alamos_CRN_Sam")

#soil water data
soil.water.crn <- read.csv("Los_Alamos_mean_monthly_soilm_depth100.csv")[,c(2,3)]
soil.water.crn.5 <- read.csv("Los_Alamos_soil_5.csv")
soil.water.crn.20 <- read.csv("Los_Alamos_soil_20.csv")
soil.water.crn.50 <- read.csv("Los_Alamos_soil_50.csv")
soil.water.crn.100 <- read.csv("Los_Alamos_soil_100.csv")

head(soil.water.crn)
soil.water.raw <- read.csv("CRNH0203-2018-NM_Los_Alamos_13_W.csv")
head(soil.water.raw)
soil.water.mid <- soil.water.raw[soil.water.raw$Day==15,c("stationID","Year","Month","Day","sm_5cm","sm_10cm","sm_20cm","sm_50cm","sm_100cm","Lat","Lon")]
tail(soil.water.mid)
plot(1:nrow(soil.water.mid),soil.water.mid[,"sm_20cm"], type = "l")

n <- 1

#repeat{

plot <- soil.water.mid[n, "stationID"]
XY <- soil.water.raw[n, c("Lat", "Lon")]
lat <- XY[1,"Lat"]
lon <- XY[1,"Lon"]

standingBiomass <- 100
litterBiomass <- 100

#Variables
slope <- 0
aspect <- 1

latitude <- lat
AET.inputs <- read.csv("Los_Alamos_AET_inputs.csv")
head(AET.inputs)
#AET.inputs <- read.csv("Los_Alamos_Penman_inputs.csv")
#Thornthwaite.pet <- read.csv("Landis_Los_Alamos_PET.csv")
#pet <- Thornthwaite.pet$monthlyPET/10 #convert mm to cm
#pet <- AET.inputs$ETo/10#convert mm to cm
#plot(pet,ETo)
head(AET.inputs)
#P.sum <- aggregate(Precip ~ Month+Year, data = AET.inputs, sum)
#Precipitation <- P.sum$Precip/10#convert mm to cm
Precipitation <- AET.inputs$Precip/10#convert mm to cm
#Tinputs <- aggregate(cbind(Tmax,Tmin)~Month+Year,data=AET.inputs,mean)
#tmin <- Tinputs$Tmin
#tmax <- Tinputs$Tmax
tmin <- AET.inputs$Tmin
tmax <- AET.inputs$Tmax

years.repeat <- rep(2010:2022,each=12)
months.repeat <- rep(seq(1,12,1), times=13)

monthly.T <- (tmin + tmax)/2

avgTemp <- (ifelse(monthly.T > 0, monthly.T, 0))

heatIndexpermonth <- (avgTemp / 5)^1.514
heatIndex <- rep(as.vector(tapply(heatIndexpermonth, (seq_along(avgTemp)-1) %/% 12, sum)), each = 12)#for a different heatIndex each year.

alpha <- (0.000000675 * heatIndex^3.0) - (0.0000771 * heatIndex^2.0) + (0.01792 * heatIndex) + 0.49239

#calculate mean monthly day length using middle day of each month.  
n.years <- 13 #Number of years in climate data
month.year <- seq(as.Date("2010-1-15"), by = "months", length = n.years*12)
DayOfYear <- rep(c(15, 46,76,107,137,168,198,229,259,290,321,351), n.years)#May need to improve to accommodate leap year
MonthOfYear <- rep(seq(0,11,1), times = n.years)
DOY <- DayOfYear #Where is this calculated in AnnualClimate.cs?

LatRad <- latitude * (2.0 * pi) / 360#recalculated below with an if statement, to deal with very high latitude? 

#r <- 1.0 - (0.0167 *  cos(0.0172 * (DOY - 3)))#radius vector of the sun. Where is this used?
z <- 0.39785 * sin(4.868961 + 0.017203 * DOY + 0.033446 * sin(6.224111 + 0.017202 * DOY))
decl <- ifelse(abs(z) < 0.7, atan(z / sqrt(1 - z^2)), pi/2.0 - atan(sqrt(1-z^2))/z)

LatRad <- ifelse(abs(LatRad) >= (pi / 2.0), sign(latitude) * (pi/2.0 - 0.01), LatRad)#for the poles? I don't see the point in this

z2 <- -tan(decl) * tan(LatRad)

ZCos <- function(T){
  TA <- abs(T)
  AC <- ifelse(TA < 0.7, 1.570796 - atan(TA/sqrt(1 - TA * TA)), atan(sqrt(1-TA*TA)/ TA))
  ifelse(T < 0, 3.141593 - AC, AC)
}

h <- ifelse(z2 >=1, 0, ifelse(z2 <= -1, pi, ZCos(z2)))#z2 >=1 when sun stays below horizon, z2<=-1 when sun stays above horizon

hr <- 2.0 * (h * 24) / (2 * 3.1416)#length of day in hours
MeanDayLength <- hr

#calculate number of days between monthly midpoints.  I think this is what is going on.  
months <- seq(as.Date("2010/1/1"), as.Date("2020/12/1"), by = "month")
DaysInMonth <- lubridate::days_in_month(months)

m2 <- 2; m1 <- 1

totalDays <- NULL
repeat{
  totalDays.x <- as.integer((DaysInMonth[m2] + DaysInMonth[m1])/2)
  totalDays.x <- ifelse(is.na(totalDays.x),31,totalDays.x)#to remove NAs in final month
  totalDays <- c(totalDays, totalDays.x)
  m2 <- m2 + 1
  m1 <- m1 + 1
  if (m2==158){break}#m=number of months + 2
}
AvgTemp <- ifelse(monthly.T > 0, monthly.T, 0)#This is the average temperature of each month in the series

monthlyPET <- ifelse(AvgTemp > 0, (16.0 * (MeanDayLength / 12.0) * (totalDays / 30.0) * (((10 * AvgTemp) / heatIndex)^alpha)), 0)#Divide by 10 for calculating AET?

pet <- monthlyPET/10

snow <- ifelse(tmin <= 0, Precipitation, 0)#Use tmin to dictate whether it snows or rains.
H2Oinputs <- ifelse(tmin <= 0, 0, Precipitation)#set rain to 0 when min T < 0. See L88 of SoilWater.cs

waterFull <- soil_depth * FieldCapacity
waterEmpty <- WiltingPoint * soil_depth
standingBiomass <- ifelse(standingBiomass > 800, 800, standingBiomass)
litterBiomass <- ifelse(litterBiomass > 400, 400, litterBiomass)
canopyIntercept <- ((0.0003 * litterBiomass) + (0.0006 * standingBiomass)) * WaterLossFactor1

#Adjust PET at the pixel scale for slope and aspect following Bugmann 1994, p 82 and Schumacher 2004, p. 114.
#First calculate slope and aspect modifier (SlAsp)
SA <- function(slope, aspect) {
  if (slope > 30) {
    if (aspect>=0 & aspect<= 90) {
      x = (-2+(aspect/90))
    } else if (aspect>90 & aspect<= 180) {
      x = (-1+((aspect-90)/30))
    } else if (aspect>180 & aspect<= 270) {
      x = (2-((aspect-180)/90))
    } else if (aspect>270 & aspect<= 360) {
      x = (1-((aspect-270)/30))
    } else {
      x = 0
    }
  } else if (slope > 10) {
    if (aspect >= 0 & aspect <= 180) {
      x = (-1+(aspect)/90)
    } else {
      x = (1-((aspect-180)/90))
    }
  } else {
    x = 0
  }

  return(x)
}

SlAsp <- SA(slope, aspect)

if (SlAsp > 0) {
  pet <- pet * (1 + SlAsp * 0.125)
} else {
  pet <- pet * (1 + SlAsp * 0.063)
}

liquidSnowpack <- NULL; liquidSnowpack.x <- 0
soilWaterContent <- NULL; 
#soilWaterContent.x <- waterFull#start with saturated soil
soilWaterContent.x <- soil.water.crn.100$SOIL_MOISTURE_100_DAILY[1] * soil_depth
#soilWaterContent.x <- waterEmpty#start with dry soil 
snowMeltFraction <- NULL; snowMeltFraction.x <- 0
remainingPET <- NULL; remainingPET.x <- 0
waterMovement <- NULL; waterMovement.x <- 0
AET <- NULL; PET <- NULL; CWD <- NULL
availableWater <- NULL
availableWaterMin <- NULL;availableWater.mid <- NULL
soilWaterMax <- NULL
soilWaterContent.mid.x <- NULL; soilWaterContent.mid <- NULL
holding.tank.x <- 0; holding.tank <- NULL #To calculate baseflow
baseFlow.x <- 0; baseFlow <- NULL
stormFlow.x <- 0; stormFlow <- NULL
streamFlow.x <- 0; streamFlow <- NULL
meanSoilWater <- NULL

#s<-1
#length(tmin)
if(watermode == "Henne"){
  for(s in 1:156){
    #...Calculating snow pack first. Occurs when mean monthly air temperature is equal to or below freezing, precipitation is in the form of snow.
    liquidSnowpack.x <- ifelse(tmin[s] <= 0, snow[s] + liquidSnowpack.x, liquidSnowpack.x)#accumulate snow when min T < 0. see L85-89 SoilWater.cs
    #...Then melt snow if there is snow on the ground and air temperature (tmax) is above minimum.     
    snowMeltFraction.x <- ifelse(liquidSnowpack.x > 0 && tmax[s] > 0, max((tmax[s] * 0.05)+0.024, 0), 0)
    snowMeltFraction.x <- ifelse(snowMeltFraction.x > 1,1,snowMeltFraction.x)
    #Amount of liquidsnowpack that melts = liquidsnowpack multiplied by the fraction that melts. Subtract melted snow from snowpack and add it to soil
    addToSoil.x <- ifelse(tmin[s] <= 0, liquidSnowpack.x * snowMeltFraction.x, H2Oinputs[s])
    liquidSnowpack.x <- liquidSnowpack.x - (liquidSnowpack.x * snowMeltFraction.x)
    #...Evaporate water from the snow pack (rewritten by Pulliam 9/94)...Coefficient 0.87 relates to heat of fusion for ice vs. liquid water
    evaporatedSnow <- ifelse(liquidSnowpack.x > 0, pet[s] * 0.87, 0) 
    evaporatedSnow <- ifelse(evaporatedSnow > liquidSnowpack.x, liquidSnowpack.x, evaporatedSnow)#Don't evaporate more snow than actually exists...Is this possible? Maybe I evaporated at the wrong time.
    liquidSnowpack.x <- liquidSnowpack.x - evaporatedSnow#Did I evaporate at the right time? 
    liquidSnowpack <- c(liquidSnowpack, liquidSnowpack.x)#vector to track snowpack through time
    #  remainingPET.x <- max(pet[s] - evaporatedSnow, 0) #Decrement remaining pet by energy used to evaporate snow.  CENTURY code divides evaporatedSnow by 0.87 so it matches the PET used to melt snow. 
    remainingPET.x <- max(pet[s] - evaporatedSnow/0.87, 0)
    #Should evaporated snow come out of the add? It is used to decrement PET (remainingPET) and therefore reduces AET
    #  addToSoil.x <- addToSoil.x - evaporatedSnow #Subtract evaporated snow from add. Does this subtract it twice?
    #Calculate bare soil water loss and interception  when air temperature is above freezing and no snow cover....Mofified 9/94 to allow interception when t < 0 but no snow cover, Pulliam
    bareSoilEvaporation.x <- ifelse(liquidSnowpack.x <= 0, 0.5 * exp((-0.002 * litterBiomass) - (0.004 * standingBiomass)) * WaterLossFactor2, 0)
    #remainingPET.x <- ifelse(liquidSnowpack.x <= 0, pet[s], remainingPET.x)#may not be needed with the way I calculated this above.     #something not right here. PET can go back up in the month that all the snow melts...check in LANDIS code.                         
    #remainingPET <- c(remainingPET, remainingPET.x)
    soilEvaporation.x <- ifelse(liquidSnowpack.x <= 0, min(((bareSoilEvaporation.x + canopyIntercept) * H2Oinputs[s]), (0.4 * remainingPET.x)), 0)
    #This is new. Subtract intercept and evaporation from the addtosoil. I did not subtract evaporated snow because this was used to decrement PET. Is this right?
    addToSoil.x <- addToSoil.x - soilEvaporation.x 
    remainingPET.x <- remainingPET.x - soilEvaporation.x #decrement PET by soil evaporation? CENTURY does this. 10/23/2020.
    #9/19/2022 Could also at soilEvaporation to AET. This would have the same effect as using remainingPET to calculate CWD, but would increase AET.
    remainingPET <- c(remainingPET, remainingPET.x)
    #Calculate the max amount of water available to trees, an overestimate of the water available to treees. It only reflects precip and melting snow.
    availableWaterMax.x <- soilWaterContent.x - waterEmpty + addToSoil.x  #New 10/23/2020: subtracted waterEmpty, because that is what is actually available to plants... Also moved below interception because that water is not available to plants
    #add water to soil 
    soilWaterContent.x <- soilWaterContent.x + addToSoil.x
    soilWaterMax.x <- soilWaterContent.x #track to compare to soil moisture data
    soilWaterMax <- c(soilWaterMax,soilWaterMax.x)
    #Calculate actual evapotranspiration.  This equation is derived from the stand equation for calculating AET from PET Bergstr?m, 1992
    #Needs to be >= or else AET can exceed PET when soilWaterContent = waterFull.  Also, CENTURY takes this out of the "add." Perhaps this doesn't matter.
    actualET.x <- #min(soilWaterContent.x - waterEmpty,
                      ifelse((soilWaterContent.x - waterEmpty) >= remainingPET.x, remainingPET.x,#actualET.x <- ifelse((soilWaterContent.x > waterFull), remainingPET.x, #but what if remainingPET > soilWaterContent? This would overestimate AET.
                             min(remainingPET.x * ((soilWaterContent.x - waterEmpty) / (waterFull - waterEmpty)), soilWaterContent.x - waterEmpty))#)#Min term is needed if AWC is very low because (soilWaterContent.x - waterEmpty) / (waterFull - waterEmpty) can be > 1
    #  actualET.x <- min(soilWaterContent.x - waterEmpty, remainingPET.x) #simplified version of CENTURY approach, which weights water losses among soil layers.
    
    # daily.remaining.PET <- remainingPET.x/30#To approximate daily PET. Removing water on an ~daily time step makes it possible for water to be harder to remove from soil as soils get drier. 
    # availableWater.m <- soilWaterContent.x - waterEmpty#To limit AET to available water
    # d <- 1
    # daily.ET <- NULL
    # repeat{
    #  #Beta <- ifelse(soilWaterContent.x >= waterFull, 1, 1 - exp(-6.68 * soilWaterContent.x / waterFull))#to allow available water to drop below PWP
    #  Beta <- ifelse(soilWaterContent.x >= waterFull, 1, 1 - exp(-6.68 * (soilWaterContent.x - waterEmpty) / (waterFull - waterEmpty)))#to limit water loss to PWP
    #  #Beta <- ifelse(soilWaterContent.x >= waterEmpty, 1, soilWaterContent.x / waterEmpty)#Water loss decreases linearly below PWP
    #   daily.ET.x <- ifelse((soilWaterContent.x - waterFull) >= daily.remaining.PET, daily.remaining.PET, daily.remaining.PET * Beta)
    #   daily.ET.x <- ifelse(daily.ET.x < 0, 0, daily.ET.x)#Probably not needed
    #   soilWaterContent.x <- max(soilWaterContent.x - daily.ET.x,0)
    #   daily.ET <- c(daily.ET,daily.ET.x)
    #   d <- d + 1
    #   if (d==31){break}
    # }
    # #actualET.x <- min(sum(daily.ET), availableWater.m) #Needed to limit AET to available water. Soil water can still go below this value though because it is drawn down in the ~daily time step loop above.
    # actualET.x <- sum(availableWater.m)
    
    #  Beta <- ifelse(soilWaterContent.x >= waterFull, 1, 1 - exp(-6.68 * soilWaterContent.x / waterFull))#NEW 9/19/2022 From Gavin: Generally, as soils dry, it is harder to remove the remaining water. In my old version reduction in AET was linear and stopped at WP. This allows water loss below WP.
    #  actualET.x <- ifelse((soilWaterContent.x - waterFull) >= remainingPET.x, remainingPET.x, remainingPET.x * Beta)#from Willmott 1985 (https://doi.org/10.1002/joc.3370050602) as applied by Gavin et al. 2005 (https://doi.org/10.1002/joc.3370050602). See: https://pages.uoregon.edu/dgavin/software/AETcalculator.pdf
    #  actualET.x <- ifelse(actualET.x < 0, 0, actualET.x)# + soilEvaporation.x + evaporatedSnow#Would increase AET. Then use PET instead of remainingPET to calculate CWD
    
    AET <- c(AET, actualET.x)
    CWD.x <- remainingPET.x - actualET.x
    CWD <- c(CWD, CWD.x)
    #Subtract transpiration from soil water content. CENTURY takes this portion out of the "add" before putting it into the soil. Does this do the same thing though. Perhaps in underestimates AET if there is a water deficit from the previous month?
    soilWaterContent.x <- max(soilWaterContent.x - actualET.x,0)#original, comment out if using daily.ET.x  
   
     #Allow excess water to run off during storm events (stormflow)
    #How much water should move during a storm event, which is based on how much water the soil can hold.
    waterMovement.x <- ifelse(soilWaterContent.x > waterFull, max(soilWaterContent.x - waterFull), 0)
    
    #Compute stormFlow, baseFlow, and streamFlow
    stormFlow.x <- waterMovement.x * stormFlowFraction#calculate stormFlow
    stormFlow <- c(stormFlow, stormFlow.x)#track as a vector
    waterMovement.x <- waterMovement.x - stormFlow.x#remove stormFlow from excess water that goes to holding tank.
    holding.tank.x <- holding.tank.x + waterMovement.x#put rest of excess into holding tank.
    baseFlow.x <- holding.tank.x * baseFlowFraction#Calculate base flow as portion of holding tank instead of portion of soil water.
    #baseFlow.x <- soilWaterContent.x * baseFlowFraction#Calculate base flow as portion of holding tank instead of portion of soil water.
    #soilWaterContent.x <- soilWaterContent.x - baseFlow.x
    holding.tank.x <- holding.tank.x - baseFlow.x#remove base flow from holding tank.
    holding.tank <- c(holding.tank, holding.tank.x)#track holding tank water as a vector.
    baseFlow <- c(baseFlow, baseFlow.x)#track base flow as a vector
    streamFlow.x <- stormFlow.x + baseFlow.x#estimate stream flow
    streamFlow <- c(streamFlow, streamFlow.x) #track stream flow as a vector.
    #Set soil water content to holding capacity if it is greater then holding capacity.
    soilWaterContent.x <- ifelse(soilWaterContent.x > waterFull, waterFull, soilWaterContent.x)
    soilWaterContent <- c(soilWaterContent, soilWaterContent.x)
    #Calculate the amount of available water after all the evapotranspiration and leaching has taken place (minimum available water)           
    availableWaterMin.x = max(soilWaterContent.x - waterEmpty, 0.0)
    availableWaterMin <- c(availableWaterMin, availableWaterMin.x)
    #Calculate the final amount of available water to the trees, which is the average of the max and min...may need to change! Used for Max Allowable Drought may overestimate now.        
    availableWater.mid <- (availableWaterMax.x + availableWaterMin.x)/ 2.0
    availableWater <- c(availableWater, availableWater.mid)
    
    meanSoilWater <- c(meanSoilWater, (soilWaterContent.x + soilWaterMax.x)/2)
  }
}

if(watermode == "New"){
  for(s in 1:156){
    #...Calculating snow pack first. Occurs when mean monthly air temperature is equal to or below freezing, precipitation is in the form of snow.
    liquidSnowpack.x <- ifelse(tmin[s] <= 0, snow[s] + liquidSnowpack.x, liquidSnowpack.x)#accumulate snow when min T < 0. see L85-89 SoilWater.cs
    #...Then melt snow if there is snow on the ground and air temperature (tmax) is above minimum.     
    snowMeltFraction.x <- ifelse(liquidSnowpack.x > 0 && tmax[s] > 0, max((tmax[s] * 0.05)+0.024, 0), 0)
    snowMeltFraction.x <- ifelse(snowMeltFraction.x > 1,1,snowMeltFraction.x)
    #Amount of liquidsnowpack that melts = liquidsnowpack multiplied by the fraction that melts. Subtract melted snow from snowpack and add it to soil
    addToSoil.x <- ifelse(tmin[s] <= 0, liquidSnowpack.x * snowMeltFraction.x, H2Oinputs[s])
    liquidSnowpack.x <- liquidSnowpack.x - (liquidSnowpack.x * snowMeltFraction.x)
    #...Evaporate water from the snow pack (rewritten by Pulliam 9/94)...Coefficient 0.87 relates to heat of fusion for ice vs. liquid water
    evaporatedSnow <- ifelse(liquidSnowpack.x > 0, pet[s] * 0.87, 0) 
    evaporatedSnow <- ifelse(evaporatedSnow > liquidSnowpack.x, liquidSnowpack.x, evaporatedSnow)#Don't evaporate more snow than actually exists...Is this possible? Maybe I evaporated at the wrong time.
    liquidSnowpack.x <- liquidSnowpack.x - evaporatedSnow#Did I evaporate at the right time? 
    liquidSnowpack <- c(liquidSnowpack, liquidSnowpack.x)#vector to track snowpack through time
    #  remainingPET.x <- max(pet[s] - evaporatedSnow, 0) #Decrement remaining pet by energy used to evaporate snow.  CENTURY code divides evaporatedSnow by 0.87 so it matches the PET used to melt snow. 
    remainingPET.x <- max(pet[s] - evaporatedSnow/0.87, 0)
    #Should evaporated snow come out of the add? It is used to decrement PET (remainingPET) and therefore reduces AET
    #  addToSoil.x <- addToSoil.x - evaporatedSnow #Subtract evaporated snow from add. Does this subtract it twice?
    
    soilWaterContent.x <- soilWaterContent.x + addToSoil.x
    
    #xx moved soil water max here
    soilWaterMax.x <- max(soilWaterContent.x, 0) #track to compare to soil moisture data
    soilWaterMax <- c(soilWaterMax,soilWaterMax.x)
    
    
    #Calculate bare soil water loss and interception  when air temperature is above freezing and no snow cover....Mofified 9/94 to allow interception when t < 0 but no snow cover, Pulliam
    bareSoilEvaporation.x <- ifelse(liquidSnowpack.x <= 0, 0.5 * exp((-0.002 * litterBiomass) - (0.004 * standingBiomass)) * WaterLossFactor2, 0)
    
    #remainingPET.x <- ifelse(liquidSnowpack.x <= 0, pet[s], remainingPET.x)#may not be needed with the way I calculated this above.     #something not right here. PET can go back up in the month that all the snow melts...check in LANDIS code.                         
    #remainingPET <- c(remainingPET, remainingPET.x)
    
    soilEvaporation.x <- ifelse(liquidSnowpack.x <= 0, min(((bareSoilEvaporation.x + canopyIntercept) * H2Oinputs[s]), (0.4 * remainingPET.x)), 0)
    soilEvaporation.x <- min(soilEvaporation.x, soilWaterContent.x) #don't let evaporation create negative soil water
    soilWaterContent.x <- soilWaterContent.x - soilEvaporation.x

    remainingPET.x <- remainingPET.x - soilEvaporation.x #decrement PET by soil evaporation? CENTURY does this. 10/23/2020.
    #xx TODO! 9/19/2022 Could also at soilEvaporation to AET. This would have the same effect as using remainingPET to calculate CWD, but would increase AET.
    remainingPET <- c(remainingPET, remainingPET.x)
    
    
    stormFlow.x <- 0
    
    if (soilWaterContent.x > waterFull)
    {
      # How much water should move during a storm event, which is based on how much water the soil can hold.
      waterMovement = max((soilWaterContent.x - waterFull), 0.0)
      
        #...Compute storm flow.
       stormFlow.x = waterMovement * stormFlowFraction; #SF some of the waterMovement just disappears here -- the waterMovement that isn't part of stormFlow gets removed in line 158,
                                                               #but not accounted for as runoff

       #Subtract stormflow from soil water
       soilWaterContent.x <- soilWaterContent.x - stormFlow.x; #SF remove some excess water as stormflow; the rest remains available until end of month. SWC can still be greater than FC at this point

    }
    
    stormFlow <- c(stormFlow, stormFlow.x)
    
    
    #Calculate actual evapotranspiration.  This equation is derived from the stand equation for calculating AET from PET Bergstr?m, 1992
    #Needs to be >= or else AET can exceed PET when soilWaterContent = waterFull.  Also, CENTURY takes this out of the "add." Perhaps this doesn't matter.
    actualET.x <- 
      ifelse(soilWaterContent.x - waterEmpty >= remainingPET.x,  #xx
             remainingPET.x,
             min(remainingPET.x * ((soilWaterContent.x - waterEmpty) / (waterFull - waterEmpty)), soilWaterContent.x - waterEmpty))
    
    # soil_prev[Case]*(1-exp(-(et0-input)[Case]/awc[Case]))
    # soilWaterContent.x * (1 - exp((-remainingPET.x/waterEmpty)))
    
    actualET.x <- min(actualET.x, soilWaterContent.x)
    actualET.x <- max(actualET.x, 0)
    
    AET <- c(AET, actualET.x)
    
    CWD.x <- pet[s] - actualET.x #xx changed to use original PET
    
    CWD <- c(CWD, CWD.x)
    
    #Subtract transpiration from soil water content. CENTURY takes this portion out of the "add" before putting it into the soil. Does this do the same thing though. Perhaps in underestimates AET if there is a water deficit from the previous month?
    
    soilWaterContent.x <- max(soilWaterContent.x - actualET.x, 0)
    
    baseFlow.x <- 0
    
    #remove baseflow
    remainingWater <- soilWaterContent.x - waterEmpty
    remainingWater <- max(remainingWater, 0)
    print(remainingWater)
    baseFlow.x <- remainingWater * baseFlowFraction #Calculate base flow as portion of holding tank instead of portion of soil water. xx
    baseFlow.x <- max(baseFlow.x, 0)
    soilWaterContent.x <- soilWaterContent.x - baseFlow.x
   
    surplus <- max(soilWaterContent.x - waterFull, 0) #xx added. Drain off surplus water and add to baseflow
    baseFlow.x <- baseFlow.x + surplus
    baseFlow <- c(baseFlow, baseFlow.x)
    soilWaterContent.x <- soilWaterContent.x - surplus
    
    soilWaterContent <- c(soilWaterContent, soilWaterContent.x)
    meanSoilWater <- c(meanSoilWater, (soilWaterContent.x + soilWaterMax.x)/2)
    #Calculate the amount of available water after all the evapotranspiration and leaching has taken place (minimum available water)
    availableWaterMax.x <- max(soilWaterMax.x - waterEmpty, 0.0) #TODO subtract waterEmpty?
    availableWaterMin.x = max(soilWaterContent.x - waterEmpty, 0.0)
    availableWaterMin <- c(availableWaterMin, availableWaterMin.x)
    #Calculate the final amount of available water to the trees, which is the average of the max and min...may need to change! Used for Max Allowable Drought may overestimate now.        
    availableWater.mid <- (availableWaterMax.x + availableWaterMin.x)/ 2.0
    availableWater <- c(availableWater, availableWater.mid)
  }
}

options(scipen = 999)
plot(seq(1,156,1),availableWater, type = "l")
abline(h = 0, col = "grey")
baseFlow
#availableWater <- c(FIA.plots.n,availableWater)
pct.soil.wat <- soilWaterContent / soil_depth
# pct.soil.wat <- meanSoilWater/soil_depth #soilWaterContent / soil_depth
pct.soil.wat.max <- soilWaterMax / soil_depth
pct.avail.wat <- availableWater / waterFull

#pdf("Los_Alamos_time_series_original_100.pdf", width = 7, height = 5, useDingbats = F)
# setwd("C:/Users/phenne/OneDrive - DOI/Presentations/IALE")
# png(filename = "Los_alamos_time_series.png", units = "in", width = 6, height = 5, res = 600, pointsize = 12)
plot(as.yearmon(dates$Date), pct.soil.wat * 100, col = "green", type = "l", lwd = 2, ylim = c(0,45), bty = "l", xlab = "Date (Year, month)", ylab=expression(Percent~soil~water~by~volume~(m^3/m^3)), las = 1, main = "Los Alamos, NM (2657 m a.s.l.)")
#lines(as.yearmon(dates$Date), pct.soil.wat.max * 100, col = "brown")
lines(LANDIS.station.soil.wat$Date, LANDIS.station.soil.wat$mean.soil.w *100, col = "blue")
#lines(as.yearmon(soil.water.crn.5$V1), soil.water.crn.5$SOIL_MOISTURE_5_DAILY * 100, col = "red")
#lines(as.yearmon(soil.water.crn.50$V1), soil.water.crn.50$SOIL_MOISTURE_50_DAILY * 100, col = "brown")
#lines(as.yearmon(soil.water.crn.20$V1), soil.water.crn.20$SOIL_MOISTURE_20_DAILY * 100, col = "orange")
lines(as.yearmon(soil.water.crn.100$V1), soil.water.crn.100$SOIL_MOISTURE_100_DAILY * 100, col = "black")
# lines(as.yearmon(dates$Date), pct.soil.wat.max * 100, col = "green", lwd=3)
#abline(h = c(waterEmpty/soil_depth*100,waterFull/soil_depth*100))
legend(x = as.yearmon("Jan 2010"), y = 49, legend = c("Modeled","Depth-weighted mean", "100 cm"), col =c("green","blue", "black"), lty = 1, lwd = 2, bty = 'n')
# dev.off()

lines(as.yearmon(dates$Date), pct.soil.wat.max * 100, col = "red") 

#read in CWD from terraclimate
library("dplyr")
cwd_tc <- read.csv("pet_time_series_single.csv") %>%
  mutate(date = as.yearmon(date),
         pet = pet * 0.1,
         aet = aet * 0.1,
         cwd = pet - aet) %>%
  filter(date %in% dates$Date)

plot(pet ~ date, cwd_tc, type = "l", col = "blue")
lines(pet*10 ~ dates$Date, col = "red")

plot(aet ~ date, cwd_tc, type = "l", col = "blue")
lines(AET*10 ~ dates$Date, col = "red")

plot(cwd ~ date, cwd_tc, type = "l", col = "blue")
lines(CWD*10 ~ dates$Date, col = "red")


soil.wat.lm <- lm(LANDIS.station.soil.wat$pct.soil.wat.max ~ LANDIS.station.soil.wat$mean.soil.w)
summary(soil.wat.lm)
mean.soil.w.r2 <- summary(soil.wat.lm)$adj.r.squared#Daily PWP r2 = 0.53, original adj r2 - 0.56

soil.wat20.lm <- lm(LANDIS.station20.soil.wat$pct.soil.wat.max ~ LANDIS.station20.soil.wat$SOIL_MOISTURE_20_DAILY)
summary(soil.wat20.lm)#Daily PWP adj R2 = 0.52; original adj R2 = 0.54
  
soil.wat50.lm <- lm(LANDIS.station50.soil.wat$pct.soil.wat.max ~ LANDIS.station50.soil.wat$SOIL_MOISTURE_50_DAILY)
summary(soil.wat50.lm)#Daily PWP adj R2 = 0.50; original adj R2 = 0.52

soil.wat100.lm <- lm((LANDIS.station100.soil.wat$pct.soil.wat.max) ~ (LANDIS.station100.soil.wat$SOIL_MOISTURE_100_DAILY))
summary(soil.wat100.lm)#Daily PWP adj R2 = 0.63; original adj R2 = 0.64

mean.50.lm <- lm(station.mean.50$SOIL_MOISTURE_50_DAILY ~ station.mean.50$mean.soil.w)
summary(mean.50.lm)

plot(LANDIS.station.soil.wat$mean.soil.w, LANDIS.station.soil.wat$pct.soil.wat.max, las =1, bty ="l
     ",las =1, bty ="l", xlim=c(0,0.5), ylim = c(0,0.5), xlab= expression(Observed~soil~water~(m^2/m^2)), ylab=expression(Simulated~soil~water~(m^2/m^2)), main = "Los_Alamos, CO (2561 m a.s.l.)")
abline(a = coef(soil.wat.lm))
text(0.1, 0.4, expression(r^2 == 0.53))

pdf("Los_Alamos_sim_v_50cm.pdf", width = 5, height = 5, useDingbats = F)
plot(LANDIS.station50.soil.wat$SOIL_MOISTURE_50_DAILY, LANDIS.station50.soil.wat$pct.soil.wat.max, las =1, bty ="l", xlim=c(0,0.5), ylim = c(0,0.5), xlab= expression(Observed~soil~water~(m^2/m^2)), ylab=expression(Simulated~soil~water~(m^2/m^2)), main = "Los_Alamos, CO (2561 m a.s.l.)")
abline(a = coef(soil.wat50.lm))
text(0.2, 0.3, expression(r^2~"="~0.4824))
dev.off()

pdf("Los_Alamos_sim_v_20cm.pdf", width = 5, height = 5, useDingbats = F)
plot(LANDIS.station20.soil.wat$SOIL_MOISTURE_20_DAILY, LANDIS.station20.soil.wat$pct.soil.wat.max, las =1, bty ="l", xlim=c(0,0.4), ylim = c(0,0.4), xlab= expression(Observed~soil~water~(m^2/m^2)), ylab=expression(Simulated~soil~water~(m^2/m^2)), main = "Los_Alamos, CO (2561 m a.s.l.)")
abline(a = coef(soil.wat20.lm))
text(0.2, 0.3, expression(r^2~"="~0.38))
dev.off()

#pdf("Los_Alamos_sim_v_100cm_original.pdf", width = 5, height = 5, useDingbats = F)
png(filename = "Los_alamos_sim_v_100cm.png", units = "in", width = 4, height = 4, res = 600, pointsize = 12)
plot(LANDIS.station100.soil.wat$SOIL_MOISTURE_100_DAILY*100, LANDIS.station100.soil.wat$pct.soil.wat.max*100, las =1, bty ="l", xlim=c(0,50), ylim = c(0,50), xlab= expression(Observed~soil~water~(m^2/m^2)), ylab=expression(Simulated~soil~water~(m^2/m^2)), main = "Los Alamos, NM (2657 m a.s.l.)")
abline(a = coef(soil.wat100.lm))
text(0.1, 0.4, expression(r^2~"="~0.64))
dev.off()

plot(station.mean.50$mean.soil.w,station.mean.50$SOIL_MOISTURE_50_DAILY)

soil.dates <- data.frame(seq(as.Date("2004/08/15"), by = "month", length.out = nrow(soil.water.mid)))
colnames(soil.dates) <- "dates"
soil.water.mid <- cbind(soil.water.mid, soil.dates)
tail(soil.water.mid,80)

plot(soil.water.mid[soil.water.mid$Year >= 2010, "dates"] ,soil.water.mid[soil.water.mid$Year >= 2010,"sm_50cm"], type = "l",col="red",ylim = c(0,0.8))
lines(LANDIS.soil.water$date, LANDIS.soil.water$pct.avail.H20, type = "l",col="green")
lines(LANDIS.soil.water$date, LANDIS.soil.water$pct.tot.H20, type = "l",col="blue")

AET <- c(FIA.plots.n,AET)
LANDIS_AET <- rbind(LANDIS_AET, AET)
PET <- c(FIA.plots.n,remainingPET)
LANDIS_PET <- rbind(LANDIS_PET, PET)

n <- n+1
if (n==3230){break}
print(paste0(n," ",Sys.time()))
}

LANDIS_availableWater[1:9,1:27]
min(LANDIS_availableWater[6,12:501])

ncol(LANDIS_availableWater)
plot(1:492,LANDIS_availableWater[1,10:501],type = "l")
lines(1:492,LANDIS_availableWater[5,10:501],col="blue")

LANDIS_availableWater.t <- t(LANDIS_availableWater)
FIA.plots.out <- cbind(FIA.plots, LANDIS_availableWater.t)

year.month <- paste0(rep(seq(1979,2019), each = 12),"_", seq(1,12,1)) 
colnames(LANDIS_availableWater) <- c("state", "invyr", "plot", "lat", "lon", "slope", "aspect", "litterBiomass", "standingBiomass", year.month) 
write.csv(LANDIS_availableWater, "FIA_soil_water.csv")
colnames(LANDIS_AET) <- c("state", "invyr", "plot", "lat", "lon", "slope", "aspect", "litterBiomass", "standingBiomass", year.month) 
write.csv(LANDIS_AET, "FIA_AET.csv")
colnames(LANDIS_PET) <- c("state", "invyr", "plot", "lat", "lon", "slope", "aspect", "litterBiomass", "standingBiomass", year.month) 
write.csv(LANDIS_PET, "FIA_PET.csv")
head(LANDIS_availableWater,48)
head(LANDIS_availableWater.mid,48)
head(LANDIS_PET,1)
head(remainingPET,48)
head(1-(AET/remainingPET),36)
cbind(availableWater1,availableWater)

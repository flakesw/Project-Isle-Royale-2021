library("amerifluxr") #to download AmeriFlux data
library("REddyProc") #to partition NEE into GPP and Reco
library("nls2") #fit nonlinear curve
library("minpack.lm")
library("tidyverse")
library("data.table")


#this process is a little janky, but essentially I went through all the sites
# by hand to check if there was GPP data. If so, great!
# If not, I used REddyProc to estimate GPP from NEE. This is a sort of involved
# process that requires some data proofing. I just did a quick and dirty job,
# but had to reject several sites because of missing data or inability to calculate
# some of the needed variables. 

#I used this script interactively to go through a list of sites and export their 
# data (including GPP) as a csv. Then I use another script, get_moisture_code_from_ameriflux.R, 
#to summarize the data for each functional group.


sites <- amf_site_info()
sites_dt <- data.table::as.data.table(sites)

pander::pandoc.table(sites_dt[c(1:3), ])

pander::pandoc.table(sites_dt[, .N])

pander::pandoc.table(sites_dt[!is.na(DATA_START), .N])

pander::pandoc.table(sites_dt[!is.na(DATA_START), .N, by = .(DATA_POLICY)])

pander::pandoc.table(sites_dt[, .N, by = "IGBP"])

pander::pandoc.table(sites_dt[!is.na(DATA_START), .N, by = "IGBP"][order(IGBP)])

pander::pandoc.table(sites_dt[!is.na(DATA_START), .N, by = .(IGBP, DATA_POLICY)][order(IGBP)])

crop_ls <- sites_dt[IGBP %in% c("CSH", "DBF", "DNF", "EBF", "ENF", "MF", "OSH", "SAV", "WET", "WSA") &
                      !is.na(DATA_START) &
                      LOCATION_LAT > 41 &
                      DATA_POLICY == "CCBY4.0",
                    .(SITE_ID, SITE_NAME, DATA_START, DATA_END)]
pander::pandoc.table(crop_ls[c(1:10),])



#download bif data
floc1 <- amf_download_bif(
  user_id = "flakesw",
  user_email = "swflake@ncsu.edu",
  data_policy = "CCBY4.0",
  agree_policy = TRUE,
  intended_use = "other",
  intended_use_text = "amerifluxr package demonstration",
  out_dir = tempdir(),
  verbose = TRUE,
  site_w_data = TRUE
)

bif <- amf_read_bif(file = floc1)


#get metadata
metadata_aval <- data.table::as.data.table(amf_list_metadata())
pander::pandoc.table(metadata_aval[c(1:3), c(1:10)])

metadata_aval_sub <- as.data.table(amf_list_metadata(site_set = crop_ls$SITE_ID))

# down-select cropland & grassland sites by interested BADM group,
#  e.g., canopy height (GRP_HEIGHTC)
# crop_ls2 <- metadata_aval_sub[GRP_SWC > 0, .(SITE_ID, GRP_HEIGHTC)][order(-GRP_HEIGHTC)]
# pander::pandoc.table(crop_ls2[c(1:10), ])

## what sort of data is available
data_aval <- data.table::as.data.table(amf_list_data(site_set = crop_ls$SITE_ID))
pander::pandoc.table(data_aval[c(1:10), ])

data_aval_gpp <- data_aval[data_aval$BASENAME %in% c("GPP"),
                           .(SITE_ID, BASENAME)]
data_aval_gpp <- data_aval[data_aval$BASENAME %in% c("NEE"),
                           .(SITE_ID, BASENAME)]
data_aval_swc <- data_aval[data_aval$BASENAME %in% c("SWC"),
                           .(SITE_ID, BASENAME)]


data_sum <- amf_summarize_data(site_set = crop_ls$SITE_ID,
                               var_set = c("GPP", "SWC"))
pander::pandoc.table(data_sum[c(1:10), ])


# sites_with_gpp <- crop_ls[crop_ls$SITE_ID %in% data_aval_gpp$SITE_ID, ]
sites_with_nee <- crop_ls[crop_ls$SITE_ID %in% data_aval_gpp$SITE_ID, ]
sites_with_swc <- crop_ls[crop_ls$SITE_ID %in% data_aval_swc$SITE_ID, ]
# write.csv(sites_with_swc, "ameriflux_gpp_swc.csv")

sites_with_nee

#edited csv
sites_to_use <- read.csv("./Parameterization/Parameterization data/ameriflux data/ameriflux_gpp_swc_edit.csv")
sites <- sites_to_use[sites_to_use$Use, "SITE_ID"]


# subset by target Site ID
bif_site <- bif[bif$SITE_ID == sites[i], ]
# pander::pandoc.table(bif[c(1:15), ])
bif_site[bif_site$VARIABLE_GROUP == "GRP_SITE_DESC", ]$DATAVALUE
#it's also worth checking here what the SWC method was -- it should be under the variable SWC_UNIT

#-------------------------------------------------------------------------------

#########
#Download data

getOption('timeout')
options(timeout=300)

floc2 <- amf_download_base(user_id = "flakesw",
                           user_email = "swflake@ncsu.edu",
                           site_id = sites[i],
                           data_product = "BASE-BADM",
                           data_policy = "LEGACY",
                           agree_policy = TRUE,
                           intended_use = "model",
                           intended_use_text = "calibrate productivity in a LANDIS-II forest landscape model",
                           verbose = TRUE,
                           out_dir = tempdir())


base_raw <- amf_read_base(file = floc2,
                      unzip = TRUE,
                      parse_timestamp = TRUE)

names(base_raw)
names(base_raw[grep("NEE", names(base_raw))])
names(base_raw[grep("TA_", names(base_raw))])
names(base_raw[grep("GPP", names(base_raw))])


#*******************************************************************************
# If no GPP data, use REddyProv to estimate GPP, below:
#*******************************************************************************

base <- base_raw %>%
  dplyr::mutate(NEE = rowMeans(select(., contains("NEE")), na.rm = TRUE),
                Rg = rowMeans(select(., contains("SW_IN")), na.rm = TRUE),
                Tair = rowMeans(select(., contains("TA_")), na.rm = TRUE),
                # Tair = TA,
                Ustar = rowMeans(select(., contains("USTAR")), na.rm = TRUE),
                rH = rowMeans(select(., contains("RH")), na.rm = TRUE)) %>%
  mutate(Rg = ifelse(Rg < 0, 0, Rg)) %>%
  mutate(rH = ifelse(rH > 100, 100, rH)) %>%
  dplyr::select(YEAR:TIMESTAMP_END, NEE, Rg, Tair, Ustar, rH)

base$VPD <- fCalcVPDfromRHandTair(base$rH, base$Tair)

EddyDataWithPosix <- fConvertTimeToPosix(
  base, 'YMDHM',Year = 'YEAR', Month = 'MONTH', Day = 'DAY', Hour = 'HOUR', Min = 'MINUTE') %>%
  mutate(NEE = ifelse(NEE < -50, NA, NEE),
         VPD = ifelse(VPD < 0, NA, VPD)) %>%
  filterLongRuns("NEE") 

EddyDataWithPosix$DateTime <- EddyDataWithPosix$DateTime + lubridate::minutes(15)

#if days need cleaning up
#EddyDataWithPosix <- EddyDataWithPosix[!(EddyDataWithPosix$YEAR == 2006 & EddyDataWithPosix$DOY %in% c(364, 365)), ]


#+++ Initalize R5 reference class sEddyProc for post-processing of eddy data
#+++ with the variables needed for post-processing later
EProc <- sEddyProc$new(
  sites[i], EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))

#see if there are large gaps
EProc$sPlotFingerprintY('NEE', Year = unique(base$YEAR)[1])

#set location
EProc$sSetLocationInfo(LatDeg = as.numeric(bif_site[bif_site$VARIABLE == "LOCATION_LAT", "DATAVALUE"]), 
                       LongDeg = as.numeric(bif_site[bif_site$VARIABLE == "LOCATION_LONG", "DATAVALUE"]), 
                       TimeZoneHour = 1)  


# #estimate ustar thresholds
# this can take a while!
EProc$sEstimateUstarScenarios(
  nSample = 100L, probs = c(0.05, 0.5, 0.95))
#check on data
EProc$sGetEstimatedUstarThresholdDistribution()
#set ustar thresholds to calculated seasonal thresholds
EProc$useSeaonsalUStarThresholds()
#check on data
EProc$sGetUstarScenarios()

#fill gaps
#takes a few minutes
EProc$sMDSGapFillUStarScens('NEE', FillAll = FALSE)
EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA)     
EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA)     
EProc$sFillVPDFromDew() # fill longer gaps still present in VPD_f

grep("NEE_.*_f$",names(EProc$sExportResults()), value = TRUE)
grep("NEE_.*_fsd$",names(EProc$sExportResults()), value = TRUE)
#not gap-filled
EProc$sPlotFingerprintY('NEE', Year = unique(base$YEAR)[5])
#gap-filled
EProc$sPlotFingerprintY('NEE_uStar_f', Year = unique(base$YEAR)[5])


#Partition into GPP and Reco
EProc$sMRFluxPartitionUStarScens()

EProc$sPlotFingerprintY('GPP_uStar_f', Year = unique(base$YEAR)[5])


#------------------------------------------------------------------------------
base$GPP_PI_F <- EProc$sExportResults()$GPP_uStar_f
base$SWC <-   base_raw %>%
  dplyr::select(contains("SWC_")) %>%
  rowMeans(na.rm = TRUE)

write.csv(base, paste0("./Parameterization/necn_parameterize/ameriflux_processed/", sites[i], "_processed.csv"))

#aggregate to daily -- average SWC
base_daily <- base %>%
  group_by(YEAR, DOY) %>%
  summarise(swc = mean(c(SWC), na.rm = TRUE),
            gpp = mean(c(GPP_PI_F), na.rm = TRUE))


#------------------------------------------------------------------------------

##Observed_Relative_production will be your input data here from the flux tower
# realdat <- base
# realdat$maxrelGPP <- base$GPP_PI_F/quantile(base$GPP_PI_F, 0.99, na.rm = TRUE)
# realdat$maxrelGPP <- ifelse(realdat$maxrelGPP < 0, NA, realdat$maxrelGPP)
# realdat$swc <- realdat$SWC_1_1_1 / 100

realdat <- base_daily
realdat$maxrelGPP <- realdat$gpp/quantile(realdat$gpp, 0.99, na.rm = TRUE)
realdat$maxrelGPP <- ifelse(realdat$maxrelGPP < 0, NA, realdat$maxrelGPP)
realdat$swc <- realdat$swc / 100



Observed_Relative_production<- realdat$maxrelGPP

swc<-sort(runif(1000, 0, 1))

out_data <- realdat[0, ]
for(j in 1:200){ #each half-percent of volumetric water content
  sub_data <- realdat[which(realdat$swc > ((j-1) * 0.005) & realdat$swc < j*0.05), ]
  
  val <- quantile(sub_data$maxrelGPP, .95, na.rm = TRUE)
  
  temp_data <- sub_data[which(sub_data$maxrelGPP > val), ]
  out_data <- rbind(out_data, temp_data)
}

# use percentile data
# Observed_Relative_production<- out_data$maxrelGPP
# soilwat <- out_data$swc

#or use all data
Observed_Relative_production<- realdat$maxrelGPP
soilwat <- realdat$swc

#plot raw data
plot(soilwat, Observed_Relative_production, col="blue", type="p",
     ylim = c(0,1),
     xlim = c(0,1))


# Function to fit coefficients to water curve. 

#original coefficients from Shelby
# moisture_1<- 0.13 #soilwater with maximum gpp
# moisture_2<- 1 #maximum soilwater with any GPP -- changes steepness of decline in gpp
# moisture_3<- 3 #higher values -- sharper dip down towards zero
# moisture_4<- 15

#starting coefficients
moisture_1<- 0.13 #soilwater with maximum gpp
moisture_2<- 1#maximum soilwater with any GPP -- changes steepness of decline in gpp
moisture_3<- 3 #higher values -- sharper dip down towards zero
moisture_4<- 15

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
                     control = list(maxiter = 1000))

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



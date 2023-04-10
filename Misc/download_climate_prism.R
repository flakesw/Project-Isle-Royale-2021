

#-------------------------------------------------------------------------------
# download PRISM data for baseline
fabric <- webdata(webdatasets[1])
variables(fabric) <- c("ppt", "tmx", "tmn")
fabric@times <- as.POSIXct(c("1950-01-01", "2006-12-01"))
knife <- webprocess(wait = TRUE, email = "your.address@email.com")
query(knife, 'algorithms')

# area grid statistics are the default, but we can change it if we want to (we don't)
algorithm(knife) <- list('Area Grid Statistics (weighted)' = 
                           "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")

#I think this is the best way to set it? The geoknife documentation isn't clear on
# if there's a better way; might be a feature in development
knife@processInputs$STATISTICS <- c("MEAN", "VARIANCE", "STD_DEV") #what statistics do we want?

testjob <- geoknife(stencil, fabric, knife)

#get information on our job in process
check(testjob) #is the job happening?
running(testjob)
error(testjob)
successful(testjob)

# cancel the job if we need to
# testjob <- cancel(testjob)

#extract the data from our completed job
test <- result(testjob)
head(test)
tail(test)

#-------------------------------------------------------------------------------
# now to run it for the full set of data that we need
# we just have one ecoregion for now, TODO figure out how to do several ecoregions

# I had the job fail when I did all variables at once, so let's split them up into separate jobs
# we can't submit several jobs at the same time, so we'll put them in a loop and
# wait until one job is done before starting the next one.
# This takes a variable amount of time -- sometimes 10 minutes or sometimes an hour-ish

knife@email <- "your.address@email.com" #I just replaced this so I don't get emails about other people's data

job_results <- list()

for(i in 1:length(varList)){
  #set the fabric for a new variable, but keep everything else the same (i.e. the stencil and knife)
  variables(fabric) <- varList[i]
  print(varList[i])
  job <- geoknife(stencil, fabric, knife)
  if(error(job)){
    break
    check(job)
  }
  
  job_results[[i]] <- result(job)
}


#save your work!
saveRDS(job_results, file = "climate_raw.RDS")

#The data are in a long format -- not quite what we want
str(job_results[[1]]) 

#check on one of our datasets
ppt <- job_results[[1]]

#reshape the data into the format we need
job_results_reform <- job_results %>% 
  #widen data format
  map(., function(x) tidyr::pivot_wider(data = x,
                                        names_from = "statistic",
                                        values_from = "1")) %>%
  #create a TIMESTEP column and add some formatting junk to the end of the date. 
  # TODO figure out a better way to do this
  map(., function(x) dplyr::mutate(x, TIMESTEP = paste0(as.character(DateTime), "T00:00:00Z")))

job_results_reform[[2]]$MEAN <- job_results_reform[[2]]$MEAN - 273.15 #convert from kelvin to celsius
job_results_reform[[3]]$MEAN <- job_results_reform[[3]]$MEAN - 273.15 #convert from kelvin to celsius

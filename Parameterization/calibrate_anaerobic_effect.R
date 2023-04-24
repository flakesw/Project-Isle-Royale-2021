# recreate anaerobic effect from NECN/century

anerob_eff <- function(ratioPrecipPET, pet, drain){
  
  aneref1 <- 1.5
  aneref2 <- 3
  aneref3 <- .008
  
  anerob <- 1
  
  if (ratioPrecipPET > aneref1){
     xh2o = (ratioPrecipPET - aneref1) * pet * (1.0 - drain) #pet has no effect!
    
    if (xh2o > 0){
       newrat = aneref1 + (xh2o / pet)
       slope = (1.0 - aneref3) / (aneref1 - aneref2)
      anerob = 1.0 + slope * (newrat - aneref1)
    }
    
    if (anerob < aneref3)
      anerob = aneref3
  }
  
  return(anerob)
}

anerob_eff(3, 20, 0) #not sensitive to PET




test <- rast("./Models/landis_test/mc_test - browse - linear - annual - subset/NECN/AnaerobicEffect-30.img")
plot(test)
hist(values(test))



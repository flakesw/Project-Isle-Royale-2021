#soil respiration T sensitivity



t <- seq(0, 20, length.out = 100)
resp <- 0 + 0.125 * exp(0.06 * t)

plot(resp ~ t)




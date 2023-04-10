#LAI establishment curves

#libraries
library("tidyverse")
library("rFIA")

#options
options(scipen = 999)


states <- c("MN", "WI", "MI")
states.index <- seq(from=1, to=3)
tables <- c("TREE","SEEDLING")
tables.index <- c(1,2)
directory <- "D:/Data/fia/rFIA_downloads"


#species reference data
sp_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_SPECIES.csv")

#import fia data
#using rFIA package automatically knits the tables together; you could also
# use readr::read_csv() or import several csvs then rbind() 
fia <- readFIA(dir = directory,
                tables = c("TREE", "SEEDLING", "COND", "PLOT"),
                states = states)

trees <- fia$TREE
seedlings <- fia$SEEDLING

# Filter what plots to use -----------------------------------------------------
# Not all plots are in forest, some have been recently treated, etc, and we need
# to filter those out





# Assigns parameters to each species group code --------------------------------
#TODO is there somewhere else to find this data in a tidier format?

hardwoods <- seq(from=25, to=43)
hardwood.S <- c(0.75, 0.81, 0.74, 0.77, 0.77, 0.83, 0.86, 0.83, 0.88, 0.82, 0.83, 0.82, 0.795, 0.88, 0.9, 0.91, 0.861, 0.78, 0.833)
softwoods <- seq(from=1, to=9)
SPGRPCDs <- c(1,2,3,4,5,6,7,8,9,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43)
SPGRPs<-c("longleaf_and_slash_pines","loblolly_pines","other_yellow_pines",
          "eastern_white_and_red_pines","Jack_pine","spruce_and_balsam_fir",
          "eastern_hemlock","cypress","other_softwoods","white_oaks_1",
          "red_oaks_1","white_oaks_2","red_oaks_2","hickory","yellow_birch",
          "hard_maples","soft_maples","beech","sweetgum","tupelo","ash",
          "cottonwood_and_aspen","basswood","yellow-poplars","black_walnut",
          "other_eastern_soft_hardwoods","other_easter_hard_hardwoods","eastern_noncommerical_hardwoods")
SPGRPCDs.index <- seq(from=1, to=length(SPGRPCDs))
S.table <- cbind(hardwoods, hardwood.S)
S.table <- as.data.frame(S.table)
colnames(S.table) <- c("SPGRPCD","Model.S")

# Calculate leaf area per tree -------------------------------------------------
# TODO refactor this, it's pretty bad. Hard to read and RAM-intensive

#TODO adjust for stocking!!
trees$DIA.CM <- trees$DIA * 2.54
trees$HT.M <- trees$HT / 3.2808
trees$PLOT.YEAR <- paste(trees$PLT_CN, trees$INVYR, sep=".")
trees <- trees[trees$STATUSCD == 1,]
trees.HARDWOOD <- trees[trees$SPGRPCD %in% hardwoods,]
trees.HARDWOOD <- merge(trees.HARDWOOD, S.table)
#TODO what are the units here?
trees.HARDWOOD$LEAF.AREA.LN <- 0.2102 + (0.0586*trees.HARDWOOD$DIA.CM) + (4.0202*trees.HARDWOOD$Model.S) #TODO revisit this formula
trees.HARDWOOD$LEAF.AREA <- exp(trees.HARDWOOD$LEAF.AREA.LN) / 672.469876708172
trees.HARDWOOD$biomass = trees.HARDWOOD$DRYBIO_AG 
trees.HARDWOOD <- filter(trees.HARDWOOD,
                         LEAF.AREA < quantile(LEAF.AREA, 0.9, na.rm = TRUE),
                         biomass < 5000)

ggplot(trees.HARDWOOD) + 
  geom_hex(mapping = aes(x = biomass, y = LEAF.AREA))

SAPPS.PLOT.HARDWOOD.LEAFAREA <- aggregate(trees.HARDWOOD$LEAF.AREA, by=list(PLOT.YEAR=trees.HARDWOOD$PLOT.YEAR), FUN=sum)
plot_hardwood_biomass <- aggregate(trees.HARDWOOD$LEAF.AREA, by=list(PLOT.YEAR=trees.HARDWOOD$PLOT.YEAR), FUN=sum)
colnames(SAPPS.PLOT.HARDWOOD.LEAFAREA) <- c("PLOT.YEAR","HARDWOOD.LEAFAREA")
trees.SOFTWOOD <- trees[trees$SPGRPCD %in% softwoods,]
trees.SOFTWOOD$LEAF.BIOMASS.KG <- 0.0621*(trees.SOFTWOOD$DIA.CM^1.505) #TODO this allometry should differ by species
trees.SOFTWOOD$LEAF.BIOMASS.G <- trees.SOFTWOOD$LEAF.BIOMASS.KG * 1000
trees.SOFTWOOD$SURFACEAREA.TOTAL <- trees.SOFTWOOD$LEAF.BIOMASS.G / 227
trees.SOFTWOOD$SURFACEAREA.HALF <- trees.SOFTWOOD$SURFACEAREA.TOTAL / 2
SAPPS.PLOT.SOFTWOOD.LEAFAREA <- aggregate(trees.SOFTWOOD$SURFACEAREA.HALF, by=list(PLOT.YEAR=trees.SOFTWOOD$PLOT.YEAR), FUN=sum)
colnames(SAPPS.PLOT.SOFTWOOD.LEAFAREA) <- c("PLOT.YEAR","SOFTWOOD.LEAFAREA")
SAPPS.PLOT.TOTAL.LEAFAREA <- merge(SAPPS.PLOT.HARDWOOD.LEAFAREA, SAPPS.PLOT.SOFTWOOD.LEAFAREA, all=T)
SAPPS.PLOT.TOTAL.LEAFAREA[is.na(SAPPS.PLOT.TOTAL.LEAFAREA)] <- 0
SAPPS.PLOT.TOTAL.LEAFAREA$TOTAL.LEAFAREA <- SAPPS.PLOT.TOTAL.LEAFAREA$HARDWOOD.LEAFAREA + SAPPS.PLOT.TOTAL.LEAFAREA$SOFTWOOD.LEAFAREA
SAPPS.PLOT.TOTAL.LEAFAREA$PLOT.LAI <- SAPPS.PLOT.TOTAL.LEAFAREA$TOTAL.LEAFAREA / 672.469876708172
seedlings$PLOT.YEAR <- paste(seedlings$PLT_CN, seedlings$INVYR, sep=".")

#species we want to model
spp_to_use <- c("ABBA",
                "ACRU",
                "ACSA3",
                "ACSP2",
                #"ALIN2", #not enough data for alder
                "BEAL2",
                "BEPA",
                #"CRDO2", #Crataegus douglasii doesn't have an entry in FIA, so we'll omit it for this
                "FRNI",
                "LALA",
                "PIBA2",
                "PIGL",
                "PIMA",
                "PIST",
                "PIRE",
                "POBA2",
                "POGR4",
                "POTR5",
                "PRVI",
                "QURU",
                "SODE3",
                "THOC2")

#get SPCD for each species. This will be different depending on how the species are named,
#but we want a crosswalk from the names used in LANIS to SPCD somehow
#In this case, I used the USDA PLANTS symbol, which is found in the FIA species reference table
# to crosswalk to FIA species code
spcd_to_use <- sp_ref[sp_ref$SPECIES_SYMBOL %in% spp_to_use, ] %>%
  dplyr::arrange(SPECIES_SYMBOL) %>%
  as.data.frame() %>%
  `[[`("SPCD")

#TODO refactor with group_by %>% summarise
for (i in 1:length(spcd_to_use)){
  SPCD <- spcd_to_use[i]
  Table <- seedlings[seedlings$SPCD == SPCD,]
  
  if(nrow(Table) == 0) next #this breaks the rest of the code, because further down expects a column for every species
  
  Sums <- aggregate(Table$TREECOUNT, by=list(PLOT.YEAR = Table$PLOT.YEAR), FUN=sum)
  
  colnames(Sums) <- c("PLOT.YEAR", spp_to_use[i])
  SAPPS.PLOT.TOTAL.LEAFAREA <- merge(SAPPS.PLOT.TOTAL.LEAFAREA, Sums, all.x = T)
}

#if a species is absent from a plot, set values to 0
SAPPS.PLOT.TOTAL.LEAFAREA[is.na(SAPPS.PLOT.TOTAL.LEAFAREA)] <- 0
#only pull out plots with LAI < 20
SAPPS.PLOT.TOTAL.LEAFAREA <- SAPPS.PLOT.TOTAL.LEAFAREA[which(SAPPS.PLOT.TOTAL.LEAFAREA$PLOT.LAI < 20), ]
hist(SAPPS.PLOT.TOTAL.LEAFAREA$PLOT.LAI)
write.csv(SAPPS.PLOT.TOTAL.LEAFAREA, file = paste("SAPPS.PLOT.TOTAL.LEAFAREA.csv", sep=""))

# assign seedlings to different LAI bins, depending on the plot LAI where they're
# found
mins <- seq(from=0, to= 19.8, by =0.2)
maxs <- seq(from=0.2, to = 20.0, by=0.2)
histogram.index <- seq(from=1, to=100)
SAPPS.histogram <- as.data.frame(cbind(mins, maxs))
colnames(SAPPS.histogram) <- c("min","max")

#TODO refactor this loop

for (i in 1:length(spp_to_use)){
  SP.needed <- spp_to_use[i]
  columns.needed.vec <- c("PLOT.LAI", SP.needed)
  columns.needed <- SAPPS.PLOT.TOTAL.LEAFAREA[names(SAPPS.PLOT.TOTAL.LEAFAREA)[names(SAPPS.PLOT.TOTAL.LEAFAREA) %in% columns.needed.vec]]
  mean.inrange <- c()
  for (j in histogram.index){
    min <- mins[j]
    max <- maxs[j]
    subset <- columns.needed[columns.needed$PLOT.LAI >= min & columns.needed$PLOT.LAI < max,]
    count <- sum(subset[, 2]>0)
    total_plots <- nrow(subset)
    # final.column <- subset[names(subset)[names(subset) %in% SP.needed]]
    # final.column.vec <- as.vector(final.column)
    #changed from histogram being mean # of seedlings/plot, to percent of plots 
    # within LAI bin occupied by the species
    mean.inrange[j] <- count / total_plots #mean(final.column[,SP.needed])
  }
  SAPPS.histogram[,SP.needed] <- mean.inrange
}


#test <- SAPPS.histogram
#backup <- SAPPS.histogram

#draw histograms for each species
for (i in 1:length(spp_to_use)){
  SP.needed <- spp_to_use[i]
  SPCD.needed <- spcd_to_use[i]
  barplot(height = SAPPS.histogram[,SP.needed], names.arg = SAPPS.histogram$max, main = SP.needed, xlab = "LAI",ylab="Seedlings/plot")
}


#estimate parameters of lognormal distribution for each species
#why? TODO
library(spatstat)
species <- c()
weighted.LAI.mean <- c()
weighted.LAI.var <- c()
weighted.LAI.SD <- c()
weighted.LAI.log.mean <- c()
weighted.LAI.log.var <- c()
weighted.LAI.log.SD <- c()
Table.NoOutliers <- SAPPS.PLOT.TOTAL.LEAFAREA[SAPPS.PLOT.TOTAL.LEAFAREA$PLOT.LAI<20,]
Table.NoOutliers$PLOT.LAI <- ifelse(Table.NoOutliers$PLOT.LAI < 0.1,
                                    0.1,
                                    Table.NoOutliers$PLOT.LAI)
Table.NoOutliers$PLOT.LAI.LOG <- log(Table.NoOutliers$PLOT.LAI)
for (i in 1:length(spp_to_use)){
  SP.needed <- spp_to_use[i]
  PLOT.LAI <- Table.NoOutliers$PLOT.LAI
  PLOT.LAI.LOG <- Table.NoOutliers$PLOT.LAI.LOG
  PLOT.SEEDLINGS <- Table.NoOutliers[,SP.needed]
  PLOT.SEEDLINGS.TOTAL <- sum(PLOT.SEEDLINGS)
  weights <- PLOT.SEEDLINGS/PLOT.SEEDLINGS.TOTAL #Number of seedlings of the species divided by total seedlings of the species
  Weighted.LAI.mean <- weighted.mean(x = PLOT.LAI, w = weights)
  Weighted.LAI.var <- weighted.var(x=PLOT.LAI, w=weights)
  Weighted.LAI.log.mean <- weighted.mean(x = PLOT.LAI.LOG, w = weights)
  Weighted.LAI.log.var <- weighted.var(x=PLOT.LAI.LOG, w=weights)
  species[i] <- SP.needed
  weighted.LAI.mean[i] <- Weighted.LAI.mean
  weighted.LAI.var[i] <- Weighted.LAI.var
  weighted.LAI.SD[i] <- sqrt(Weighted.LAI.var)
  weighted.LAI.log.mean[i] <- Weighted.LAI.log.mean
  weighted.LAI.log.var[i] <- Weighted.LAI.log.var
  weighted.LAI.log.SD[i] <- sqrt(Weighted.LAI.log.var)
}

weighted.mean.LAI.table <- data.frame(species, weighted.LAI.mean, weighted.LAI.var, weighted.LAI.SD, weighted.LAI.log.mean, weighted.LAI.log.var, weighted.LAI.log.SD)
grid <- seq(0,20,.1)

#example data
plot(grid,dlnorm(grid,-0.08653947,1.3804560),type="l",xlab="LAI",ylab="f(x)", col="blue")
lines(grid,dlnorm(grid,1.39746695, 0.7739607), col="purple")
lines(grid,dlnorm(grid,1.94351101, 0.6928515), col="red")
legend("topright", legend=c("Loblolly Pine","White Oak","Sugar Maple"), col=c("blue","purple","red"), lty=1)

#plot distribution for each species calculated
for (i in 1:length(spp_to_use)){
  SP.needed <- weighted.mean.LAI.table$species[i]
  logmean <- weighted.mean.LAI.table$weighted.LAI.log.mean[i]
  logsd <- weighted.mean.LAI.table$weighted.LAI.log.SD[i]
  plot(grid,dlnorm(grid,logmean,logsd),type="l",xlab="LAI",ylab="f(x)", col="blue", main=SP.needed, ylim=c(0,0.8))
}


#what breaks should we use?
#Here, we split up the data into categories based on quantiles; this might not be appropriate
# if, e.g., you have really long tails that you might want to incorporate rather 
# than collapsing into one category
hist(Table.NoOutliers$PLOT.LAI)
bins <- quantile(Table.NoOutliers$PLOT.LAI, c(1/6,2/6,3/6,4/6,5/6,1))

#bin up establishment based on the lognormal pdfs calculated for each spcies
cdfcolumns <- c("species","class.0","class.1","class.2","class.3","class.4","class.5")
species.column <- c()
shade.0 <- c()
shade.1 <- c()
shade.2 <- c()
shade.3 <- c()
shade.4 <- c()
shade.5 <- c()
index <- seq(from=1, to=length(weighted.mean.LAI.table$species))
for (i in index){
  species <- weighted.mean.LAI.table$species[i]
  log.sd <- weighted.mean.LAI.table$weighted.LAI.log.SD[i]
  log.mean <- weighted.mean.LAI.table$weighted.LAI.log.mean[i]
  species.column[i] <- species
  shade.0[i] <- plnorm(q = bins[1], meanlog=log.mean, sdlog =  log.sd)
  shade.1[i] <- plnorm(q = bins[2], meanlog=log.mean, sdlog =  log.sd) - plnorm(q = bins[1], meanlog=log.mean, sdlog =  log.sd)
  shade.2[i] <- plnorm(q = bins[3], meanlog=log.mean, sdlog =  log.sd) - plnorm(q = bins[2], meanlog=log.mean, sdlog =  log.sd)
  shade.3[i] <- plnorm(q = bins[4], meanlog=log.mean, sdlog =  log.sd) - plnorm(q = bins[3], meanlog=log.mean, sdlog =  log.sd)
  shade.4[i] <- plnorm(q = bins[5], meanlog=log.mean, sdlog =  log.sd) - plnorm(q = bins[4], meanlog=log.mean, sdlog =  log.sd)
  shade.5[i] <- 1 - plnorm(q = bins[6], meanlog=log.mean, sdlog =  log.sd)
}
cdf.table <- data.frame(species.column, shade.0, shade.1, shade.2, shade.3, shade.4, shade.5)
colnames(cdf.table) <- cdfcolumns

write.csv(cdf.table, "lai_establishment.csv")


#what species should get assigned to which shade tolerance classes?

weighted.mean.LAI.table$median_shade <- exp(weighted.mean.LAI.table$weighted.LAI.log.mean)
# a few ways to define the shade tolerance classes
# shade_quantiles <- quantile(weighted.mean.LAI.table$median_shade, c(0, .2, .4, .6, .8, 1))
# shade_quantiles <- seq(min(weighted.mean.LAI.table$median_shade), 
#                        max(weighted.mean.LAI.table$median_shade), 
#                        length.out = 6)
shade_quantiles = c(0, bins[-length(bins)])
weighted.mean.LAI.table$shade_class <- cut(weighted.mean.LAI.table$median_shade, 
                                           breaks = shade_quantiles,
                                           labels = FALSE,
                                           include.lowest = TRUE)

weighted.mean.LAI.table[, c("species", "shade_class")]



### recalculate LAI table for classes


#bin up establishment based on the lognormal pdfs calculated for each spcies
cdfcolumns <- c("class","class.0","class.1","class.2","class.3","class.4","class.5")

shade.0 <- c()
shade.1 <- c()
shade.2 <- c()
shade.3 <- c()
shade.4 <- c()
shade.5 <- c()
index <- seq(from=1, to=5)
for (i in index){
  subset <- weighted.mean.LAI.table[which(weighted.mean.LAI.table$shade_class == i), ]
  
  log.sd <- mean(subset$weighted.LAI.log.SD)
  log.mean <- mean(subset$weighted.LAI.log.mean)

  shade.0[i] <- plnorm(q = bins[1], meanlog=log.mean, sdlog =  log.sd)
  shade.1[i] <- plnorm(q = bins[2], meanlog=log.mean, sdlog =  log.sd) - plnorm(q = bins[1], meanlog=log.mean, sdlog =  log.sd)
  shade.2[i] <- plnorm(q = bins[3], meanlog=log.mean, sdlog =  log.sd) - plnorm(q = bins[2], meanlog=log.mean, sdlog =  log.sd)
  shade.3[i] <- plnorm(q = bins[4], meanlog=log.mean, sdlog =  log.sd) - plnorm(q = bins[3], meanlog=log.mean, sdlog =  log.sd)
  shade.4[i] <- plnorm(q = bins[5], meanlog=log.mean, sdlog =  log.sd) - plnorm(q = bins[4], meanlog=log.mean, sdlog =  log.sd)
  shade.5[i] <- 1 - plnorm(q = bins[6], meanlog=log.mean, sdlog =  log.sd)
}
cdf.table <- data.frame(index, shade.0, shade.1, shade.2, shade.3, shade.4, shade.5)
colnames(cdf.table) <- cdfcolumns

write.csv(cdf.table, "lai_establishment.csv")



#makes a figure, but in a really awkward way
#TODO

species.rep <- c("ABBA","PIST","PIMA")
for (i in 1:length(species.rep)){
  species <- species.rep[i]
  neededrow <- cdf.table[cdf.table$species.column %in% species,]
  probabilities <- c()
  probabilities[1] <- neededrow$shade.0[1]
  probabilities[2] <- neededrow$shade.1[1]
  probabilities[3] <- neededrow$shade.2[1]
  probabilities[4] <- neededrow$shade.3[1]
  probabilities[5] <- neededrow$shade.4[1]
  probabilities[6] <- neededrow$shade.5[1]
  assign(x=paste(species,".cdfs",sep=""), value=probabilities)
}

bargraph.data <- data.frame(ABBA.cdfs, PIST.cdfs, PIMA.cdfs)
shadeclasses <- c("0","1","2","3","4","5")
row.names(bargraph.data) <- shadeclasses
bargraph.colors <- c("blue","chocolate","purple")
bargraph.legend <- c("Balsam fir","White pine","Black spruce")
barplot(t(as.matrix(bargraph.data)), main="Probability Densities under LAI Ranges",xlab="Shade Class", col= bargraph.colors, legend = bargraph.legend, beside=T, ylim=c(0,1.2))


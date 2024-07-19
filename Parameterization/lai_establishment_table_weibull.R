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
                tables = c("TREE", "SEEDLING"),
                states = states)

trees <- fia$TREE
seedlings <- fia$SEEDLING

# Filter what plots to use -----------------------------------------------------
# Not all plots are in forest, some have been recently treated, etc, and we need
# to filter those out





# Assigns parameters to each species group code --------------------------------

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


trees$DIA.CM <- trees$DIA * 2.54
trees$HT.M <- trees$HT / 3.2808
trees$PLOT.YEAR <- paste(trees$PLT_CN, trees$INVYR, sep=".")
trees <- trees[trees$STATUSCD == 1,]
trees.HARDWOOD <- trees[trees$SPGRPCD %in% hardwoods,]
trees.HARDWOOD <- merge(trees.HARDWOOD, S.table)
trees.HARDWOOD$LEAF.AREA.LN <- 0.2102 + (0.0586*trees.HARDWOOD$DIA.CM) + (4.0202*trees.HARDWOOD$Model.S)
trees.HARDWOOD$LEAF.AREA <- exp(trees.HARDWOOD$LEAF.AREA.LN)
SAPPS.PLOT.HARDWOOD.LEAFAREA <- aggregate(trees.HARDWOOD$LEAF.AREA, by=list(PLOT.YEAR=trees.HARDWOOD$PLOT.YEAR), FUN=sum)
colnames(SAPPS.PLOT.HARDWOOD.LEAFAREA) <- c("PLOT.YEAR","HARDWOOD.LEAFAREA")
trees.SOFTWOOD <- trees[trees$SPGRPCD %in% softwoods,]
trees.SOFTWOOD$LEAF.BIOMASS.KG <- 0.0621*(trees.SOFTWOOD$DIA.CM^1.505)
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
#but we want a crosswalk from the names used in LANDIS to FIA SPCD somehow
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
  count.inrange <- c()
  for (j in histogram.index){
    min <- mins[j]
    max <- maxs[j]
    subset <- columns.needed[columns.needed$PLOT.LAI >= min & columns.needed$PLOT.LAI < max,]
    final.column <- subset[names(subset)[names(subset) %in% SP.needed]]
    final.column.vec <- as.vector(final.column)
    mean.inrange[j] <- mean(final.column[,SP.needed])
    count.inrange[j] <- sum(final.column[,SP.needed])
  }
  # SAPPS.histogram[,SP.needed] <- mean.inrange
  SAPPS.histogram[,SP.needed] <- count.inrange
}

#draw histograms for each species
for (i in 1:length(spp_to_use)){
  SP.needed <- spp_to_use[i]
  SPCD.needed <- spcd_to_use[i]
  barplot(height = SAPPS.histogram[,SP.needed], names.arg = SAPPS.histogram$max, main = SP.needed, xlab = "LAI",ylab="Seedlings/plot")
}


#Fit Weibull curves to the LAI data
weibull_models <- SAPPS.PLOT.TOTAL.LEAFAREA %>%
  tidyr::pivot_longer(cols = all_of(spp_to_use),
                      names_to = "Species",
                      values_to = "Count") %>%
  group_by(Species) %>%
  filter(Count > 0) %>%
  mutate(PLOT.LAI = ifelse(PLOT.LAI < 0.01, 0.01, PLOT.LAI)) %>%
  group_by(Species) %>%
  summarise(weibull = list(fitdist(PLOT.LAI, "weibull")))


#plot the histogram and weibull fit for each species
pdf(file = "Weibull LAI distribution by species.pdf")
map2(.x = weibull_models$weibull, 
     .y = weibull_models$Species, 
     .f = ~ denscomp(.x, 
                     main = .y,
                     xlab = "LAI"))
dev.off()

weibull_params <- weibull_models %>%
  mutate(shape = map_dbl(.x = .$weibull, .f = ~ pluck(.x, "estimate", "shape")),
         scale = map_dbl(.x = .$weibull, .f = ~ pluck(.x, "estimate", "scale"))) %>%
  dplyr::select(-c(weibull))

write.csv(weibull_params, "weibull_params.csv")


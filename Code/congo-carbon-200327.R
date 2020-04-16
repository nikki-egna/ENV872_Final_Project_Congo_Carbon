####################################################
### CONGO CARBON CODE TO ESIMATE ACG FOR 30 
### PLOTS OVER THREE TIME PERIODS 2005, 09, 13 

## load packages & data
pacman::p_load(ggplot2, GGally, BIOMASS, tidyr, dplyr, data.table, glmm, glmmTMB, lme4, vegan, tidyverse, stringr)
options(digits = 3)

cdat <- read.csv("./data/Sangha.all.csv", header = T, stringsAsFactors = F)

### DATA CLEAN-UP 
####################################################

## remove unneccesary columns
cdat <- dplyr::select(cdat, -Tree.Notes, -Census.Notes, -Census.Notes.2)
cdat <- cdat[cdat$Species != "Liana",]

## check & change column classes
sapply(cdat, class)
cdat$Species <- as.character(cdat$Species)
cdat$D1 <- as.numeric(cdat$D1)

## check tag situation - no duplicated tags w/in a plot
length(unique(cdat$Tag.No)) 
 cdat$Tag.No3 <- ifelse(!is.na(cdat$Tag.No), cdat$Tag.No, cdat$Tag.No2)
  length(unique(cdat$Tag.No3)) 

cc <- cdat[order(cdat$Tag.No3),]
 cc <- select(cc, Plot, Tag.No, Tag.No2, Tag.No3)
  cc <- cc[duplicated(cc$Tag.No3),]

 cc1 <- cc %>% 
       group_by(Tag.No3, Plot) %>%
        mutate(dupe = n()>1)
 
 nplots <- 30

 ## change all diameters to cm
 cnums <- which(str_extract(colnames(cdat), "DBH")  == "DBH")
  cdat[,cnums] <- sapply(cdat[,cnums], as.numeric)
   cdat[,cnums] <- apply(cdat[,cnums], 2, function(x)x/10)
    cdat$D1 <- cdat$D1/10

### FIX TAXONOMY
####################################################
 
sppname <- strsplit(cdat$Species, " ")
cdat$Genus <- sapply(sppname, "[", 1)
cdat$Spp <- sapply(sppname, "[", 2)

Taxo <- correctTaxo(genus = cdat$Genus, species = cdat$Spp)
cdat$genusCorr <- Taxo$genusCorrected
cdat$sppCorr <- Taxo$speciesCorrected

APG <- getTaxonomy(cdat$genusCorr, findOrder = T)
cdat$familyAPG <- APG$family
cdat$orderAPG <- APG$order


### ADD WOOD DENSITY
####################################################

WdDen <- getWoodDensity(genus = cdat$genusCorr,
                        species = cdat$sppCorr, 
                        stand = cdat$Plot)

cdat$meanWD <- WdDen$meanWD
cdat$sdWD <- WdDen$sdWD


### CORRECT COLUMNS W/ NO 2013 MEASUREMENT 
####################################################

cdat$DBH4_13[cdat$DBH4_13 == 0] <- NA # inserting NA so there isn't a zero class
cdat$D1[cdat$D1 == 0] <- NA
# cdat$D1 <- with(cdat, ifelse(!is.na(D1), D1, DBH4_13))
# I think the above line was setting D1 as DBH4_13 when it shouldn't be

# problem: if D1 = NA when DBH13_04 has a number, could be a field error
# where D1 wasn't measured but should have been 
# to correct: multiply DBH13_04 by mean ratio between DBH13_04 & D1 for each plot

p.ratio <- as.vector(with(cdat[cdat$D1 > 10,], tapply(DBH4_13, list(Plot), 
                                                      function(x) mean(x/D1, na.rm = T))))

# no of D1's without measurements
cdat$F3.2 <- as.integer(cdat$F3.2)
num.bad <- length(cdat$F3.2[cdat$F3.2 == 4 & is.na(cdat$D1)])

find.d1 <- list()
find.d1 <- lapply(1:length(p.ratio), function(x)cdat$DBH4_13[cdat$Plot == x] * p.ratio[x])
find.d1 <- unlist(find.d1)
cdat$D1C <-with(cdat, ifelse(!is.na(D1), D1, find.d1))

# no of D1's without measurements after correction to D1C
num.bad <- length(cdat$F3.2[cdat$F3.2 == 4 & is.na(cdat$D1C)])


### CORRECT BAD MEASUREMENT METHODS IN 2005 & 09
####################################################

# in 2005 & 09 big trees were not measured using ladders above roots
# to correct, find the proportional difference between the good & bad
# measurements in 2013 and correct 2005 and 2009 DBH's with the proportion
# summarize frequency of trees by dbh categories in counts data.frame
# to determine how to group trees into classes to apply corrections 
# below by dbh class

round_any <- function(x, accuracy, f = floor){
  f(x/ accuracy) * accuracy
} 

cdat$cat09 <- round_any(cdat$DBH4_09, 10) #NA's come up for trees w/o measurement
 cdat$cat05 <- round_any(cdat$DBH4_05, 10) #should there be NAs in '05?
  counts <- with(cdat[cdat$DBH4_09 >= 10,], data.frame(table(cdat$cat09)))
   counts$cat <- seq(10, nrow(counts) * 10, by = 10)


## create data.frame of proportional changes (prop.change) b/n correct & incorrect 
## 2013 diameters & apply the results to 2005 & 2009 diameter measurements

prop.change <- cdat %>%
 group_by(cat09) %>%
  summarize(prop.delta = mean(na.omit(DBH4_13/D1C)), sd.delta = sd(na.omit(DBH4_13/D1C)))

 big.trees <- cdat[cdat$cat09  >= 140,]
  mean.max <- mean(cdat$DBH4_13[cdat$cat09>= 140 & !is.na(cdat$cat09)]/
                   cdat$D1corr[cdat$cat09>= 140 & !is.na(cdat$cat09)], na.rm = T)

   sd.max <- sd(cdat$DBH4_13[cdat$cat09>= 140 & !is.na(cdat$cat09)]/
                cdat$D1corr[cdat$cat09>= 140 & !is.na(cdat$cat09)], na.rm = T)

 prop.change <- data.frame(prop.change)
  prop.change <- prop.change[prop.change$cat09 < 140,]
   prop.change[nrow(prop.change),] <- c(140, c(mean.max, sd.max))
    prop.change$cat <- prop.change$cat09 + 10
     prop.change <- prop.change[1:(nrow(prop.change)-1), ]

## multiply the prop.change by the data to get new, corrected dbh's
## categorizing everything >= 150 as just 150
cdat$cat09.1 <- round_any(cdat$DBH4_09, 10)
 cdat$cat09.1[cdat$cat09.1 > 140] <- 140
  cdat$cat05.1 <- round_any(cdat$DBH4_05, 10)
   cdat$cat05.1[cdat$cat05.1 > 140] <- 140

n09 <- c(); n05 <- c()
for(i in 1:nrow(cdat)){

  n09[i] <- ifelse(!is.na(cdat$DBH4_09[i] & cdat$DBH4_09[i]>0), 
                   cdat$DBH4_09[i] * prop.change$prop.delta[prop.change$cat == cdat$cat09.1[i]], 
                   NA)
  
  n05[i] <- ifelse(!is.na(cdat$DBH4_05[i] & cdat$DBH4_05[i]>0), 
                   cdat$DBH4_05[i] * prop.change$prop.delta[prop.change$cat == cdat$cat05.1[i]], 
                   NA)
}

cdat$newD09 <- n09
cdat$newD05 <- n05


### CORRECT DBH BASED ON SLOPE FROM 2013 MEASUREMENT 
##################################################

# no need for this change anymore but don't want to change all below
cdat2 <- cdat     
t05 <- 2005
t09 <- 2009
t13 <- 2013

# replace 0s w/ NAs
cdat2$DBH4_13[cdat2$DBH4_13 == 0] <- NA
cdat2$DBH4_09[cdat2$DBH4_09 == 0] <- NA
cdat2$DBH4_05[cdat2$DBH4_05 == 0] <- NA

# calculate slopes betweeen wrongly measured D's (2005 to 2009 & 2009 to 2013)
slope.0509 <- (cdat2$DBH4_09 - cdat2$DBH4_05)/(t09 - t05) 
slope.0913 <- (cdat2$D1C - cdat2$DBH4_09)/(t13 - t09) 

# use slope to backcalculate corrected D for 2009
# y - bx = a, written as (D1 - slope.0913*(t13-t09))
# taking correct measurement & backtracking w/ slope of incorrect measurements
cdat2$Dnew.09 <- cdat2$DBH4_13 - slope.0913*(t13-t09) 
cdat2$D09corr <- with(cdat2, ifelse(!is.na(Dnew.09), Dnew.09, newD09)) # 3 negative #'s

# use slope to backcalculate corrected D for 2005
cdat2$Dnew.05 <- cdat2$D09corr - slope.0509*(t09-t05)
cdat2$D05corr <- with(cdat2, ifelse(!is.na(Dnew.05), Dnew.05, newD05)) # 1 negative #

### GRAPHICALLY CHECK DBH DIFFERENCES B/N YEARS
##################################################

# graph is to check above work to make sure there aren't wonky differences
# between years, like shrinking trees and plots
# how does DBH change by plot?

check <- cdat2[,c('Plot', 'DBH4_13', 'D09corr', 'D05corr', 'Tag.No3')]
 check$change0509 <- check$D09corr - check$D05corr
  check$change0913 <- check$DBH4_13 - check$D09corr
   check$change0513 <- check$DBH4_13 - check$D05corr

check.plot <- check %>%
  group_by(Plot) %>%
   summarize(mean.change0509 = mean(na.omit(change0509)), mean.change0913 = mean(na.omit(change0913)), mean.change0513 = mean(na.omit(change0513)))

par(mfrow = c(1,2))
plot(check.plot$Plot, check.plot$mean.change0509, xlab = "Plot", 
     ylab = "Change in DBH by Plot '05-'09", col = check.plot$Plot)
 abline(h = 0, col = "blue")
plot(check.plot$Plot, check.plot$mean.change0913, xlab = "Plot", 
     ylab = "Change in DBH by Plot '09-'13", col = check.plot$Plot)
 abline(h = 0, col = "blue")

negative.plots <- cdat[cdat$Plot == c(5,14),]
negative.plots <- merge(negative.plots, check, by.x = "Tag.No3", by.y = "Tag.No3")


### USE HD MODELS TO PREDICT TREE HEIGHTS
#######################################################

# load height data for 2013 & average across field measurements
ht.1 <- read.csv("./data/congo-all-hts-200324.csv", header = T)
 ht.1 <- dplyr::select(ht.1, -Date, -PlotName, -Category, -S.Plct, -Method, -Name.of.Observer, -Notes)

 ht.1 <-  ht.1 %>% 
   mutate_at(vars(Code, Number, Tag), ~as.integer(as.character(.))) %>%
    mutate_at(vars(H1, H2, H3), ~as.numeric(as.character(.)))
 
  ht.1$Hmean <- with(ht.1, rowMeans(data.frame(cbind(H1, H2, H3)), na.rm = TRUE))


# add tree height measurements to data.frame with tree diameters
# height & DBH tags don't line up perfectly so gain a few trees
ht.1$PlTag <- paste(ht.1$Code, ht.1$Tag, sep=".")
cdat2$PlTag <- paste(cdat2$Plot, cdat2$Tag.No3, sep=".")

ht.2<- ht.1%>%
  left_join(cdat2, by = "PlTag")
ht.2 <- ht.2[!is.na(ht.2$Hmean),]

cdat2 <- merge(cdat2, ht.1, by.x = "PlTag", by.y  = "PlTag", all.x = T) 

# compare 3 ht models and pick the best for each plot

H_mod <- with(ht.2, 
               modelHD(D = DBH4_13, H = Hmean, method = NULL, plot = Code, 
                  useWeight = F, drawGraph = F))

H_mod <- H_mod[names(H_mod) %in% "NA" == FALSE]  

lnames <- names(H_mod)
 rse <- do.call("rbind", lapply(H_mod, "[[", "RSE"))
  rse.min <- apply(rse, 1, FUN=min)
   rse.min <- data.frame(plt = as.numeric(names(rse.min)), rse.min)
 
 pick_fun <- function(pfdat, pfrse){
   pfrse <- pfrse[1]
   d <- data.frame(pfdat[[1]])
    pick <- d[which(d$RSE == pfrse),]
   pick
 }
 
 mod_pick <- lapply(1:nplots, function(i)pick_fun(pfdat = H_mod[names(H_mod) == lnames[i]], 
                                             pfrse = rse.min$rse.min[rse.min$plt == lnames[i]]))
 mod_pick <- do.call("rbind", mod_pick)
  mod_pick$plt <- names(H_mod)

 # use best ht model by plot to get heights for all trees
 get_ht <- function(pdat, hmod, d_name){
   hmod <- hmod$method
    ppdat <- pdat[!is.na(pdat$Hmean), c("Hmean", d_name, "Plot")]
     names(ppdat) <- c("hmean", "diam", "plot")
      ppdat <- ppdat[!is.na(ppdat$diam),]
       h <- with(ppdat, modelHD(D = diam, H = hmean, method = hmod, useWeight = F))
      prht <- retrieveH(D = pdat[,d_name], model = h)$H
    prht
 }


# get heights for 2013, 2009, and 2005 based on 2013 model
prHt05 <- lapply(1:nplots, function(i)get_ht(pdat = cdat2[cdat2$Plot == i,], 
                                             hmod = mod_pick[mod_pick$plt == i,], 
                                             d_name = "D05corr"))
cdat2$H05 <- unlist(prHt05)

prHt09 <- lapply(1:nplots, function(i)get_ht(pdat = cdat2[cdat2$Plot == i,], 
                                             hmod = mod_pick[mod_pick$plt == i,], 
                                             d_name = "D09corr"))
cdat2$H09 <- unlist(prHt09)

prHt13 <- lapply(1:nplots, function(i)get_ht(pdat = cdat2[cdat2$Plot == i,], 
                                             hmod = mod_pick[mod_pick$plt == i,], 
                                             d_name = "DBH4_13"))
cdat2$H13 <- unlist(prHt13)

AGB_plot_ht <- cdat2 %>%
  group_by(Plot) %>%
   summarize(H05 = mean(na.omit(H05)), H09 = mean(na.omit(H09)), H13 = mean(na.omit(H13)))
   
AGB_plot_wd <- cdat2 %>%
  group_by(Plot) %>%
   summarize(W05 = mean(na.omit(meanWD)), W09 = mean(na.omit(meanWD)), W13 = mean(na.omit(meanWD)))

AGB_plot_d <- cdat2 %>%
  group_by(Plot) %>%
  summarize(D05 = mean(na.omit(D05corr)), D09 = mean(na.omit(D09corr)), D13 = mean(na.omit(DBH4_13)))

print(AGB_plot_ht, n = 30)
 colMeans(AGB_plot_ht)
  colMeans(AGB_plot_wd)
 colMeans(AGB_plot_d)


### ESTIMATE ABOVEGROUND BIOMASS ###
#######################################################

# tree hts are short, shrink over time and result in very low AGB
cdat2$AGB05.Mg <- computeAGB(D = cdat2$D05corr, WD = cdat2$meanWD, H = cdat2$H05)
cdat2$AGB09.Mg <- computeAGB(D = cdat2$D09corr, WD = cdat2$meanWD, H = cdat2$H09)
cdat2$AGB13.Mg <- computeAGB(D = cdat2$DBH4_13, WD = cdat2$meanWD, H = cdat2$H13)


# use coords to get Chave's E & estimate AGB
coords <- read.csv("./data/plotsHFI.csv", header = T, numerals = c("no.loss"))
 coords <- coords[,c("plot", "lat", "lon")]
  coords$freq <- with(cdat2, tapply(Plot, list(Plot), function(x)length(x)))
   format(coords, digits=10)

 df <- coords %>% 
   uncount(freq)
    format(df, digits=10)
   coord <- cbind(long = df$lon, lat = df$lat)
    

cdat2$AGB05.MgE <- computeAGB(D = cdat2$D05corr, WD = cdat2$meanWD, coord = coord)
cdat2$AGB09.MgE <- computeAGB(D = cdat2$D09corr, WD = cdat2$meanWD, coord = coord)
cdat2$AGB13.MgE <- computeAGB(D = cdat2$DBH4_13, WD = cdat2$meanWD, coord = coord)

write.csv(cdat2, "sangha-plots-agb-200415.csv")

# get MEAN biomass by plot

AGB_plot_mn <- cdat2 %>%
  group_by(Plot) %>%
   summarize(mean_AGB05 = mean(na.omit(AGB05.Mg)), mean_AGB09 = mean(na.omit(AGB09.Mg)), mean_AGB13 = mean(na.omit(AGB13.Mg)))

# get TOTAL biomass by plot

AGB_plot_sum <- cdat2 %>%
  group_by(Plot) %>%
   summarize(sum_AGB05 = sum(AGB05.Mg, na.rm = TRUE), sum_AGB09 = sum(AGB09.Mg, na.rm = TRUE), sum_AGB13 = sum(AGB13.Mg, na.rm = TRUE))

colMeans(AGB_plot_sum)

# get TOTAL biomass by plot using Chave's E value

AGB_plot_sumE <- cdat2 %>%
  group_by(Plot) %>%
  summarize(sum_AGB05 = sum(AGB05.MgE, na.rm = TRUE), 
             sum_AGB09 = sum(AGB09.MgE, na.rm = TRUE), 
              sum_AGB13 = sum(AGB13.MgE, na.rm = TRUE))

colMeans(AGB_plot_sum)

# get TOTAL changes by plot using Chave's E value
AGB_plot_sumE$change0509 <- with(AGB_plot_sumE, sum_AGB09 - sum_AGB05)
AGB_plot_sumE$change0913 <- with(AGB_plot_sumE, sum_AGB13 - sum_AGB09)
AGB_plot_sumE$change0513 <- with(AGB_plot_sumE, sum_AGB13 - sum_AGB05)

write.csv(AGB_plot_sumE, "plot-agb-summary-200415.csv")

AGB_change <- AGB_plot_sumE %>%
   summarize(mean(change0509), mean(change0913))

# plot change by site for 2005--> 2009 --> 2013

#######################################################

### DIVERSITY ###

require(reshape2)
cast <- cdat2
cast <- acast(cast, cast$Species ~ cast$Plot, drop = FALSE)
cast <- as.data.frame(cast)
cast <- t(cast)

plot.div <- as.data.frame(diversity(cast))
plot.div$Plot <- row.names(plot.div)
#need to put this in covariates data.frame


### FOREST TYPE ###

AGB_plot_sum$ftype <- 'placeholder'
for (i in 1:nrow(AGB_plot_sum)) {
  row <- AGB_plot_sum[i,]
  plot <- row$Plot
  if (plot <= 10) { 
    row$ftype <- "HL"
  } else if (plot <= 20) { 
    row$ftype <- "L"
  } else if ( plot > 20) { 
    row$ftype <- "P" 
  }
  AGB_plot_sum[i,] <- row
}

AGB_plot_sum$ftype <- as.factor(AGB_plot_sum$ftype)

#######################################################


### PLOTS TO SEE BIOMASS CHANGE ###

par(mfrow = c(1,2))
plot(AGB_plot_sum$Plot, AGB_plot_sum$change0509, xlab = "Plot", 
     ylab = "Change in AGB '05-'09", col = AGB_plot_sum$ftype)
abline(h = 0, col = "blue")
plot(AGB_plot_sum$Plot, AGB_plot_sum$change0913, xlab = "Plot", 
     ylab = "Change in AGB '09-'13", col = AGB_plot_sum$ftype)
abline(h = 0, col = "blue")

# total biomass by plot type
AGB_ftype_sum <- AGB_plot_sum %>%
  group_by(ftype) %>%
  summarize(sum_AGB05 = sum(sum_AGB05, na.rm = TRUE), sum_AGB09 = sum(sum_AGB09, na.rm = TRUE), sum_AGB13 = sum(sum_AGB13, na.rm = TRUE))

AGB_ftype_sum$change0509 <- AGB_ftype_sum$sum_AGB09 - AGB_ftype_sum$sum_AGB05
AGB_ftype_sum$change0913 <- AGB_ftype_sum$sum_AGB13 - AGB_ftype_sum$sum_AGB09

# try melting
dfmelt <- melt(AGB_ftype_sum)
dfmelt<- dfmelt[dfmelt$variable != "change0509" & dfmelt$variable != "change0913",]
ggplot()+geom_point(aes(x=dfmelt$variable, y=dfmelt$value, color=dfmelt$ftype)) + theme(legend.position = "right")


### PLOT DATA ###

#load in plot environmental variables 
plot.ev <- read.csv("./data/Plot_table.csv", header = T)

plot.ev <- merge(plot.ev, plot.div, by.x = "Plot", by.y = "Plot")

# merge into plot-level data frame
plot.ev <- merge(plot.ev, AGB_plot_sum, by.x = "Plot", by.y  = "Plot")
plot.ev <- merge(plot.ev, plotcoo, by.x = "Plot", by.y  = "plot")
# looking good

# making a new data frame w/ only 2013 DBHs, AGB, and plot variables
cdat.simp <- merge(cdat2, plot.ev, by.x = "Plot", by.y = "Plot")
cdat.simp <- cdat.simp[,c(1, 2, 8, 9, 52, 54, 82:84, 87:100)]
#cdat <- merge(cdat, plot.div, by.x = "Plot", by.y  = "Plot") 

write.csv(cdat.simp, "cdat_pca.csv")


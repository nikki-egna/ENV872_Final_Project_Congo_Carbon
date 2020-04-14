####################################################
### summary: the code corrects the dbh measurements 
### because of different measurement techniques, 
### estimates tree heights and plot-level AGC
####################################################


### load packages
pacman::p_load(ggplot2, GGally, BIOMASS, tidyr, dplyr, data.table, glmm, glmmTMB, 
               lme4, ggsci, gridExtra)
options(digits = 3)

mean_rm <- function(x)mean(x, na.rm = T)
sd_rm <- function(x)sd(x, na.rm = T)

## load data
cdat <- read.csv("./data/Sangha.all.csv", header = T, stringsAsFactors = F)

####################################################
### prepare data for analyses
####################################################

## remove unneccesary columns
cdat <- dplyr::select(cdat, -Tree.Notes, -Census.Notes, -Census.Notes.2)
cdat <- cdat[cdat$Species != "Liana",]

## check & change column classes
sapply(cdat, class)
cdat$Species <- as.character(cdat$Species)
cdat$D1 <- as.numeric(cdat$D1)

## check tags -  no duplicated tags w/in a plot
length(unique(cdat$Tag.No)) 
 cdat$Tag.No3 <- ifelse(!is.na(cdat$Tag.No), cdat$Tag.No, cdat$Tag.No2)
  length(cdat$Tag.No3)
   length(unique(cdat$Tag.No3)) 

cc <- cdat[order(cdat$Tag.No3),]
 cc <- select(cc, Plot, Tag.No, Tag.No2, Tag.No3)
  cc <- cc[duplicated(cc$Tag.No3),]

cc1 <- cc %>% 
  group_by(Tag.No3, Plot) %>%
  mutate(dupe = n()>1)

## tree sizes
cdat$DBH4_13[cdat$DBH4_13 == 0] <- NA
cdat$DBH4_09[cdat$DBH4_09 == 0] <- NA
cdat$DBH4_05[cdat$DBH4_05 == 0] <- NA

 sm.trees <- subset(cdat, subset=(DBH4_13 < 100|DBH4_09 < 100|DBH4_05 < 100))
  sm.trees <- subset(sm.trees, subset=(DBH4_13 > 0|DBH4_09 > 0|DBH4_05 > 0))

 nums <- cdat$DBH4_13[cdat$DBH4_13 < 100 & cdat$DBH4_13 != 0 & !is.na(cdat$DBH4_13)] 

 cdat$DBH4_13[cdat$DBH4_13 %in% nums] <- nums * 10 # are we sure about these?


####################################################
### get latest taxonomies
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


####################################################
### get wood densities
####################################################

WdDen <- getWoodDensity(genus = cdat$genusCorr,
                        species = cdat$sppCorr, 
                        stand = cdat$Plot)

cdat$meanWD <- WdDen$meanWD
cdat$sdWD <- WdDen$sdWD


##################################################
### correcting dbh's based on 'good' measurements in 2013
##################################################

# D1 is the DBH from the incorrect method of measuring in 2013
cdat$D1[cdat$D1 == 0] <- NA
cdat$D1c <- with(cdat, ifelse(!is.na(D1), D1, DBH4_13))

# dbh difference histogram function
dbh_plot <- function(d1, d2){
  ddiff <- na.omit(d1 - d2)
  mean_diff <- signif(mean(d1-d2, na.rm = T), 2) 
  quant <- quantile(na.omit(d1 - d2))[4]
  mind <- min(na.omit(d1 - d2))
  ggplot(data = data.frame(ddiff), aes(x = ddiff)) + 
    geom_histogram(color = "white", fill = 2) + theme_bw() + xlab("Difference, mm") + ylab("Count") + 
    annotate("text", x = -quant, y = 3000, label = paste('mean = ', mean_diff,sep=" ")) + 
    scale_fill_jco()
}


with(cdat, mean(DBH4_13/D1, na.rm = T))
with(cdat, mean(DBH4_13/D1c, na.rm = T))

cdat$D9corr[2953] <- 134 
cdat$D9corr[6374] <- 122
cdat$D9corr[7408] <- 112

# subtract change from t3 to get new dbh of t2, etc.
cdat$D13corr <- cdat$DBH4_13
cdat$D9corr <- cdat$DBH4_13 - (cdat$D1c - cdat$DBH4_09)
cdat$D5corr <- cdat$D9corr - (cdat$DBH4_09 - cdat$DBH4_05)
#cdat$D5corr <- cdat$DBH4_05 - (cdat$DBH4_09 - cdat$DBH4_05)


####START HERE!!!!####
missing_d <- function(D, delta){
ddelta <- data.frame(cbind(D = D, delta = delta, 
                           grp = 30))
 ddelta$grp[ddelta$D > 300 & ddelta$D <= 500] <- 50
  ddelta$grp[ddelta$D > 500 & ddelta$D <= 700] <- 70
   ddelta$grp[ddelta$D > 700 & ddelta$D <= 1500] <- 150
    ddelta$grp[ddelta$D > 1500] <- "150+"
    
mn <- with(ddelta, tapply(delta/10, list(factor(grp)), function(x)(mean(x, na.rm = T))))
std <- with(ddelta, tapply(delta/10, list(factor(grp)), function(x)(sd(x, na.rm = T))))                                                        
 res <- data.frame(rbind(mn, std))
  colnames(res) = c("150", "150+", "30", "50", "70")
   res <- select(res, "30", "50", "70", "150", "150+")
    return(res)
}


dn9 <- missing_d(D = cdat$D9corr, delta = cdat$D1c - cdat$DBH4_09)
dn5 <- missing_d(D = cdat$D5corr, delta = cdat$DBH4_09 - cdat$DBH4_05)

breaks <- c(10,30,50,70,150,500)
tags <- c("30", "50", "70", "150", "150+")

cdat$D9gps <- cut(cdat$DBH4_09/10, breaks=breaks, include.lowest=TRUE, 
                    right=FALSE, labels=tags)
cdat$D5gps <- cut(cdat$DBH4_05/10, breaks=breaks, include.lowest=TRUE, 
                  right=FALSE, labels=tags)


newd <- function(dmt, dbh4, gp, dn){
         mgp <- dn[1, as.character(gp)] 
         sgp <- dn[2, as.character(gp)] 
         dmt_new = ifelse(!is.na(dmt), dmt, 
                    ifelse(is.na(dbh4), NA, 
                          dbh4 - rnorm(1, mean = mgp, sd = sgp)))
}

res9 <- list()
res9 <- lapply(1:nrow(cdat), function(x)newd(dmt = cdat$D9corr[x], dbh4 = cdat$DBH4_09[x], 
                                     gp = cdat$D9gps[x], dn = dn9))
cdat$D9corr1 <- unlist(res9)

res5 <- list()
res5 <- lapply(1:nrow(cdat), function(x)newd(dmt = cdat$D5corr[x], dbh4 = cdat$DBH4_05[x], 
                                             gp = cdat$D5gps[x], dn = dn5))
cdat$D5corr1 <- unlist(res5)

mymns0 <- with(cdat, c(m13 = mean_rm(DBH4_13), m9 = mean_rm(DBH4_09), m5 = mean_rm(DBH4_05)))
mymns1 <- with(cdat, c(m13 = mean_rm(D13corr), m9 = mean_rm(D9corr), m5 = mean_rm(D5corr)))
mymns2 <- with(cdat, c(m13 = mean_rm(D13corr), m9 = mean_rm(D9corr1), m5 = mean_rm(D5corr1)))

# plot level estimates of dbh for 'correct' measurement method
t05<- with(cdat, tapply(D5corr1, list(Plot), function(x)mean(x, na.rm = T)))/10
t09 <- with(cdat, tapply(D9corr1, list(Plot), function(x)mean(x, na.rm = T)))/10
t13<- with(cdat, tapply(D13corr, list(Plot), function(x)mean(x, na.rm = T)))/10
 mean(t13-t09, na.rm = T)
 mean(t09-t05, na.rm = T)
 mean(t13-t05, na.rm = T)

# plot level estimates of dbh for 'bad' measurement method
t05u <- with(cdat, tapply(DBH4_05, list(Plot), function(x)mean(x, na.rm = T)))/10
t09u <- with(cdat, tapply(DBH4_09, list(Plot), function(x)mean(x, na.rm = T)))/10
t13u <- with(cdat, tapply(D1, list(Plot), function(x)mean(x, na.rm = T)))/10
 mean(t13u - t09u, na.rm = T)
 mean(t09u - t05u, na.rm = T)

tdf <- data.frame(cbind(signif(c(t05, t09, t13), 4), c(rep(c("05", "09", "13"), each = 30))))
 names(tdf) <- c("dbh", "year")
  tdf$plot <- rep(1:30, 3)
   tdf$dbh <- as.numeric(as.character(tdf$dbh))
    tdf$site <- ifelse(tdf$plot > 20, 3, 
                 ifelse(tdf$plot < 21 & tdf$plot > 10, 2, 1))

tdf2 <- data.frame(cbind(signif(c(t05u, t09u, t13u), 4), c(rep(c("05", "09", "13"), each = 30))))
 names(tdf2) <- c("dbh", "year")
  tdf2$plot <- rep(1:30, 3)
   tdf2$dbh <- as.numeric(as.character(tdf2$dbh))

bar_plot <- function(dat){
  ggplot(dat, aes(x = as.factor(plot), y = dbh, fill = year)) + 
    geom_bar(stat="identity", position=position_dodge()) + 
    coord_cartesian(ylim = c(0, 40)) +  
    xlab("Plots") + ylab("DBH, cm") + 
    theme_bw()}

#bar_plot(dat = tdf)
#bar_plot(dat = tdf2)

pdf("dbh_plots.pdf")
gg1 <- ggplot(tdf, aes(y = dbh, x = year, fill = factor(site))) + 
       geom_boxplot() +   
        ylab("DBH, cm") + xlab("Years") + 
       scale_fill_discrete(name = "Site", labels = c("L/H", "L", "U")) + 
      theme_bw() + theme(legend.position = "top", legend.box = "horizontal")

gg2 <- ggplot(tdf, aes(y = dbh, x = year, fill = factor(year))) + 
       geom_boxplot() + 
        ylab("DBH, cm") + xlab("Years") + 
       #scale_fill_discrete(name = "Site", labels = c("L/H", "L", "U")) + 
      theme_bw() + theme(legend.position = "none")

grid.arrange(gg1, gg2, nrow = 1)

dev.off()

## numbers of trees per plot
nt05 <- with(cdat, tapply(DBH4_05, list(Plot), function(x)length(x[!is.na(x)])))
nt09 <- with(cdat, tapply(DBH4_09, list(Plot), function(x)length(x[!is.na(x)])))
nt13 <- with(cdat, tapply(DBH4_13, list(Plot), function(x)length(x[!is.na(x)])))
               
comp.df <- data.frame(cbind(t05, t09, t13, nt05, nt09, nt13))

dbh_group_fun <- function(dvec){
                  breaks <- c(10,30,50,70,150,500)
                   tags <- c("[10-30]", "[30-50]", "[50-70]", "[70-150]", "[150+]")

                    groups <- cut(dvec/10, breaks=breaks, 
                                  include.lowest=TRUE, right=FALSE, labels=tags)
                  summary(groups)
}
  
  l13 <- list()
  l13 <- lapply(1:30, function(x){dbh_group_fun(dvec = cdat$DBH4_13[cdat$Plot == x])})
   l13 <- data.frame(do.call("rbind", l13))
  
  l9 <- list()
  l9 <- lapply(1:30, function(x){dbh_group_fun(dvec = cdat$DBH4_09[cdat$Plot == x])})
   l9 <- data.frame(do.call("rbind", l9))
   
  l5 <- list()
  l5 <- lapply(1:30, function(x){dbh_group_fun(dvec = cdat$DBH4_05[cdat$Plot == x])})
   l5 <- data.frame(do.call("rbind", l5)) 
   
  dall <- data.frame(rbind(l13, l9, l5))
   dall$Plot <- rep(1:30, 3)
    colnames(dall) <- c("30", "50", "70", "150", "150+", "NA", "plot")
     dall <- select(dall, -"NA")
    
  dall <- melt(dall, id = c("plot"))
   dall$time <- rep(c(rep("13", 30), rep("09", 30), rep("05", 30)), 5)
    dall$time <- as.factor(dall$time)
    dall1 <- dall[dall$plot < 11,]
    
  
  gg10 <- ggplot(data = dall[dall$plot < 11,], aes(x=variable, y=value, fill = factor(time))) + 
          geom_bar(position = "dodge", stat = "identity") + 
           facet_wrap( ~ plot) + xlab("Number of trees") + 
            ylab("DBH class, cm") + 
             theme_bw() + 
            theme(legend.position = c(0.7, 0.15)) + 
          scale_fill_discrete(name = "Years", labels = c("2005", "2009", "2013"))

  gg20 <- ggplot(data = dall[dall$plot > 10 & dall$plot < 21,], 
                 aes(x=variable, y=value, fill = factor(time))) + 
          geom_bar(position = "dodge", stat = "identity") + 
           facet_wrap( ~ plot) + xlab("Number of trees") + 
            ylab("DBH class, cm") + 
             theme_bw() + 
            theme(legend.position = c(0.7, 0.15)) + 
           scale_fill_discrete(name = "Years", labels = c("2005", "2009", "2013"))
  

  gg30 <- ggplot(data = dall[dall$plot > 20,], aes(x=variable, y=value, fill = factor(time))) + 
          geom_bar(position = "dodge", stat = "identity") + 
           facet_wrap( ~ plot) + xlab("Number of trees") + 
            ylab("DBH class, cm") + 
             theme_bw() + 
            theme(legend.position = c(0.7, 0.15)) + 
           scale_fill_discrete(name = "Years", labels = c("2005", "2009", "2013"))
  
  pdf("tree_num_plots.pdf")
  gg10; gg20; gg30
  dev.off()
 

####################################################
### height:diameter models
####################################################

cdat2 <- cdat
 
# Run HDmodel for 2013 to get model and coefficients

ht.1 <- read.csv("./data/Congo_All_Heights_Tag_Updates_AEN.csv", header = T)
ht.1$Hmean <- with(ht.1, rowMeans(data.frame(cbind(H1, H2, H3)), na.rm = TRUE))
# remove unnecessary columns
ht.1 <- ht.1[,c(2, 6, 7, 15)]

ht.1$Tag3 <- paste(ht.1$Tag, ht.1$Tag2)
cdat2$Tag3 <- paste(cdat2$Tag.No, cdat2$Tag.No2)

cdat2 <- merge(cdat2, ht.1, by.x = "Tag3", by.y  = "Tag3", all.x = T) 
nrow(cdat2)
# gain 54 trees here because height and DBH tag numbers don't all line up

Hlog1 <- with(cdat2[!is.na(cdat2$Hmean) & cdat2$D13corr != 0,], modelHD(D = D13corr, H = Hmean, 
                                                         method = "log1", useWeight = F, drawGraph = F))

Hmich <- with(cdat2[!is.na(cdat2$Hmean) & cdat2$D13corr != 0,], modelHD(D = D13corr, H = Hmean, 
                                                         method = "michaelis", useWeight = F, drawGraph = F))

Hweib <- with(cdat2[!is.na(cdat2$Hmean) & cdat2$D13corr != 0,], modelHD(D = D13corr, H = Hmean, 
                                                         method = "weibull", useWeight = F, drawGraph = F))

RSE <- c(Hlog1$RSE, Hmich$RSE, Hweib$RSE)
names(RSE) <- c("log1", "michaelis", "weibull")
RSE
# weibull is best

# extract heights for 2013, 2009, and 2005
predHts05 <- retrieveH(D = cdat2$D5corr1, model = Hweib)
predHts09 <- retrieveH(D = cdat2$D9corr1, model = Hweib)
predHts13 <- retrieveH(D = cdat2$D13corr, model = Hweib)

cdat2$H05 <- predHts05$H
cdat2$H09 <- predHts09$H
cdat2$H13 <- predHts13$H   
  

####################################################
### above ground biomass calculations
####################################################

##cdat$D13corr, cdat$D9corr1, and cdat$D5corr1
  
cdat2$AGB05.Mg <- computeAGB(D = cdat2$D5corr1, WD = cdat2$meanWD, H = cdat2$H05)
cdat2$AGB09.Mg <- computeAGB(D = cdat2$D9corr1, WD = cdat2$meanWD, H = cdat2$H09)
cdat2$AGB13.Mg <- computeAGB(D = cdat2$D13corr, WD = cdat2$meanWD, H = cdat2$H13)

  
 # get MEAN biomass by plot
  
AGB_plot_mn <- cdat2 %>%
    group_by(Plot) %>%
    summarize(mean_AGB05 = mean(na.omit(AGB05.Mg)), mean_AGB09 = mean(na.omit(AGB09.Mg)), mean_AGB13 = mean(na.omit(AGB13.Mg)))
 
 # get TOTAL biomass by plot
 
AGB_plot_sum <- cdat2 %>%
   group_by(Plot) %>%
   summarize(sum_AGB05 = sum(AGB05.Mg, na.rm = TRUE), sum_AGB09 = sum(AGB09.Mg, na.rm = TRUE), sum_AGB13 = sum(AGB13.Mg, na.rm = TRUE))
 
AGB_plot_sum$change0509 <- AGB_plot_sum$sum_AGB09 - AGB_plot_sum$sum_AGB05
AGB_plot_sum$change0913 <- AGB_plot_sum$sum_AGB13 - AGB_plot_sum$sum_AGB09
AGB_plot_sum$change0513 <- AGB_plot_sum$sum_AGB13 - AGB_plot_sum$sum_AGB05

write.csv(AGB_plot_sum, 'AGB_plot_sum.csv')

#######################################################
### plots of AGB change
#######################################################

par(mfrow = c(1,3))
plot(AGB_plot_sum$Plot, AGB_plot_sum$change0509, xlab = "Plot", 
     ylab = "Change in AGB '05-'09", col = AGB_plot_sum$ftype)
abline(h = 0, col = "blue")
plot(AGB_plot_sum$Plot, AGB_plot_sum$change0913, xlab = "Plot", 
     ylab = "Change in AGB '09-'13", col = AGB_plot_sum$ftype)
abline(h = 0, col = "blue")
plot(AGB_plot_sum$Plot, AGB_plot_sum$change0513, xlab = "Plot", 
     ylab = "Change in AGB '05-'13", col = AGB_plot_sum$ftype)
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

#######################################################
### species diversity
#######################################################

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
 
 
 
 
# writing .csv for AGB by individual
write.csv(cdat2, "AGBind.csv")
 
 
 
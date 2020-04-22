#Set working directory
setwd("~/Desktop/Classes/Spring 2020/Environmental Data Analytics/ENV872_Final_Project_Congo_Carbon")

#Load packages
pacman::p_load(tidyr, dplyr)

#Read in raw data file
Raw_CongoCarbon_Data<-read.csv("./Data/Raw/CongoCarbon_Raw_Data.csv")

#Create columns with the sum AGB by plot for each year '05, '09, '13
AGB_by_Plot <- Raw_CongoCarbon_Data %>%
  group_by(Plot) %>%
  summarize(sum_AGB05 = sum(AGB05.MgE, na.rm = TRUE), 
            sum_AGB09 = sum(AGB09.MgE, na.rm = TRUE), 
            sum_AGB13 = sum(AGB13.MgE, na.rm = TRUE))

#Create columns for change in AGB from '05 to '09, from '09 to '13, and from '05 to '13
AGB_by_Plot$change0509 <- with(AGB_by_Plot, sum_AGB09 - sum_AGB05)
AGB_by_Plot$change0913 <- with(AGB_by_Plot, sum_AGB13 - sum_AGB09)
AGB_by_Plot$change0513 <- with(AGB_by_Plot, sum_AGB13 - sum_AGB05)

#Load the covariates dataframe
CongoCarbon_Covariates <- read.csv("./Data/Raw/CongoCarbon_Plot_Covariates.csv")

#Combine AGB_by_Plot and CongoCarbon_Covariates by plot so it is one dataframe
AGB_and_Covariates_by_Plot <- merge(CongoCarbon_Covariates, AGB_by_Plot, by="Plot")

write.csv(AGB_and_Covariates_by_Plot, "./Data/Processed/CongoCarbon_AGB_and_Covariates_by_Plot.csv", row.names = F)

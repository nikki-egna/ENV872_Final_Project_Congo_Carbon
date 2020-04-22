################## Data exploration of datasets within the Congo Carbon repository #####################

#Load necessary packges
library(dplyr)
library(data.table)
library(reshape2)

#Set working directory
setwd("~/Desktop/Classes/Spring 2020/Environmental Data Analytics/ENV872_Final_Project_Congo_Carbon")

#Explore raw data
Raw_CongoCarbon_Data<-read.csv("./Data/Raw/CongoCarbon_Raw_Data.csv")

head(Raw_CongoCarbon_Data)
summary(Raw_CongoCarbon_Data)

##Check class of necessary columns
class(Raw_CongoCarbon_Data$Plot)
class(Raw_CongoCarbon_Data$AGB05.MgE)
class(Raw_CongoCarbon_Data$AGB09.MgE)
class(Raw_CongoCarbon_Data$AGB13.MgE)

#Check for NAs
anyNA(Raw_CongoCarbon_Data$AGB05.MgE)
anyNA(Raw_CongoCarbon_Data$AGB09.MgE)
anyNA(Raw_CongoCarbon_Data$AGB13.MgE)

#Look at NA values
nrow(subset(Raw_CongoCarbon_Data, is.na(AGB05.MgE)))
nrow(subset(Raw_CongoCarbon_Data, is.na(AGB09.MgE)))
nrow(subset(Raw_CongoCarbon_Data, is.na(AGB13.MgE)))

#Explore covariates data
CongoCarbon_Covariates <- read.csv("./Data/Raw/CongoCarbon_Plot_Covariates.csv")

head(CongoCarbon_Covariates)
summary(CongoCarbon_Covariates)

#Check the class for all columns in CongoCarbon_Covariates
lapply(CongoCarbon_Covariates, class)

################################### Visualize change in AGB over time #############################

#Change data frame from horizontal to vertical
melted_AGB_by_Plot <- melt(AGB_by_Plot, id=c("Plot"))

sum_AGB <- melted_AGB_by_Plot[melted_AGB_by_Plot$variable %like% "sum", ]
change_AGB <- melted_AGB_by_Plot[melted_AGB_by_Plot$variable %like% "change", ]

AGB_change_by_year <- change_AGB %>%
  group_by(variable) %>%
  summarise(sum=sum(value))

#Play around with visualizations

ggplot()+
  geom_point(change_AGB, mapping=aes(x=Plot, y=value, group=Plot, color=variable))

ggplot()+
  geom_boxplot(subset(melted_raw_data,melted_raw_data$value<30 & melted_raw_data$Year=="2005"), mapping=aes(x=Plot, y=value, group=Plot))

ggplot()+
  geom_point(sum_AGB, mapping=aes(x=Plot, y=value, group=Plot, color=variable))

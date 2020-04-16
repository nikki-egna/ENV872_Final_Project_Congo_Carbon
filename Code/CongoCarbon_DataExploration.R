#Data exploration of datasets within the Congo Carbon repository

#Load necessary packges
library(dplyr)

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

#Explore covariates data
CongoCarbon_Covariates <- read.csv("./Data/Raw/CongoCarbon_Plot_Covariates.csv")

head(CongoCarbon_Covariates)
summary(CongoCarbon_Covariates)
lapply(CongoCarbon_Covariates, class)

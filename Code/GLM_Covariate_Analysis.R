######## GLM ANALYSIS #######

#Main effects sum '05
AGB.main.05 <- lm(data = plots_with_covariates, sum_AGB05 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.05)

step(AGB.main.05)

#Best model
AGB.main.05 <- lm(formula = sum_AGB05 ~ Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m, 
   data = plots_with_covariates)
summary(AGB.main.05)

#Main effects sum '09
AGB.main.09 <- lm(data = plots_with_covariates, sum_AGB09 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.09)

step(AGB.main.09)

#Best model
AGB.main.09 <- lm(formula = sum_AGB09 ~ Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m, 
   data = plots_with_covariates)
summary(AGB.main.09)

#Main effects sum '13
AGB.main.13 <- lm(data = plots_with_covariates, sum_AGB13 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.13)

step(AGB.main.13)

#Best model
AGB.main.13 <- lm(formula = sum_AGB13 ~ Precip_sum_2013 + Dist_Road_m + Dist_Village_m + 
                    Dist_PA_m + Dist_Saw_Mills_m, data = plots_with_covariates)
summary(AGB.main.13)

#Main effects change '05-'09
AGB.main.0509 <- lm(data = plots_with_covariates, change0509 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.0509)

step(AGB.main.0509)

#Best model
AGB.main.0509 <- lm(formula = sum_AGB13 ~ Precip_sum_2013 + Dist_Road_m + Dist_Village_m + 
     Dist_PA_m + Dist_Saw_Mills_m, data = plots_with_covariates)
summary(AGB.main.0509)

#Main effects change '09-'13
AGB.main.0913 <- lm(data = plots_with_covariates, change0913 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.0913)

step(AGB.main.0913)

#Best model
AGB.main.0913 <- lm(formula = change0913 ~ GlobCover + Precip_sum_2013, data = plots_with_covariates)
summary(AGB.main.0913)

#Main effects change '05-'13
AGB.main.0513 <- lm(data = plots_with_covariates, change0513 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.0513)

step(AGB.main.0513)

AGB.main.0513 <- lm(formula = change0513 ~ GlobCover + Precip_sum_2013 + Dist_Road_m + 
     Dist_Saw_Mills_m, data = plots_with_covariates)
summary(AGB.main.0513)


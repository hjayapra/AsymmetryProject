#DLPFC and Middle Frontal Asymmetry distribution with respect to predvar
#Statistical Analysis 
#3/07/21 

library(ggplot2)
#age, sex, APOE, race (binary), education (binary), MMS, GDS, FDG_global, PiB_global (binary), ICV
#Excluded MMS 
#Ecluded GDS 

#load Data and Filter 

Asymm <- read.table("C:/Users/remov/Documents/KLU_APC2_Master_List_p012.csv", header = TRUE, sep = ",")
Asymm <- Asymm[which(Asymm$Visit_Relative == 1),]

#Define Predictor Variables 

Asymm$Age <- Asymm$Age_CurrentVisit

Asymm$Education_cat <- Asymm$Education >12

Asymm$Race_cat <- Asymm$Race != "White"

Asymm$Sex <- Asymm$Sex

Asymm$APOE_CODE[Asymm$APOE_CODE == "NaN"] = NA

Asymm$PiBStatus_SUVR_GTM_FS_Global[Asymm$PiBStatus_SUVR_GTM_FS_Global== "NaN"] = NA


#Response 

#FS_Asymm_caudalmiddlefrontal_volume
#FS_Asymm_superiorfrontal_volume
#FS_Asymm_rostralmiddlefrontal_volume
#Include inferior frontal 


#multivariate linear regression model (DLPFC)
#model not significant 
mlm <- lm(cbind(FS_Asymm_caudalmiddlefrontal_volume, FS_Asymm_rostralmiddlefrontal_volume, FS_Asymm_superiorfrontal_volume)~Age + Sex + Education_cat + Race_cat + FDG_SUVR_GTM_FS_Global + PiBStatus_SUVR_GTM_FS_Global + APOE_CODE + FS_IntraCranialVol, data = Asymm)
summary(mlm)


#multivariate linear regression model (middle frontal)
#APOE significant 
mlm1 <- lm(cbind(FS_Asymm_caudalmiddlefrontal_volume, FS_Asymm_rostralmiddlefrontal_volume)~Age + Sex + Education_cat + Race_cat + FDG_SUVR_GTM_FS_Global + PiBStatus_SUVR_GTM_FS_Global + APOE_CODE + FS_IntraCranialVol, data = Asymm)
summary(mlm1)

#linear regression (superior frontal)
mlm2 <- lm(FS_Asymm_superiorfrontal_volume~Age + Sex + Education_cat + Race_cat + FDG_SUVR_GTM_FS_Global + PiBStatus_SUVR_GTM_FS_Global + APOE_CODE + FS_IntraCranialVol, data = Asymm)
summary(mlm2)

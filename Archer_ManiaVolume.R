library(data.table); library(Jmisc); library("lavaan");
library(qgraph); library(psych); library(corrplot); library("psych");
library(ggplot2); library(car); library(compare); library(gdata); 
library(corrplot); library(moments); library(ltm); library(Hmisc); library(dplyr);
library(tidyverse); library(stats); library(na.tools); library(modelr);
library(multiplex)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## load data ##
data<-readRDS('ABCD_5.1.rds')

# make data frame of variables
mania.data<-data[,c(2,7,13:16,22,25,28,31,1546,2857,10666,11894,35671,36439,36441,40034,42938,42942,
                    42946,42950,42954,42958,42962,42966,42970,42974,42978,42982,42986,42990,
                    42994,42998,43002,43006,43010,43014,43018,43022,43026,43030,43034,43038,
                    43042,43046,43050,43054,43058,43062,43066,43070,43074,43078,43082,43086,
                    43090,43094,43098,43102,43106,43110,43114,43118,43122,43126,43130,43134,
                    43138,43142,43146,43150,43154,43158,43162,43166,43170,43174,43178,43182,
                    43186,43190,43194,43198,43202,43202,43206,43210,43214,43218,43222,43226
                    ,43230,43234,43238,43242,43246,43250,43254,43258,43262,43266,43270,43274
                    ,43278,43336,43339,43342,43345,43348,43351,43354,43357,43360,43363,43366,
                    43369,43372,43375,43378,43381,43384,43387,43390,43393,43396,43399,
                    43408,43420,43423,43426,43429,43438,43441,43444,43447,50958:50959,50989)]
mania.data<-mania.data[,-c(85:88,91:105,108,114:120,123:125)]
mania.data<-as.data.frame(mania.data)
mania.data <- mania.data %>% rename("vol_1" = "smri_vol_cdk_banksstslh_b", 
                                    "vol_2" = "smri_vol_cdk_cdacatelh_b",
                                    "vol_3" = "smri_vol_cdk_cdmdfrlh_b", 
                                    "vol_4" = "smri_vol_cdk_cuneuslh_b",
                                    "vol_5" = "smri_vol_cdk_ehinallh_b", 
                                    "vol_6" = "smri_vol_cdk_fusiformlh_b",
                                    "vol_7" = "smri_vol_cdk_ifpllh_b",
                                    "vol_8" = "smri_vol_cdk_iftmlh_b",
                                    "vol_9" = "smri_vol_cdk_ihcatelh_b", 
                                    "vol_10" = "smri_vol_cdk_locclh_b",
                                    "vol_11" = "smri_vol_cdk_lobfrlh_b", 
                                    "vol_12" = "smri_vol_cdk_linguallh_b",
                                    "vol_13" = "smri_vol_cdk_mobfrlh_b", 
                                    "vol_14" = "smri_vol_cdk_mdtmlh_b",
                                    "vol_15" = "smri_vol_cdk_parahpallh_b", 
                                    "vol_16" = "smri_vol_cdk_paracnlh_b",
                                    "vol_17" = "smri_vol_cdk_parsopclh_b", 
                                    "vol_18" = "smri_vol_cdk_parsobislh_b",
                                    "vol_19" = "smri_vol_cdk_parstgrislh_b", 
                                    "vol_20" = "smri_vol_cdk_pericclh_b",
                                    "vol_21" = "smri_vol_cdk_postcnlh_b", 
                                    "vol_22" = "smri_vol_cdk_ptcatelh_b",
                                    "vol_23" = "smri_vol_cdk_precnlh_b", 
                                    "vol_24" = "smri_vol_cdk_pclh_b",
                                    "vol_25" = "smri_vol_cdk_rracatelh_b", 
                                    "vol_26" = "smri_vol_cdk_rrmdfrlh_b",
                                    "vol_27" = "smri_vol_cdk_sufrlh_b", 
                                    "vol_28" = "smri_vol_cdk_supllh_b",
                                    "vol_29" = "smri_vol_cdk_sutmlh_b", 
                                    "vol_30" = "smri_vol_cdk_smlh_b",
                                    "vol_31" = "smri_vol_cdk_frpolelh_b", 
                                    "vol_32" = "smri_vol_cdk_tmpolelh_b",
                                    "vol_33" = "smri_vol_cdk_trvtmlh_b", 
                                    "vol_34" = "smri_vol_cdk_insulalh_b",
                                    "vol_35" = "smri_vol_cdk_banksstsrh_b", 
                                    "vol_36" = "smri_vol_cdk_cdacaterh_b",
                                    "vol_37" = "smri_vol_cdk_cdmdfrrh_b", 
                                    "vol_38" = "smri_vol_cdk_cuneusrh_b",
                                    "vol_39" = "smri_vol_cdk_ehinalrh_b", 
                                    "vol_40" = "smri_vol_cdk_fusiformrh_b",
                                    "vol_41" = "smri_vol_cdk_ifplrh_b", 
                                    "vol_42" = "smri_vol_cdk_iftmrh_b",
                                    "vol_43" = "smri_vol_cdk_ihcaterh_b", 
                                    "vol_44" = "smri_vol_cdk_loccrh_b",
                                    "vol_45" = "smri_vol_cdk_lobfrrh_b", 
                                    "vol_46" = "smri_vol_cdk_lingualrh_b",
                                    "vol_47" = "smri_vol_cdk_mobfrrh_b", 
                                    "vol_48" = "smri_vol_cdk_mdtmrh_b",
                                    "vol_49" = "smri_vol_cdk_parahpalrh_b", 
                                    "vol_50" = "smri_vol_cdk_paracnrh_b",
                                    "vol_51" = "smri_vol_cdk_parsopcrh_b", 
                                    "vol_52" = "smri_vol_cdk_parsobisrh_b",
                                    "vol_53" = "smri_vol_cdk_parstgrisrh_b", 
                                    "vol_54" = "smri_vol_cdk_periccrh_b",
                                    "vol_55" = "smri_vol_cdk_postcnrh_b", 
                                    "vol_56" = "smri_vol_cdk_ptcaterh_b",
                                    "vol_57" = "smri_vol_cdk_precnrh_b", 
                                    "vol_58" = "smri_vol_cdk_pcrh_b",
                                    "vol_59" = "smri_vol_cdk_rracaterh_b", 
                                    "vol_60" = "smri_vol_cdk_rrmdfrrh_b",
                                    "vol_61" = "smri_vol_cdk_sufrrh_b", 
                                    "vol_62" = "smri_vol_cdk_suplrh_b",
                                    "vol_63" = "smri_vol_cdk_sutmrh_b", 
                                    "vol_64" = "smri_vol_cdk_smrh_b",
                                    "vol_65" = "smri_vol_cdk_frpolerh_b", 
                                    "vol_66" = "smri_vol_cdk_tmpolerh_b",
                                    "vol_67" = "smri_vol_cdk_trvtmrh_b", 
                                    "vol_68" = "smri_vol_cdk_insularh_b",
                                    "vol_112" = "smri_vol_scs_crbcortexlh_b",
                                    "vol_113" = "smri_vol_scs_tplh_b",
                                    "vol_114" = "smri_vol_scs_caudatelh_b",
                                    "vol_115" = "smri_vol_scs_putamenlh_b",
                                    "vol_116" = "smri_vol_scs_pallidumlh_b",
                                    "vol_119" = "smri_vol_scs_bstem_b",
                                    "vol_120" = "smri_vol_scs_hpuslh_b",
                                    "vol_121" = "smri_vol_scs_amygdalalh_b",
                                    "vol_124" = "smri_vol_scs_aal_b",
                                    "vol_125" = "smri_vol_scs_vedclh_b",
                                    "vol_130" = "smri_vol_scs_crbcortexrh_b",
                                    "vol_131" = "smri_vol_scs_tprh_b",
                                    "vol_132" = "smri_vol_scs_caudaterh_b",
                                    "vol_133" = "smri_vol_scs_putamenrh_b",
                                    "vol_134" = "smri_vol_scs_pallidumrh_b",
                                    "vol_135" = "smri_vol_scs_hpusrh_b",
                                    "vol_136" = "smri_vol_scs_amygdalarh_b",
                                    "vol_138" = "smri_vol_scs_aar_b",
                                    "vol_139" = "smri_vol_scs_vedcrh_b")
#VERIFY VARIABLE TYPES
names(mania.data)[98]<- c("TICV_b")
names(mania.data)[109]<- c("parent_education_b")
names(mania.data)[2]<- c("age_b")
names(mania.data)[11]<- c("medication_b")
names(mania.data)[12]<- c("income_b")
names(mania.data)[3]<- c("NHWHITE_b")
names(mania.data)[5]<- c("HISPANIC_b")
names(mania.data)[4]<- c("AFRICAN_b")
names(mania.data)[6]<- c("OTHER_b")

mania.data <- mania.data[,c(107,2,1,15,18,108,3:6,98,7:10,12,109,11,14,19:97,99:106,17,16,13)]
names(mania.data)[5]<- c("FAMID_b")
mania.data[is.na(mania.data)]<-"."

mania.data$FEMALE_b <- as.factor(mania.data$FEMALE_b)
mania.data$NHWHITE_b <- as.factor(mania.data$NHWHITE_b)
mania.data$HISPANIC_b <- as.factor(mania.data$HISPANIC_b)
mania.data$AFRICAN_b <- as.factor(mania.data$AFRICAN_b)
mania.data$OTHER_b <- as.factor(mania.data$OTHER_b)
mania.data$medication_b <- as.factor(mania.data$medication_b)
mania.data$COIL2_b <- as.factor(mania.data$COIL2_b)
mania.data$COIL3_b <- as.factor(mania.data$COIL3_b)
mania.data$COIL4_b <- as.factor(mania.data$COIL4_b)
mania.data$COIL5_b <- as.factor(mania.data$COIL5_b)

# change characters to numeric
indx<-sapply(mania.data, is.character)
mania.data[indx]<-lapply(mania.data[indx], function(x) as.numeric(x))

# remove participants with missing brain data
mania.data <- mania.data[mania.data$excluded_missing == '1',]

write.csv(test,"mania.demo.csv", row.names = FALSE)
write.dat(mania.data, "mania.data")





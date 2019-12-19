# Title:                  Melbourne BE-TR Colocated stops linear Model
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   This script performs pretreatment (factor analysis) followed by multivariate multiple linear regression of built environment and sociodemographic variables on transit ridership for a sample of tram and bus locations in Melbourne. 
# Data:                   Ridership data includes average normal (school) weekday ridership 
#                         Train, Tram ridership, averaged for 2018, by Victorian Department of Transport. 
#                         Bus ridership, averaged for 4 months from August - November 2018, provided by Chris Loader from the bus planning team at the Victorian Department of Transport
#                         Refer to [insert doi for figshare] for ontology and reference for built environment.
#                         Copyright statement: This script is the product of Laura Aston


#This is the script for the first run through of factor and cluster analysis, involving
#modes: train/tram/bus
#variables: all
#unit of analysis: 
#                 train-bus sample: 800m catchment originating at train centroid
#                 tram-bus sample: 600 catchment originating at tram centroid
#outlier: TBC
# Analysis method: multivariate multiple regression

#Col headers: 8_Total_Pat	9_Bus_patronage	10_PropComm	11_Balance	12_LUEntropy	13_PedConnect	14_PBN	15_Parkiteer	16_ACDist	17_ACCount	18_FTZ	19_Parking	20_PropUrban	21_PropRural	22_EmpAccess	23_C_LOS	24_O_Bus_LOS	25_O_Tram_LOS	26_O_Train_LOS	27_O_LOS	28_MedInc	29_PropOS	30_PropBach	31_LocalAccess_800	32_EmpDen_800	33_ResidentialDen_800	34_censored_PropFTE	35_censored_MeanSize

library(lm.beta)
library(car)
library(Hmisc)
library(pysych)

#turn off scientific notation
options(scipen = 999)

#read in data
MMLR_Data<-read.csv(file="BE-TR_Co-Located_data.csv")

#Transform patronage into natural log and add as a column to the dataframe
MMLR_Data$X9_Bus_patronage<-as.numeric(MMLR_Data$X9_Bus_patronage)

MMLR_Data<-mutate(MMLR_Data,
                  ln_Bus = log(X9_Bus_patronage),
                  ln_Centroid_Mode = log(X8_Total_Pat),)

MMLR_Data[MMLR_Data == -Inf] <- 0

#optional - assign the Sample ID as the row names
row.names(MMLR_Data) <- MMLR_Data[,c(2)]

#subsetting for mode in the census of all eligible stops

Melb.Trambus<- MMLR_Data[ which(MMLR_Data$Mode=='tram'),]
Melb.Trainbus<- MMLR_Data[ which(MMLR_Data$Mode=='train'),]

#Step 2 estimate covariance of the outcome variables 
cov_Trambus<-cor.test(Melb.Trambus$X9_Bus_patronage, Melb.Trambus$X8_Total_Pat, method = "pearson", conf.level = 0.95)
cov_Trambus #0.43  p < 0.000

cov_Trambus_ln<-cor.test(Melb.Trambus$ln_Bus, Melb.Trambus$ln_Centroid_Mode, method = "pearson", conf.level = 0.95)
cov_Trambus_ln #0.35 p = <0.000

capture.output(cov_Trambus_ln,file="cov_Trambus_ln.txt")

cov_Trainbus<-cor.test(Melb.Trainbus$X9_Bus_patronage, Melb.Trainbus$X8_Total_Pat, method = "pearson", conf.level = 0.95)
cov_Trainbus #0.76, 

cov_Trainbus_ln<-cor.test(Melb.Trainbus$ln_Bus, Melb.Trainbus$ln_Centroid_Mode, method = "pearson", conf.level = 0.95)
cov_Trainbus_ln #0.66, p=<0.000

capture.output(cov_Trainbus_ln,file="cov_Trainbus_ln.txt")

#step 3 Check for multicolinearity
Melb.Trambus.VIF<-vif(lm(ln_Centroid_Mode ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X18_FTZ+ X19_Parking+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X33_ResidentialDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus))
#emoved parking, rural and overlapping level of service to get rid of singularity
Melb.Trambus.VIF
#No multicolinearity
#remove propurban (VIF = 39)

Melb.Trambus.VIF<-vif(lm(ln_Centroid_Mode ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X18_FTZ+ X19_Parking+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X33_ResidentialDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus))
#emoved parking, rural and overlapping level of service to get rid of singularity
Melb.Trambus.VIF
#remove EmpDen (VIF=11)

Melb.Trambus.VIF<-vif(lm(ln_Centroid_Mode ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X18_FTZ+ X19_Parking+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X33_ResidentialDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus))

Melb.Trambus.VIF

#Remove PropFTE
Melb.Trambus.VIF<-vif(lm(ln_Centroid_Mode ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X18_FTZ+ X19_Parking+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X33_ResidentialDen_800+X35_censored_MeanSize, data =Melb.Trambus))

Melb.Trambus.VIF

#step 4 Simple correlations
Corrdata.Trambus<-Melb.Trambus[,c(36,37,10,11,12,13,14,16,17,18,19,22,23,24,26,28,29,30,31,33,35)]

#Option 1 for Correlation matrices with p-values
Corrdata.Trambus<-rcorr(as.matrix(Corrdata.Trambus))

#option 2 for flat correlation matrix
#Set up a custom function to flatten
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

options(max.print=1000000)

Corrdata.Trambus<-flattenCorrMatrix(Corrdata.Trambus$r,Corrdata.Trambus$P)
capture.output(Corrdata.Trambus,file="FlatCor.Trambus.csv")

#not significant for ln_bus
#X12_LUEntropy
#X29_PropOS
#X30_PropBach
#X31_LocalAccess_800
#X35_censored_MeanSize

#ln_Tram
#X19_Parking

#Step 4 maximally adjusted model
#reciprocally sinular until Parking removed)
Melb.Trambus.MMLR.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X18_FTZ+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X33_ResidentialDen_800+X35_censored_MeanSize, data =Melb.Trambus)
summary(Melb.Trambus.MMLR.1)
Anova(Melb.Trambus.MMLR.1)

#remoe AC Dist
Melb.Trambus.MMLR.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+X17_ACCount+ X18_FTZ+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X33_ResidentialDen_800+X35_censored_MeanSize, data =Melb.Trambus)
summary(Melb.Trambus.MMLR.2)
Anova(Melb.Trambus.MMLR.2)

#remove pedoconnect
Melb.Trambus.MMLR.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X14_PBN+X17_ACCount+ X18_FTZ+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X33_ResidentialDen_800+X35_censored_MeanSize, data =Melb.Trambus)
summary(Melb.Trambus.MMLR.3)
Anova(Melb.Trambus.MMLR.3)

#removeFTZ
Melb.Trambus.MMLR.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X14_PBN+X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X33_ResidentialDen_800+X35_censored_MeanSize, data =Melb.Trambus)
summary(Melb.Trambus.MMLR.4)
Anova(Melb.Trambus.MMLR.4)

#removeCensored meanszie
Melb.Trambus.MMLR.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X14_PBN+X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X33_ResidentialDen_800, data =Melb.Trambus)
summary(Melb.Trambus.MMLR.5)
Anova(Melb.Trambus.MMLR.5)

#remove Ac Count
Melb.Trambus.MMLR.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X14_PBN+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X33_ResidentialDen_800, data =Melb.Trambus)
summary(Melb.Trambus.MMLR.6)
Anova(Melb.Trambus.MMLR.6)

#remove propOS
Melb.Trambus.MMLR.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X14_PBN+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X30_PropBach+ X31_LocalAccess_800+ X33_ResidentialDen_800, data =Melb.Trambus)
summary(Melb.Trambus.MMLR.7)
Anova(Melb.Trambus.MMLR.7)

#removeLUEntropy
Melb.Trambus.MMLR.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+  X14_PBN+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X30_PropBach+ X31_LocalAccess_800+ X33_ResidentialDen_800, data =Melb.Trambus)
summary(Melb.Trambus.MMLR.8)
Anova(Melb.Trambus.MMLR.8)

#remove local access
Melb.Trambus.MMLR.9<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+  X14_PBN+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X30_PropBach+ X33_ResidentialDen_800, data =Melb.Trambus)
summary(Melb.Trambus.MMLR.9)
Anova(Melb.Trambus.MMLR.9)

#remvoe residential density
Melb.Trambus.MMLR.10<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+  X14_PBN+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X28_MedInc+ X30_PropBach, data =Melb.Trambus)
summary(Melb.Trambus.MMLR.10)
Anova(Melb.Trambus.MMLR.10)

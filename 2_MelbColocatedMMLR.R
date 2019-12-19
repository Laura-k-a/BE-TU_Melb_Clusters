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

Melb.Trambus.noFTZ<- MMLR_Data[ which(MMLR_Data$Mode=='tram'
                                & MMLR_Data$X18_FTZ=='0'),]
Melb.Trainbus<- MMLR_Data[ which(MMLR_Data$Mode=='train'),]

#Step 2 estimate covariance of the outcome variables 
cov_Trambus.noFTZ<-cor.test(Melb.Trambus.noFTZ$X9_Bus_patronage, Melb.Trambus.noFTZ$X8_Total_Pat, method = "pearson", conf.level = 0.95)
cov_Trambus.noFTZ #0.12  p < 0.02

cov_Trambus_ln.noFTZ<-cor.test(Melb.Trambus.noFTZ$ln_Bus, Melb.Trambus.noFTZ$ln_Centroid_Mode, method = "pearson", conf.level = 0.95)
cov_Trambus_ln.noFTZ #0.29 p = <0.000

capture.output(cov_Trambus_ln.noFTZ,file="cov_Trambus_ln.noFTZ.txt")

cov_Trainbus<-cor.test(Melb.Trainbus$X9_Bus_patronage, Melb.Trainbus$X8_Total_Pat, method = "pearson", conf.level = 0.95)
cov_Trainbus #0.76, 

cov_Trainbus_ln<-cor.test(Melb.Trainbus$ln_Bus, Melb.Trainbus$ln_Centroid_Mode, method = "pearson", conf.level = 0.95)
cov_Trainbus_ln #0.66, p=<0.000

capture.output(cov_Trainbus_ln,file="cov_Trainbus_ln.txt")

#step 3 Check for multicolinearity
Melb.Trambus.noFTZ.VIF<-vif(lm(ln_Centroid_Mode ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ))
#removed parking, rural, urban overlapping level of service to get rid of singularity
Melb.Trambus.noFTZ.VIF

#step 4 Simple correlations
Corrdata.Trambus.noFTZ<-Melb.Trambus.noFTZ[,c(39,40,10,11,12,13,14,16,17,22,23,24,26,29,30,34,35, 36, 37, 38)]

#Option 1 for Correlation matrices with p-values
Corrdata.Trambus.noFTZ<-rcorr(as.matrix(Corrdata.Trambus.noFTZ))

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

Corrdata.Trambus.noFTZ<-flattenCorrMatrix(Corrdata.Trambus.noFTZ$r,Corrdata.Trambus.noFTZ$P)
capture.output(Corrdata.Trambus.noFTZ,file="FlatCor.Trambus.noFTZ.csv")

#not significant for ln_bus
#X11_Balance
#X12_LUEntropy
#X23_C_LOS
#X29_PropOS
#X30_PropBach
#X35_censored_MeanSize


#ln_Tram
#X11_Balance

#exclude balance

#Step 4 maximally adjusted model

Melb.Trambus.noFTZ.MMLR.S1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.S1)
Anova(Melb.Trambus.noFTZ.MMLR.S1)

#remove ACDist
Melb.Trambus.noFTZ.MMLR.S2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.S2)
Anova(Melb.Trambus.noFTZ.MMLR.S2)

#remove ACCount
Melb.Trambus.noFTZ.MMLR.S3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.S4)
Anova(Melb.Trambus.noFTZ.MMLR.S4)

#remove meansize
Melb.Trambus.noFTZ.MMLR.S4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.S4)
Anova(Melb.Trambus.noFTZ.MMLR.S4)

#remove train LOS
Melb.Trambus.noFTZ.MMLR.S5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.S5)
Anova(Melb.Trambus.noFTZ.MMLR.S5)

#remove empden surrogate
Melb.Trambus.noFTZ.MMLR.S6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.S6)
Anova(Melb.Trambus.noFTZ.MMLR.S6)

#remove proportion commercial
Melb.Trambus.noFTZ.MMLR.S7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~  X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.S7)
Anova(Melb.Trambus.noFTZ.MMLR.S7)

#remove PBN
Melb.Trambus.noFTZ.MMLR.S8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~  X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.S8)
Anova(Melb.Trambus.noFTZ.MMLR.S8)

#remove Prop OS
Melb.Trambus.noFTZ.MMLR.S9<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~  X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach+ X34_censored_PropFTE+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.S9)
Anova(Melb.Trambus.noFTZ.MMLR.S9)

#remove population
Melb.Trambus.noFTZ.MMLR.S10<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~  X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS +X30_PropBach+ X34_censored_PropFTE+ DestScore_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.S10)
Anova(Melb.Trambus.noFTZ.MMLR.S10)

#run diagnostics
par(mfrow=c(2,2))
plot(lm(ln_Centroid_Mode ~  X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS +X30_PropBach+ X34_censored_PropFTE+ DestScore_surrogate, data =Melb.Trambus.noFTZ))
#remove 249-tram

plot(lm(ln_Bus ~  X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS +X30_PropBach+ X34_censored_PropFTE+ DestScore_surrogate, data =Melb.Trambus.noFTZ))

which(rownames(Melb.Trambus.noFTZ) == "249-tram") #151

#remove influential outlier
Melb.Trambus.noFTZ.rd2 <- Melb.Trambus.noFTZ[-c(151),]

#rd 2 maximally adjusted
Melb.Trambus.noFTZ.MMLR.S2.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.S2.1)
Anova(Melb.Trambus.noFTZ.MMLR.S2.1)

#remove AC Dist
Melb.Trambus.noFTZ.MMLR.S2.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.S2.2)
Anova(Melb.Trambus.noFTZ.MMLR.S2.2)

#remove AC Count
Melb.Trambus.noFTZ.MMLR.S2.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.S2.3)
Anova(Melb.Trambus.noFTZ.MMLR.S2.3)

#remove pedconnect
Melb.Trambus.noFTZ.MMLR.S2.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.S2.4)
Anova(Melb.Trambus.noFTZ.MMLR.S2.4)

#remove Mean size
Melb.Trambus.noFTZ.MMLR.S2.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.S2.5)
Anova(Melb.Trambus.noFTZ.MMLR.S2.5)

#remove destscore
Melb.Trambus.noFTZ.MMLR.S2.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ Population_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.S2.6)
Anova(Melb.Trambus.noFTZ.MMLR.S2.6)

#remove PBN
Melb.Trambus.noFTZ.MMLR.S2.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ Population_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.S2.7)
Anova(Melb.Trambus.noFTZ.MMLR.S2.7)

#remove Prop comm
Melb.Trambus.noFTZ.MMLR.S2.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~  X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ Population_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.S2.8)
Anova(Melb.Trambus.noFTZ.MMLR.S2.8)

capture.output(Melb.Trambus.noFTZ.MMLR.S2.8,file="Trambus_MMLR_NoFTZ_surrogates.csv")

#diagnostics
plot(lm(ln_Centroid_Mode ~  X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ Population_surrogate, data =Melb.Trambus.noFTZ.rd2))

plot(lm(ln_Bus ~  X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ Population_surrogate, data =Melb.Trambus.noFTZ.rd2))

#see what happens if 258 and 276 removed
#no substantive change

#Trainbus
#step 3 Check for multicolinearity

Melb.Trainbus.VIF<-vif(lm(ln_Centroid_Mode ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X19_Parking+ X21_PropRural + X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X25_O_Tram_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trainbus))
#removed FTZ, urban overlapping level of service to get rid of singularity
Melb.Trainbus.VIF
#no colinearty

#step 4 Simple correlations
Corrdata.Trainbus<-Melb.Trainbus[,c(39,40,10,11,12,13,14,15,16,17,19, 21,22,23,24,25,29,30,34,35, 36, 37, 38)]

#Option 1 for Correlation matrices with p-values
Corrdata.Trainbus<-rcorr(as.matrix(Corrdata.Trainbus))

#option 2 for flat correlation matrix
options(max.print=1000000)

Corrdata.Trainbus<-flattenCorrMatrix(Corrdata.Trainbus$r,Corrdata.Trainbus$P)
capture.output(Corrdata.Trainbus,file="Corrdata.Trainbus.csv")

#not significant for ln_bus
#X13_PedConnect
#X21_PropRural
#X22_EmpAccess
#X25_O_Tram_LOS
#X30_PropBach
#X35_censored_MeanSize
#EmpDen_surrogate
#Population_surrogate

#ln_Tram
#X21_PropRural
#X25_O_Tram_LOS
#X35_censored_MeanSize


#exclude PropRural, Tram LOS and Meansize

#Step 4 maximally adjusted model

Melb.Trainbus.MMLR.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.1)
Anova(Melb.Trainbus.MMLR.1)

#reove population surrogate
Melb.Trainbus.MMLR.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate+ DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.2)
Anova(Melb.Trainbus.MMLR.2)

#remove employment density surrogate
Melb.Trainbus.MMLR.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist +X17_ACCount+ X19_Parking+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.3)
Anova(Melb.Trainbus.MMLR.3)

#remove propFTE
Melb.Trainbus.MMLR.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist +X17_ACCount+ X19_Parking+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.4)
Anova(Melb.Trainbus.MMLR.4)

#remove LUEntropy
Melb.Trainbus.MMLR.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X17_ACCount+ X19_Parking+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.5)
Anova(Melb.Trainbus.MMLR.5)

#remove prop comm
Melb.Trainbus.MMLR.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X17_ACCount+ X19_Parking+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.6)
Anova(Melb.Trainbus.MMLR.6)

#remove employment accesss
Melb.Trainbus.MMLR.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X17_ACCount+ X19_Parking+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.7)
Anova(Melb.Trainbus.MMLR.7)

#remove pedconnect
Melb.Trainbus.MMLR.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X17_ACCount+ X19_Parking+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.8)
Anova(Melb.Trainbus.MMLR.8)

#remove propOS
Melb.Trainbus.MMLR.9<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach+DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.9)
Anova(Melb.Trainbus.MMLR.9)

#remove parking
Melb.Trainbus.MMLR.10<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach+DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.10)
Anova(Melb.Trainbus.MMLR.10)

#remove destinationscore surrogate
Melb.Trainbus.MMLR.11<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.11)
Anova(Melb.Trainbus.MMLR.11)

#diagnostics
plot(lm(ln_Bus ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus))
#outlier: 572-train

plot(lm(ln_Centroid_Mode ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus))

#636 not past cook's distance; but far outlying on the curves
which(rownames(Melb.Trainbus) == "572-train") #14
which(rownames(Melb.Trainbus) == "636-train") #1

#remove influential outlier
Melb.Trainbus.rd2 <- Melb.Trainbus[-c(1, 14),]

#rerun maximally adjusted
Melb.Trainbus.MMLR.2.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.1)
Anova(Melb.Trainbus.MMLR.2.1)

#remove LU Entropy
Melb.Trainbus.MMLR.2.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.2)
Anova(Melb.Trainbus.MMLR.2.2)

#remove destscore
Melb.Trainbus.MMLR.2.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate	+ Population_surrogate, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.3)
Anova(Melb.Trainbus.MMLR.2.3)

#remove pop surrogate
Melb.Trainbus.MMLR.2.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.4)
Anova(Melb.Trainbus.MMLR.2.4)

#remove employment density
Melb.Trainbus.MMLR.2.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.5)
Anova(Melb.Trainbus.MMLR.2.5)

#remove ped connect
Melb.Trainbus.MMLR.2.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.6)
Anova(Melb.Trainbus.MMLR.2.6)

#remove propFTE
Melb.Trainbus.MMLR.2.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.7)
Anova(Melb.Trainbus.MMLR.2.7)

#remove prop comm
Melb.Trainbus.MMLR.2.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.8)
Anova(Melb.Trainbus.MMLR.2.8)

#remove propOS
Melb.Trainbus.MMLR.2.9<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.9)
Anova(Melb.Trainbus.MMLR.2.9)

#remove empaccess
Melb.Trainbus.MMLR.2.10<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.10)
Anova(Melb.Trainbus.MMLR.2.10)

#remove parking
Melb.Trainbus.MMLR.2.11<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.11)
Anova(Melb.Trainbus.MMLR.2.11)

#diagnostics
plot(lm(ln_Centroid_Mode ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus.rd2))
#affectig shape (assumptions): 615-train, 632

plot(lm(ln_Bus ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus.rd2))
#affecting shape (assumptions): 451 train

#636 not past cook's distance; but far outlying on the curves
which(rownames(Melb.Trainbus.rd2) == "615-train") #2
which(rownames(Melb.Trainbus.rd2) == "632-train") #1
which(rownames(Melb.Trainbus.rd2) == "451-train") #13

#remove influential outlier
Melb.Trainbus.rd3 <- Melb.Trainbus.rd2[-c(1, 2, 13),]

Melb.Trainbus.MMLR.3.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate	+ Population_surrogate+ DestScore_surrogate, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.1)
Anova(Melb.Trainbus.MMLR.3.1)

#remove dest score
Melb.Trainbus.MMLR.3.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate	+ Population_surrogate, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.2)
Anova(Melb.Trainbus.MMLR.3.2)

#remove pop
Melb.Trainbus.MMLR.3.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ EmpDen_surrogate, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.3)
Anova(Melb.Trainbus.MMLR.3.3)

#remove empden
Melb.Trainbus.MMLR.3.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.4)
Anova(Melb.Trainbus.MMLR.3.4)

#remove prop bachelors
Melb.Trainbus.MMLR.3.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X34_censored_PropFTE, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.5)
Anova(Melb.Trainbus.MMLR.3.5)

#prop FTE
Melb.Trainbus.MMLR.3.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.6)
Anova(Melb.Trainbus.MMLR.3.6)

#peconnect
Melb.Trainbus.MMLR.3.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.7)
Anova(Melb.Trainbus.MMLR.3.7)

#prop OS
Melb.Trainbus.MMLR.3.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.8)
Anova(Melb.Trainbus.MMLR.3.8)

#prop comm
Melb.Trainbus.MMLR.3.9<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X12_LUEntropy+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.9)
Anova(Melb.Trainbus.MMLR.3.9)

#LUEntropy
Melb.Trainbus.MMLR.3.10<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.10)
Anova(Melb.Trainbus.MMLR.3.10)

#ACCount
Melb.Trainbus.MMLR.3.11<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking + X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.11)
Anova(Melb.Trainbus.MMLR.3.11)

#Balance
Melb.Trainbus.MMLR.3.12<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking + X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.12)
Anova(Melb.Trainbus.MMLR.3.12)

#diagnostics
plot(lm(ln_Bus ~ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking + X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS, data =Melb.Trainbus.rd3))
#explore 493 (well of the normal QQ plot) #checked, removing reduces explanatory power
#looks ok so if unchanged revert to this solution (and be sure to plot)
plot(lm(ln_Centroid_Mode~ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking + X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS, data =Melb.Trainbus.rd3))

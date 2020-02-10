# Title:                  Melbourne BE-TR Colocated stops linear Model
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   This script performs multivariate multiple linear regression of builtenvironment and sociodemographic variables on transit ridership for a sample of tram and bus locations in Melbourne. 
# Data:                   Ridership data includes average normal (school) weekday ridership Train, Tram ridership, averaged for 2018, by Victorian Department of Transport. 
#                         Bus ridership, averaged for 4 months from August - November 2018,provided by Chris Loader from the bus planning team at the Victorian Department of Transport
#                         Refer to [insert doi for figshare] for ontology and reference for built environment.
#                         Copyright statement: This script is the product of Laura Aston


#modes: train-bus /tram-bus
#variables: all
#unit of analysis: 
#                 train-bus sample: 800m catchment originating at train centroid
#                 tram-bus sample: 600 catchment originating at tram centroid
#outlier: TBC
#Analysis method: multivariate multiple regression

#Col headers: ln_Centroid +ln_bus ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X27_O_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach + ln_emp + ln_pop

library(lm.beta)
library(car)
library(Hmisc)
library(psych)
library(dplyr)

#turn off scientific notation
options(scipen = 999)
options(max.print= 1000000)


#Set working directory
setwd("C:/Users/lkast1/Google Drive/PhD/2.Analysis/2. Empirical Analysis/BE-TR_Multi Country Samples/Melbourne/Melb.All.Stops/Melb.AllStops.Repo/Data")
#read in data

Colocated_Data<-read.csv(file="BE-TR_Co-Located_data.csv")

#optional - assign the Sample ID as the row names
row.names(Colocated_Data) <- Colocated_Data[,c(2)]

#subsetting for mode in the census of all eligible stops

Melb.Trambus.noFTZ<- Colocated_Data[ which(Colocated_Data$Mode_Centroid=='tram'
                                & Colocated_Data$X19._FTZ=='0'),]
Melb.Trainbus<- Colocated_Data[ which(Colocated_Data$Mode_Centroid=='train'),]

#Step 2 estimate covariance of the outcome variables
cov_Trambus.noFTZ<-cor.test(Melb.Trambus.noFTZ$ln_bus, Melb.Trambus.noFTZ$ln_Centroid, method = "pearson", conf.level = 0.95)
cov_Trambus.noFTZ #0.29  p < 0.000

capture.output(cov_Trambus.noFTZ,file="cov_Trambus_noFTZ.txt")

cov_Trainbus<-cor.test(Melb.Trainbus$ln_bus, Melb.Trainbus$ln_Centroid, method = "pearson", conf.level = 0.95)
cov_Trainbus #0.67 p<0.00000 

capture.output(cov_Trainbus,file="cov_Trainbus.txt")

setwd("C:\Users\lkast1\Google Drive\PhD\2.Analysis\2. Empirical Analysis\BE-TR_Multi Country Samples\Melbourne\Melb.All.Stops\Melb.AllStops.Repo\Regression outputs\Colocoated")


#step 3 Check for multicolinearity
Melb.Trambus.noFTZ.VIF<-vif(lm(ln_Centroid ~ ln_pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ))
#removed  overlapping level of service, parkiteer to get rid of singularity
#removed ln_emp (high VIF)
Melb.Trambus.noFTZ.VIF

#step 4 Simple correlations
Corrdata.Trambus.noFTZ<-Melb.Trambus.noFTZ[,c(24, 26:30, 34, 35, 36, 38:41, 44, 46, 47, 49, 50, 54, 55, 57)]

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
#X8_Balance
#X9_LUEntropy
#X10_HousingDiv
#X16_CBDDist
#X23_C_LOS
#X29_MeanSize
#X31_PropOS
#X32_PropBach

#not significant for Tram
#X8_Balance
#X20_Parking
#X28_Prop FTE

#exclude balance

#Step 4 maximally adjusted model

Melb.Trambus.noFTZ.MMLR.1<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.1)
#was signular until parking removed
Anova(Melb.Trambus.noFTZ.MMLR.1)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.1), file = "Trambus.MA.summary.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.1), file = "Trambus.MA.Anova.txt")

#MA with standardized coefficients for bus
Trambus.bus.noFTZ.MMLR.1<-lm.beta(lm(ln_bus ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ))

capture.output(summary(Trambus.bus.noFTZ.MMLR.1), file = "Trambus.bus.MA.summary.txt")

Trambus.tram.noFTZ.MMLR.1<-lm.beta(lm(ln_Centroid ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ))

capture.output(summary(Trambus.tram.noFTZ.MMLR.1), file = "Trambus.tram.MA.summary.txt")


#Remove AC Count
Melb.Trambus.noFTZ.MMLR.2<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ)

summary(Melb.Trambus.noFTZ.MMLR.2)
Anova(Melb.Trambus.noFTZ.MMLR.2)

#ACDIst
Melb.Trambus.noFTZ.MMLR.3<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.3)
Anova(Melb.Trambus.noFTZ.MMLR.3)

#Parking
Melb.Trambus.noFTZ.MMLR.4<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.4)
Anova(Melb.Trambus.noFTZ.MMLR.4)

#PedConnect
Melb.Trambus.noFTZ.MMLR.5<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.5)
Anova(Melb.Trambus.noFTZ.MMLR.5)

#Meansize
Melb.Trambus.noFTZ.MMLR.6<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.6)
Anova(Melb.Trambus.noFTZ.MMLR.6)

#Prop Urban
Melb.Trambus.noFTZ.MMLR.7<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.7)
Anova(Melb.Trambus.noFTZ.MMLR.7)

#PropOS
Melb.Trambus.noFTZ.MMLR.8<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X32_PropBach, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.8)
Anova(Melb.Trambus.noFTZ.MMLR.8)

#PropFTE
Melb.Trambus.noFTZ.MMLR.9<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X32_PropBach, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.9)
Anova(Melb.Trambus.noFTZ.MMLR.9)


capture.output(summary(Melb.Trambus.noFTZ.MMLR.9), file = "Trambus.PM.summary.rd1.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.9), file = "Trambus.PM.Anova.rd1.txt")

#standardised coefficients for PM rd 1
#bus
Melb.Trambus.bus.noFTZ.MMLR.9<-lm.beta(lm(ln_bus ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X32_PropBach, data =Melb.Trambus.noFTZ))

capture.output(summary(Melb.Trambus.bus.noFTZ.MMLR.9), file = "Trambus.bus.PM.summary.rd1.txt")

Melb.Trambus.tram.noFTZ.MMLR.9<-lm.beta(lm(ln_Centroid ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X32_PropBach, data =Melb.Trambus.noFTZ))

capture.output(summary(Melb.Trambus.tram.noFTZ.MMLR.9), file = "Trambus.tram.PM.summary.rd1.txt")


#diagnostics
par(mfrow=c(2,2))

plot(lm(ln_Centroid ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X32_PropBach, data =Melb.Trambus.noFTZ))

#249 is influential outlier

plot(lm(ln_bus ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X32_PropBach, data =Melb.Trambus.noFTZ))

#258 is outlier, but don't remove

which(rownames(Melb.Trambus.noFTZ) == "249-tram") #151

#remove potentially influential outliers (just the ones close to cook's distance)
Melb.Trambus.noFTZ.rd2 <- Melb.Trambus.noFTZ[-c(151),]


#Round 2 maximally adjusted model
Melb.Trambus.noFTZ.MMLR.2.1<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.1)
Anova(Melb.Trambus.noFTZ.MMLR.2.1)


capture.output(summary(Melb.Trambus.noFTZ.MMLR.2.1), file = "trambus.MA.summary.rd2.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.2.1), file = "trambus.MA.Anova.rd2.txt")

#standardised coefficients
Melb.Trambus.bus.noFTZ.MMLR.2.1<-lm.beta(lm(ln_bus ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd2))

capture.output(summary(Melb.Trambus.bus.noFTZ.MMLR.2.1), file = "trambus.MA.bus.summary.rd2.txt")

Melb.Trambus.tram.noFTZ.MMLR.2.1<-lm.beta(lm(ln_Centroid ~ ln_pop + X6_PropComm +X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd2))

capture.output(summary(Melb.Trambus.tram.noFTZ.MMLR.2.1), file = "trambus.MA.tram.summary.rd2.txt")

#remove AC Count
Melb.Trambus.noFTZ.MMLR.2.2<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.2)
Anova(Melb.Trambus.noFTZ.MMLR.2.2)

#AC Dist
Melb.Trambus.noFTZ.MMLR.2.3<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X20_Parking+ X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.3)
Anova(Melb.Trambus.noFTZ.MMLR.2.3)

#Parking
Melb.Trambus.noFTZ.MMLR.2.3<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm +  X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.3)
Anova(Melb.Trambus.noFTZ.MMLR.2.3)

#MeanSize
Melb.Trambus.noFTZ.MMLR.2.4<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.4)
Anova(Melb.Trambus.noFTZ.MMLR.2.4)

#Pedconnect
Melb.Trambus.noFTZ.MMLR.2.5<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.5)
Anova(Melb.Trambus.noFTZ.MMLR.2.5)

#PropUrban
Melb.Trambus.noFTZ.MMLR.2.6<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.6)
Anova(Melb.Trambus.noFTZ.MMLR.2.6)

#PropOS
Melb.Trambus.noFTZ.MMLR.2.7<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE +  X32_PropBach, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.7)
Anova(Melb.Trambus.noFTZ.MMLR.2.7)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.2.7), file = "trambus.PM.summary.rd.2.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.2.7), file = "trambus.PM.Anova.rd.2.txt")

#standardised coefficients
Melb.Trambus.bus.noFTZ.MMLR.2.7<-lm.beta(lm(ln_bus ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE +  X32_PropBach, data =Melb.Trambus.noFTZ.rd2))
capture.output(summary(Melb.Trambus.bus.noFTZ.MMLR.2.7), file = "trambus.PM.bus.summary.rd.2.txt")

Melb.Trambus.tram.noFTZ.MMLR.2.7<-lm.beta(lm(ln_Centroid ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE +  X32_PropBach, data =Melb.Trambus.noFTZ.rd2))
capture.output(summary(Melb.Trambus.tram.noFTZ.MMLR.2.7), file = "trambus.PM.tram.summary.rd.2.txt")

#dianostics
plot(lm(ln_Centroid ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE +  X32_PropBach, data =Melb.Trambus.noFTZ.rd2))
#no distinctive pattern, although 210, 276 and 1942 deviate fom the residual plot and are outliers on all graphs. Investigate graphically

plot(lm(ln_bus ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE +  X32_PropBach, data =Melb.Trambus.noFTZ.rd2))
#deviates from normality assumption. 
#Remove 258 tram

which(rownames(Melb.Trambus.noFTZ.rd2) == "258-tram") #151

#remove potentially influential outliers (just the ones close to cook's distance)
Melb.Trambus.noFTZ.rd3 <- Melb.Trambus.noFTZ.rd2[-c(151),]

#Round 3 maximally adjusted model
Melb.Trambus.noFTZ.MMLR.3.1<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.1)
Anova(Melb.Trambus.noFTZ.MMLR.3.1)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.3.1), file = "trambus.MA.summary.rd3.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.3.1), file = "trambus.MA.Anova.rd3.txt")

#standardised coefficients
Melb.Trambus.bus.noFTZ.MMLR.3.1<-lm.beta(lm(ln_bus ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd3))

capture.output(summary(Melb.Trambus.bus.noFTZ.MMLR.3.1), file = "trambus.MA.bus.summary.rd3.txt")

Melb.Trambus.tram.noFTZ.MMLR.3.1<-lm.beta(lm(ln_Centroid ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd3))

capture.output(summary(Melb.Trambus.tram.noFTZ.MMLR.3.1), file = "trambus.MA.tram.summary.rd3.txt")


#remove ACDist
Melb.Trambus.noFTZ.MMLR.3.2<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X18_ACCount + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.2)
Anova(Melb.Trambus.noFTZ.MMLR.3.2)

#AC Count
Melb.Trambus.noFTZ.MMLR.3.3<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X20_Parking + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.3)
Anova(Melb.Trambus.noFTZ.MMLR.3.3)

#Parking
Melb.Trambus.noFTZ.MMLR.3.4<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.4)
Anova(Melb.Trambus.noFTZ.MMLR.3.4)

#meansize
Melb.Trambus.noFTZ.MMLR.3.5<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.5)
Anova(Melb.Trambus.noFTZ.MMLR.3.5)

#PropUrban
Melb.Trambus.noFTZ.MMLR.3.6<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.6)
Anova(Melb.Trambus.noFTZ.MMLR.3.6)

#PedConnect
Melb.Trambus.noFTZ.MMLR.3.7<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X31_PropOS + X32_PropBach, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.7)
Anova(Melb.Trambus.noFTZ.MMLR.3.7)

#PropOS
Melb.Trambus.noFTZ.MMLR.3.8<-lm(cbind(ln_bus,ln_Centroid) ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X32_PropBach, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.8)
Anova(Melb.Trambus.noFTZ.MMLR.3.8)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.3.8), file = "trambus.PM.summary.rd.3.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.3.8), file = "trambus.PM.Anova.rd.3.txt")

#standardised coefficients
Melb.Trambus.bus.noFTZ.MMLR.3.8<-lm.beta(lm(ln_bus ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X32_PropBach, data =Melb.Trambus.noFTZ.rd3))
capture.output(summary(Melb.Trambus.bus.noFTZ.MMLR.3.8), file = "trambus.bus.PM.summary.rd.3.txt")

Melb.Trambus.tram.noFTZ.MMLR.3.8<-lm.beta(lm(ln_Centroid ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X32_PropBach, data =Melb.Trambus.noFTZ.rd3))
capture.output(summary(Melb.Trambus.tram.noFTZ.MMLR.3.8), file = "trambus.tram.PM.summary.rd.3.txt")

#diagnostics
plot(lm(ln_Centroid ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X32_PropBach, data =Melb.Trambus.noFTZ.rd3))
#appears as per rd 2

plot(lm(ln_bus ~ ln_pop + X6_PropComm + X9_LUEntropy + X10_HousingDiv + X12_PBN + X13_DestScore + X16_CBDDist + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X28_PropFTE + X32_PropBach, data =Melb.Trambus.noFTZ.rd3))
#Slightly more normal looking plots, no major outliers.


#trainbus
#step 3 Check for multicolinearity

Melb.Trainbus.VIF<-vif(lm(ln_Centroid ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X29_MeanSize + X31_PropOS + ln_pop, data =Melb.Trainbus))

#removed overlapping train level of service to get rid of singularity
#remove ln_emp (VIF = 6.16), lnb_O_tram_LOS (VIF = 7.43), ln_O_LOS, Prop Bach, Prop Urban (as proxy for ln_Pop)
Melb.Trainbus.VIF

#step 4 Simple correlations
Corrdata.Trainbus<-Melb.Trainbus[,c(54, 55, 57, 24, 26:29, 31, 33:36, 38, 40, 42, 42, 46, 47, 49)]

#Option 1 for Correlation matrices with p-values
Corrdata.Trainbus<-rcorr(as.matrix(Corrdata.Trainbus))

#option 2 for flat correlation matrix
options(max.print=1000000)

Corrdata.Trainbus<-flattenCorrMatrix(Corrdata.Trainbus$r,Corrdata.Trainbus$P)
capture.output(Corrdata.Trainbus,file="Corrdata.Trainbus.csv")

#NS for bus:
#ln_pop
#X11_PedConnect
#X16_CBDDist
#X22_EmpAccess
#X29_MeanSize


#NS for train
#X29_MeanSize

#remove #mean size

#Maximally adjsuted model
Melb.Trainbus.MMLR.1<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.1)
Anova(Melb.Trainbus.MMLR.1)

capture.output(summary(Melb.Trainbus.MMLR.1), file = "trainbus.MA.summary.txt")
capture.output(Anova(Melb.Trainbus.MMLR.1), file = "trainbus.MA.Anova.txt")

#standardised coefficients
Melb.Trainbus.bus.MMLR.1<-lm.beta(lm(ln_bus ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus))

capture.output(summary(Melb.Trainbus.bus.MMLR.1), file = "trainbus.bus.MA.summary.txt")

Melb.Trainbus.train.MMLR.1<-lm.beta(lm(ln_Centroid ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus))

capture.output(summary(Melb.Trainbus.train.MMLR.1), file = "trainbus.train.MA.summary.txt")

#LUEntropy
Melb.Trainbus.MMLR.2<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.2)
Anova(Melb.Trainbus.MMLR.2)

#PropFTE
Melb.Trainbus.MMLR.3<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X31_PropOS + ln_pop, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.3)
Anova(Melb.Trainbus.MMLR.3)

#DestScore
Melb.Trainbus.MMLR.4<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X31_PropOS + ln_pop, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.4)
Anova(Melb.Trainbus.MMLR.4)

#EmpAccess
Melb.Trainbus.MMLR.5<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking +X23_C_LOS + X24_O_Bus_LOS + X31_PropOS + ln_pop, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.5)
Anova(Melb.Trainbus.MMLR.5)

#PropOS
Melb.Trainbus.MMLR.6<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking +X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.6)
Anova(Melb.Trainbus.MMLR.6)

#HousingDiv
Melb.Trainbus.MMLR.7<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking +X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.7)
Anova(Melb.Trainbus.MMLR.7)

#ln_pop
Melb.Trainbus.MMLR.8<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking +X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.8)
Anova(Melb.Trainbus.MMLR.8)

#PropComm
Melb.Trainbus.MMLR.9<-lm(cbind(ln_Centroid, ln_bus) ~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking +X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.9)
Anova(Melb.Trainbus.MMLR.9)

#pedConnect
Melb.Trainbus.MMLR.10<-lm(cbind(ln_Centroid, ln_bus) ~ X8_Balance + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking +X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.10)
Anova(Melb.Trainbus.MMLR.10)

capture.output(summary(Melb.Trainbus.MMLR.10), file = "trainbus.PM.summary.txt")
capture.output(Anova(Melb.Trainbus.MMLR.10), file = "trainbus.PM.Anova.txt")

#standardised coefficients
Melb.Trainbus.bus.MMLR.10<-lm.beta(lm(ln_bus ~ X8_Balance + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking +X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus))
capture.output(summary(Melb.Trainbus.bus.MMLR.10), file = "trainbus.bus.PM.summary.txt")

Melb.Trainbus.train.MMLR.10<-lm.beta(lm(ln_Centroid ~ X8_Balance + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking +X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus))
capture.output(summary(Melb.Trainbus.train.MMLR.10), file = "trainbus.train.PM.summary.txt")


#diagnostics
par(mfrow=c(2,2))
plot(lm(ln_bus ~ X8_Balance + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking +X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus))
#generally seems to meet assumptions with no influential outliers. Most outlying values are 572, 451. 547

plot(lm(ln_Centroid ~ X8_Balance + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking +X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus))
#636 major outliers affecting heteroscedasticity and with largest deviation from normal QQ plot

which(rownames(Melb.Trainbus) == "636-train") #1

Melb.Trainbus.rd2 <- Melb.Trainbus[-c(1),]

#rd 2 maximally adjusted model
Melb.Trainbus.MMLR.2.1<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.1)
Anova(Melb.Trainbus.MMLR.2.1)

capture.output(summary(Melb.Trainbus.MMLR.2.1), file = "trainbus.MA.summary.rd.2.txt")
capture.output(Anova(Melb.Trainbus.MMLR.2.1), file = "trainbus.MA.Anova.rd.2.txt")

#standardised coefficients
Melb.Trainbus.bus.MMLR.2.1<-lm.beta(lm(ln_bus ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd2))

capture.output(summary(Melb.Trainbus.bus.MMLR.2.1), file = "trainbus.bus.MA.summary.rd.2.txt")

Melb.Trainbus.train.MMLR.2.1<-lm.beta(lm(ln_Centroid ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd2))

capture.output(summary(Melb.Trainbus.train.MMLR.2.1), file = "trainbus.train.MA.summary.rd.2.txt")

#LUEntropy
Melb.Trainbus.MMLR.2.2<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.2)
Anova(Melb.Trainbus.MMLR.2.2)

#PropFTE
Melb.Trainbus.MMLR.2.3<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X31_PropOS + ln_pop, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.3)
Anova(Melb.Trainbus.MMLR.2.3)

#DestScore
Melb.Trainbus.MMLR.2.4<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X31_PropOS + ln_pop, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.4)
Anova(Melb.Trainbus.MMLR.2.4)

#PropOS
Melb.Trainbus.MMLR.2.5<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.5)
Anova(Melb.Trainbus.MMLR.2.5)

#Housing Div
Melb.Trainbus.MMLR.2.6<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X11_PedConnect + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.6)
Anova(Melb.Trainbus.MMLR.2.6)

#ln_Pop
Melb.Trainbus.MMLR.2.7<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X11_PedConnect + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.7)
Anova(Melb.Trainbus.MMLR.2.7)

#PropComm
Melb.Trainbus.MMLR.2.8<-lm(cbind(ln_Centroid, ln_bus) ~ X8_Balance + X11_PedConnect + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.8)
Anova(Melb.Trainbus.MMLR.2.8)

#EmpAccess
Melb.Trainbus.MMLR.2.9<-lm(cbind(ln_Centroid, ln_bus) ~ X8_Balance + X11_PedConnect + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.9)
Anova(Melb.Trainbus.MMLR.2.9)

#Pedconnect
Melb.Trainbus.MMLR.2.10<-lm(cbind(ln_Centroid, ln_bus) ~ X8_Balance + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.10)
Anova(Melb.Trainbus.MMLR.2.10)

capture.output(summary(Melb.Trainbus.MMLR.2.10), file = "trainbus.PM.summaryrd.2.txt")
capture.output(Anova(Melb.Trainbus.MMLR.2.10), file = "trainbus.PM.Anova.rd.2.txt")

#standardised coefficients
Melb.Trainbus.bus.MMLR.2.10<-lm.beta(lm(ln_bus ~ X8_Balance + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus.rd2))

capture.output(summary(Melb.Trainbus.bus.MMLR.2.10), file = "trainbus.bus.PM.summaryrd.2.txt")

Melb.Trainbus.train.MMLR.2.10<-lm.beta(lm(ln_Centroid ~ X8_Balance + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus.rd2))

capture.output(summary(Melb.Trainbus.train.MMLR.2.10), file = "trainbus.bus.PM.summaryrd.2.txt")


#diagnostics
plot(lm(ln_bus ~ X8_Balance + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus.rd2))
#547, 451, 572 more pronounced on the residuals and heteroskedasticity plots - investigate; but lines fit assumptions so leave

plot(lm(ln_Centroid ~ X8_Balance + X12_PBN +  X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X23_C_LOS + X24_O_Bus_LOS, data =Melb.Trainbus.rd2))
#Severe deviation from assumptions. 632, 615 bordering Cook's distance--> remove
which(rownames(Melb.Trainbus.rd2) == "615-train") #2
which(rownames(Melb.Trainbus.rd2) == "632-train") #1

Melb.Trainbus.rd3 <- Melb.Trainbus.rd2[-c(1, 2),]

#rd 3 MA
Melb.Trainbus.MMLR.3.1<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.1)
Anova(Melb.Trainbus.MMLR.3.1)

capture.output(summary(Melb.Trainbus.MMLR.3.1), file = "trainbus.MA.summary.rd.3.txt")
capture.output(Anova(Melb.Trainbus.MMLR.3.1), file = "trainbus.MA.Anova.rd.3.txt")

#standardised coefficients
Melb.Trainbus.bus.MMLR.3.1<-lm.beta(lm(ln_bus ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd3))

capture.output(summary(Melb.Trainbus.bus.MMLR.3.1), file = "trainbus.bus.MA.summary.rd.3.txt")

Melb.Trainbus.train.MMLR.3.1<-lm.beta(lm(ln_Centroid ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd3))

capture.output(summary(Melb.Trainbus.train.MMLR.3.1), file = "trainbus.train.MA.summary.rd.3.txt")

#destscore
Melb.Trainbus.MMLR.3.2<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.2)
Anova(Melb.Trainbus.MMLR.3.2)

#LU Entropy
Melb.Trainbus.MMLR.3.3<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd3)

summary(Melb.Trainbus.MMLR.3.3)
Anova(Melb.Trainbus.MMLR.3.3)

#propFTE
Melb.Trainbus.MMLR.3.4<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X31_PropOS + ln_pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.4)
Anova(Melb.Trainbus.MMLR.3.4)

#PropOS
Melb.Trainbus.MMLR.3.5<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.5)
Anova(Melb.Trainbus.MMLR.3.5)

#CBD Dist
Melb.Trainbus.MMLR.3.6<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X10_HousingDiv + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.6)
Anova(Melb.Trainbus.MMLR.3.6)

#Housing Div
Melb.Trainbus.MMLR.3.7<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.7)
Anova(Melb.Trainbus.MMLR.3.7)

#propcomm
Melb.Trainbus.MMLR.3.8<-lm(cbind(ln_Centroid, ln_bus) ~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.8)
Anova(Melb.Trainbus.MMLR.3.8)

#AC_Dist
Melb.Trainbus.MMLR.3.9<-lm(cbind(ln_Centroid, ln_bus) ~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.9)
Anova(Melb.Trainbus.MMLR.3.9)


capture.output(summary(Melb.Trainbus.MMLR.3.9), file = "trainbus.PM.summary.rd.3.txt")
capture.output(Anova(Melb.Trainbus.MMLR.3.9), file = "trainbus.PM.Anova.rd.3.txt")

#standardised coefficients
Melb.Trainbus.bus.MMLR.3.9<-lm.beta(lm(ln_bus ~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd3))

capture.output(summary(Melb.Trainbus.bus.MMLR.3.9), file = "trainbus.bus.PM.summary.rd.3.txt")

Melb.Trainbus.train.MMLR.3.9<-lm.beta(lm(ln_Centroid ~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd3))

capture.output(summary(Melb.Trainbus.train.MMLR.3.9), file = "trainbus.train.PM.summary.rd.3.txt")

#diagnostics
plot(lm(ln_Centroid~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd3))
#Some outliers but not influential and generally conforms to assumptions.  

plot(lm(ln_bus~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd3))
#547, #451, #527 now clearly deviating from other data points. remove

which(rownames(Melb.Trainbus.rd3) == "547-train") #36
which(rownames(Melb.Trainbus.rd3) == "451-train") #12
which(rownames(Melb.Trainbus.rd3) == "527-train") #104

Melb.Trainbus.rd4 <- Melb.Trainbus.rd3[-c(36, 12, 104),]

#rd 4 MA
Melb.Trainbus.MMLR.4.1<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd4)
summary(Melb.Trainbus.MMLR.4.1)
Anova(Melb.Trainbus.MMLR.4.1)

capture.output(summary(Melb.Trainbus.MMLR.4.1), file = "trainbus.MA.summary.rd.4.txt")
capture.output(Anova(Melb.Trainbus.MMLR.4.1), file = "trainbus.MA.Anova.rd.4.txt")

#standardised coefficients
Melb.Trainbus.bus.MMLR.4.1<-lm.beta(lm(ln_bus ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd4))

capture.output(summary(Melb.Trainbus.bus.MMLR.4.1), file = "trainbus.bus.MA.summary.rd.4.txt")

Melb.Trainbus.train.MMLR.4.1<-lm.beta(lm(ln_Centroid ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd4))

capture.output(summary(Melb.Trainbus.train.MMLR.4.1), file = "trainbus.train.MA.summary.rd.4.txt")

#CBDDist
Melb.Trainbus.MMLR.4.2<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd4)
summary(Melb.Trainbus.MMLR.4.2)
Anova(Melb.Trainbus.MMLR.4.2)

#DestScore
Melb.Trainbus.MMLR.4.3<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X28_PropFTE + X31_PropOS + ln_pop, data =Melb.Trainbus.rd4)
summary(Melb.Trainbus.MMLR.4.3)
Anova(Melb.Trainbus.MMLR.4.3)

#propfte
Melb.Trainbus.MMLR.4.4<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X31_PropOS + ln_pop, data =Melb.Trainbus.rd4)
summary(Melb.Trainbus.MMLR.4.4)
Anova(Melb.Trainbus.MMLR.4.4)

#HousingDiv
Melb.Trainbus.MMLR.4.5<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X9_LUEntropy + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X31_PropOS + ln_pop, data =Melb.Trainbus.rd4)
summary(Melb.Trainbus.MMLR.4.5)
Anova(Melb.Trainbus.MMLR.4.5)

#LUEntropy
Melb.Trainbus.MMLR.4.6<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X31_PropOS + ln_pop, data =Melb.Trainbus.rd4)
summary(Melb.Trainbus.MMLR.4.6)
Anova(Melb.Trainbus.MMLR.4.6)

#PropOS
Melb.Trainbus.MMLR.4.7<-lm(cbind(ln_Centroid, ln_bus) ~ X6_PropComm + X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd4)
summary(Melb.Trainbus.MMLR.4.7)
Anova(Melb.Trainbus.MMLR.4.7)

#PropComm
Melb.Trainbus.MMLR.4.8<-lm(cbind(ln_Centroid, ln_bus) ~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd4)
summary(Melb.Trainbus.MMLR.4.8)
Anova(Melb.Trainbus.MMLR.4.8)

capture.output(summary(Melb.Trainbus.MMLR.4.8), file = "trainbus.PM.summary.rd.4.txt")
capture.output(Anova(Melb.Trainbus.MMLR.4.8), file = "trainbus.PM.Anova.rd.4.txt")

#standardised coefficients
Melb.Trainbus.bus.MMLR.4.8<-lm.beta(lm(ln_bus~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd4))

capture.output(summary(Melb.Trainbus.bus.MMLR.4.8), file = "trainbus.bus.PM.summary.rd.4.txt")

Melb.Trainbus.train.MMLR.4.8<-lm.beta(lm(ln_Centroid~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd4))

capture.output(summary(Melb.Trainbus.train.MMLR.4.8), file = "trainbus.train.PM.summary.rd.4.txt")

#diagnostics
plot(lm(ln_Centroid ~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd4))
#493 is outlier. Scale location only does not fit assumptions. Retain. 

plot(lm(ln_bus ~ X8_Balance + X11_PedConnect + X12_PBN + X15_Parkiteer + X17_ACDist + X18_ACCount + X20_Parking + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + ln_pop, data =Melb.Trainbus.rd4))
#572 is outlier but generally fits assumptions. investigate but retain. 
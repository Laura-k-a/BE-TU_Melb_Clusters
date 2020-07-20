# Title:                  Melbourne BE-TR Census stops final models stepwise 
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   This script performs multivariate multiple linear regression of built environment and sociodemographic variables on transit ridership for the census sample of tram and bus locations in Melbourne. The purpose of this script is to exert more control over the stepwise removal of the variables, instead of using R's inbuilt regb function which finds a parsimonious model based on optimisation of AIC only. Instead, this model removes variables in a stepwisde fashion based on BOTH statistical significance, and practical significance, defined as having a standardized effect size >.1 (Pr>|t| must still be smaller than 0.25)
# Data:                   Ridership data includes average normal (school) weekday ridership 
#                         Train, Tram ridership, averaged for 2018, by Victorian Department of Transport. 
#                         Bus ridership, averaged for 4 months from August - November 2018, provided by Chris Loader from the bus planning team at the Victorian Department of Transport
#                         Refer to [insert doi for figshare] for ontology and reference for built environment.
#                         Copyright statement: This script is the product of Laura Aston

#Set working directory
setwd("C:/Users/lkast1/Google Drive/PhD/2.Analysis/2. Empirical Analysis/BE-TR_Multi Country Samples/Melbourne/Melb.All.Stops/Melb.AllStops.Repo/Data")

library(dplyr)# mutate function
library(car)#VIF function
library(Hmisc)#rcorr function
library(lm.beta)

options(scipen=999)

AllStops_Data<- read.csv("BE-TR_AllStops_data.csv", header=TRUE, sep=",")

row.names(AllStops_Data) <- AllStops_Data[,c(2)]

BusSample.400<-AllStops_Data[which (AllStops_Data$Mode=='bus'
                                    & AllStops_Data$Target.break=='400'),]
TramSample.600<-AllStops_Data[which (AllStops_Data$Mode=='tram'
                                     & AllStops_Data$Target.break=='600'),]
TrainSample.800<-AllStops_Data[which (AllStops_Data$Mode=='train'
                                      & AllStops_Data$Target.break=='800'),]

#Bus rd. 1
#Step 4 maximally adjusted model
Melb.bus.LM.1<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount  + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400))
Melb.bus.LM.1<-lm.beta(Melb.bus.LM.1)
summary(Melb.bus.LM.1)
capture.output(summary(Melb.bus.LM.1), file = "bus.MA.v2.txt")

#1.2 remove Parking
Melb.bus.LM.1.2<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400))
Melb.bus.LM.1.2<-lm.beta(Melb.bus.LM.1.2)
summary(Melb.bus.LM.1.2)

#1.3 remove AC Dist
Melb.bus.LM.1.3<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400))
Melb.bus.LM.1.3<-lm.beta(Melb.bus.LM.1.3)
summary(Melb.bus.LM.1.3)

#1.4 remove Housing Div
Melb.bus.LM.1.4<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400))
Melb.bus.LM.1.4<-lm.beta(Melb.bus.LM.1.4)
summary(Melb.bus.LM.1.4)

#1.5 remove Housing Div
Melb.bus.LM.1.5<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400))
Melb.bus.LM.1.5<-lm.beta(Melb.bus.LM.1.5)
summary(Melb.bus.LM.1.5)

par(mfrow=c(2,2))
plot(Melb.bus.LM.1.5)
#1498-bus Extreme outlier

#remove 866 and 1483
which(rownames(BusSample.400) == "1498-bus") #9474
BusSample.400.rd2 <- BusSample.400[-c(9474),]

#Rd 2
#Step 4 maximally adjusted model
Melb.bus.LM.2.1<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount  + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400.rd2))
Melb.bus.LM.2.1<-lm.beta(Melb.bus.LM.2.1)
summary(Melb.bus.LM.2.1)
capture.output(summary(Melb.bus.LM.2.1), file = "bus.MA.v2.txt")

#2.2 remove Parking
Melb.bus.LM.2.2<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400.rd2))
Melb.bus.LM.2.2<-lm.beta(Melb.bus.LM.2.2)
summary(Melb.bus.LM.2.2)

#2.3 remove Parking
Melb.bus.LM.2.3<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400.rd2))
Melb.bus.LM.2.3<-lm.beta(Melb.bus.LM.2.3)
summary(Melb.bus.LM.2.3)

#2.4 remove CBDDist
Melb.bus.LM.2.4<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X17_ACDist + X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400.rd2))
Melb.bus.LM.2.4<-lm.beta(Melb.bus.LM.2.4)
summary(Melb.bus.LM.2.4)

#2.4 remove ACDist
Melb.bus.LM.2.5<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400.rd2))
Melb.bus.LM.2.5<-lm.beta(Melb.bus.LM.2.5)
summary(Melb.bus.LM.2.5)

#2.6 remove O_tram
Melb.bus.LM.2.6<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400.rd2))
Melb.bus.LM.2.6<-lm.beta(Melb.bus.LM.2.6)
summary(Melb.bus.LM.2.6)

#2.7 remove O_tram
Melb.bus.LM.2.7<-(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X11_IntDensity + X12_CycleConnect + X13_DestScore + X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X26_O_Train_LOS + X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =BusSample.400.rd2))
Melb.bus.LM.2.7<-lm.beta(Melb.bus.LM.2.7)
summary(Melb.bus.LM.2.7)

plot(Melb.bus.LM.2.7)
capture.output(summary(Melb.bus.LM.2.7), file = "busCensus.PM.v2.txt")


Melb.bus.LM.2.7.VIF<- vif(Melb.bus.LM.2.7)
capture.output(Melb.bus.LM.2.7.VIF, file = "Melb.busCensus.LM.v2.VIF.txt")
Melb.bus.LM.2.7.VIF
#See if removal of outliers: #1416-bus, 1144-bus and 1131-bus changes results

which(rownames(BusSample.400.rd2) == "1416-bus") #8675
which(rownames(BusSample.400.rd2) == "1144-bus") #7170
which(rownames(BusSample.400.rd2) == "1131-bus") #9175
BusSample.400.rd3 <- BusSample.400.rd2[-c(8675, 7170, 9175),]

#when running with these outliers removed:#variable composition unchanged; fits slightly better. Revert to version with the other outliers in the model since no change to the significant variables

Melb.bus.LM.3.7
summary(Melb.bus.LM.3.7)
plot(Melb.bus.LM.3.7)


#Tram MA
#Step 4 maximally adjusted model
Melb.tram.LM.1.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X17_ACDist + X19._FTZ+ X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS, data =TramSample.600)
Melb.tram.LM.1.1<-lm.beta(Melb.tram.LM.1.1)
summary(Melb.tram.LM.1.1)
capture.output(summary(Melb.tram.LM.1), file = "tram.MA.txt")

#remove lnpop
Melb.tram.LM.1.2<-lm(X1ln.ln_Patronage ~  X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X17_ACDist + X19._FTZ+ X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS, data =TramSample.600)
Melb.tram.LM.1.2<-lm.beta(Melb.tram.LM.1.2)
summary(Melb.tram.LM.1.2)

#remove PropFTE
Melb.tram.LM.1.3<-lm(X1ln.ln_Patronage ~  X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X17_ACDist + X19._FTZ+ X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS	+ X29_Censored_MeanSize + X31_PropOS, data =TramSample.600)
Melb.tram.LM.1.3<-lm.beta(Melb.tram.LM.1.3)
summary(Melb.tram.LM.1.3)

#remove Parkiteer
Melb.tram.LM.1.4<-lm(X1ln.ln_Patronage ~  X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X17_ACDist + X19._FTZ+ X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS	+ X29_Censored_MeanSize + X31_PropOS, data =TramSample.600)
Melb.tram.LM.1.4<-lm.beta(Melb.tram.LM.1.4)
summary(Melb.tram.LM.1.4)

#remove ACDist
Melb.tram.LM.1.5<-lm(X1ln.ln_Patronage ~  X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X19._FTZ+ X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS	+ X29_Censored_MeanSize + X31_PropOS, data =TramSample.600)
Melb.tram.LM.1.5<-lm.beta(Melb.tram.LM.1.5)
summary(Melb.tram.LM.1.5)

#remove Housingdiv
Melb.tram.LM.1.6<-lm(X1ln.ln_Patronage ~  X6_PropComm + X8_Balance + X9_LUEntropy + X11_IntDensity + X12_CycleConnect + X13_DestScore + X19._FTZ+ X18_ACCount  + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS	+ X29_Censored_MeanSize + X31_PropOS, data =TramSample.600)
Melb.tram.LM.1.6<-lm.beta(Melb.tram.LM.1.6)
summary(Melb.tram.LM.1.6)

plot(Melb.tram.LM.1.6)

capture.output(summary(Melb.tram.LM.1.6), file = "tramCensus.PM.v2.txt")
Melb.tram.LM.1.6.VIF<- vif(Melb.tram.LM.1.6)
capture.output(Melb.tram.LM.1.6.VIF, file = "Melb.tramCensus.LM.v2.VIF.txt")
Melb.tram.LM.1.6.VIF

#step 3 check for multicolinearity
Train.LM.VIF<-vif(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS +  X27_O_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800))
#removed  overlapping (train) level of service to get rid of singularity
# overlapping (all) removed due to high VIF
#removed prop urban and PropBach, high VIF (again, Pop density higher than propUrban, but aim to preserve pop density in favour of propr urban --> more descriptive. This approach has been taken consistently across all samples)
Train.LM.VIF

#Step 4 maximally adjusted model
Melb.train.LM.1.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800)
Melb.train.LM.1.1<- lm.beta(Melb.train.LM.1.1)
summary(Melb.train.LM.1.1)
capture.output(summary(Melb.train.LM.1.1), file = "train.MA.txt")

#removeLUEntropy
Melb.train.LM.1.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800)
Melb.train.LM.1.2<- lm.beta(Melb.train.LM.1.2)
summary(Melb.train.LM.1.2)


#remove IntDensity
Melb.train.LM.1.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800)
Melb.train.LM.1.3<- lm.beta(Melb.train.LM.1.3)
summary(Melb.train.LM.1.3)

#remove PropOS
Melb.train.LM.1.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize, data =TrainSample.800)
Melb.train.LM.1.4<- lm.beta(Melb.train.LM.1.4)
summary(Melb.train.LM.1.4)

#remove PropComm
Melb.train.LM.1.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize, data =TrainSample.800)
Melb.train.LM.1.5<- lm.beta(Melb.train.LM.1.5)
summary(Melb.train.LM.1.5)

#remove DestScore
Melb.train.LM.1.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize, data =TrainSample.800)
Melb.train.LM.1.6<- lm.beta(Melb.train.LM.1.6)
summary(Melb.train.LM.1.6)

#remove MeanSize
Melb.train.LM.1.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data =TrainSample.800)
Melb.train.LM.1.7<- lm.beta(Melb.train.LM.1.7)
summary(Melb.train.LM.1.7)

plot(Melb.train.LM.1.7)
#30961, 30955 extreme outliers
which(rownames(TrainSample.800) == "30961") #219
which(rownames(TrainSample.800) == "30955") #213

TrainSample.800.rd2 <- TrainSample.800[-c(219, 213),]

#Step 4 maximally adjusted model
Melb.train.LM.2.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800.rd2)
Melb.train.LM.2.1<-lm.beta(Melb.train.LM.2.1)
summary(Melb.train.LM.2.1)
capture.output(summary(Melb.tram.LM.1), file = "tram.MA.txt")

#removepropComm
Melb.train.LM.2.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800.rd2)
Melb.train.LM.2.2<-lm.beta(Melb.train.LM.2.2)
summary(Melb.train.LM.2.2)

#removeIntDensity
Melb.train.LM.2.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800.rd2)
Melb.train.LM.2.3<-lm.beta(Melb.train.LM.2.3)
summary(Melb.train.LM.2.3)

#removePropOS
Melb.train.LM.2.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize, data =TrainSample.800.rd2)
Melb.train.LM.2.4<-lm.beta(Melb.train.LM.2.4)
summary(Melb.train.LM.2.4)

#removeLUEntropy
Melb.train.LM.2.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize, data =TrainSample.800.rd2)
Melb.train.LM.2.5<-lm.beta(Melb.train.LM.2.5)
summary(Melb.train.LM.2.5)

#removeMeanSize
Melb.train.LM.2.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data =TrainSample.800.rd2)
Melb.train.LM.2.6<-lm.beta(Melb.train.LM.2.6)
summary(Melb.train.LM.2.6)

#removeO_Tram
Melb.train.LM.2.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd2)
Melb.train.LM.2.7<-lm.beta(Melb.train.LM.2.7)
summary(Melb.train.LM.2.7)

#remove DestScore
Melb.train.LM.2.8<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd2)
Melb.train.LM.2.8<-lm.beta(Melb.train.LM.2.8)
summary(Melb.train.LM.2.8)

plot(Melb.train.LM.2.8)

#Values appear to be affecting assumptions although not extreme outliers. Check if removing them changes results: 30745, 30773, 30960,
which(rownames(TrainSample.800.rd2) == "30745") #3
which(rownames(TrainSample.800.rd2) == "30773") #31
which(rownames(TrainSample.800.rd2) == "30960") #217


TrainSample.800.rd3 <- TrainSample.800.rd2[-c(3, 31, 217),]

#rd 3 MA
Melb.train.LM.3.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800.rd3)
Melb.train.LM.3.1<-lm.beta(Melb.train.LM.3.1)
summary(Melb.train.LM.3.1)
capture.output(summary(Melb.tram.LM.1), file = "tram.MA.txt")

#remove MeanSize
Melb.train.LM.3.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X31_PropOS, data =TrainSample.800.rd3)
Melb.train.LM.3.2<-lm.beta(Melb.train.LM.3.2)
summary(Melb.train.LM.3.2)

#remove DestScore
Melb.train.LM.3.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity +X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X31_PropOS, data =TrainSample.800.rd3)
Melb.train.LM.3.3<-lm.beta(Melb.train.LM.3.3)
summary(Melb.train.LM.3.3)

#remove PropComm
Melb.train.LM.3.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity +X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X31_PropOS, data =TrainSample.800.rd3)
Melb.train.LM.3.4<-lm.beta(Melb.train.LM.3.4)
summary(Melb.train.LM.3.4)

#remove PropOS
Melb.train.LM.3.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity +X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data =TrainSample.800.rd3)
Melb.train.LM.3.5<-lm.beta(Melb.train.LM.3.5)
summary(Melb.train.LM.3.5)

#remove IntDensity
Melb.train.LM.3.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv +X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data =TrainSample.800.rd3)
Melb.train.LM.3.6<-lm.beta(Melb.train.LM.3.6)
summary(Melb.train.LM.3.6)

#remove LUEntropy
Melb.train.LM.3.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv +X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data =TrainSample.800.rd3)
Melb.train.LM.3.7<-lm.beta(Melb.train.LM.3.7)
summary(Melb.train.LM.3.7)

#remove O_Tram
Melb.train.LM.3.8<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv +X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd3)
Melb.train.LM.3.8<-lm.beta(Melb.train.LM.3.8)
summary(Melb.train.LM.3.8)

#remove ACCount
Melb.train.LM.3.9<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv +X15_Parkiteer + X16_CBDDist + X17_ACDist+ X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd3)
Melb.train.LM.3.9<-lm.beta(Melb.train.LM.3.9)
summary(Melb.train.LM.3.9)

#remove Balance
Melb.train.LM.3.10<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X10_HousingDiv +X15_Parkiteer + X16_CBDDist + X17_ACDist+ X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd3)
Melb.train.LM.3.10<-lm.beta(Melb.train.LM.3.10)
summary(Melb.train.LM.3.10)
capture.output(summary(Melb.train.LM.1.7), file = "trainCensus.PM.v2.txt")

#remove_lnPop
Melb.train.LM.3.11<-lm(X1ln.ln_Patronage ~ X10_HousingDiv +X15_Parkiteer + X16_CBDDist + X17_ACDist+ X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd3)
Melb.train.LM.3.11<-lm.beta(Melb.train.LM.3.11)
summary(Melb.train.LM.3.11)

plot(Melb.train.LM.3.11)

#affecting assumptions: 30755, 30746, 30793

which(rownames(TrainSample.800.rd3) == "30755") #12
which(rownames(TrainSample.800.rd3) == "30746") #3
which(rownames(TrainSample.800.rd3) == "30793") #49


TrainSample.800.rd4 <- TrainSample.800.rd3[-c(12, 3, 49),]

#rd 4 MA
Melb.train.LM.4.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800.rd4)
Melb.train.LM.4.1<-lm.beta(Melb.train.LM.4.1)
summary(Melb.train.LM.4.1)
capture.output(summary(Melb.train.LM.4.1), file = "tram.MA.txt")

#4.2 remove Prop comm
Melb.train.LM.4.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800.rd4)
Melb.train.LM.4.2<-lm.beta(Melb.train.LM.4.2)
summary(Melb.train.LM.4.2)

#4.3 remove PropOS
Melb.train.LM.4.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize, data =TrainSample.800.rd4)
Melb.train.LM.4.3<-lm.beta(Melb.train.LM.4.3)
summary(Melb.train.LM.4.3)

#4.4 remove PropOS
Melb.train.LM.4.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data =TrainSample.800.rd4)
Melb.train.LM.4.4<-lm.beta(Melb.train.LM.4.4)
summary(Melb.train.LM.4.4)

#4.5 remove PropOS
Melb.train.LM.4.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data =TrainSample.800.rd4)
Melb.train.LM.4.5<-lm.beta(Melb.train.LM.4.5)
summary(Melb.train.LM.4.5)

#4.6 remove Tram_LOS
Melb.train.LM.4.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd4)
Melb.train.LM.4.6<-lm.beta(Melb.train.LM.4.6)
summary(Melb.train.LM.4.6)

#4.7 remove AC_Count
Melb.train.LM.4.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd4)
Melb.train.LM.4.7<-lm.beta(Melb.train.LM.4.7)
summary(Melb.train.LM.4.7)

#4.8 remove Balance
Melb.train.LM.4.8<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd4)
Melb.train.LM.4.8<-lm.beta(Melb.train.LM.4.8)
summary(Melb.train.LM.4.8)

plot(Melb.train.LM.4.8)

#outliers affecting assumptions are: 30743, 30749, 30748
which(rownames(TrainSample.800.rd4) == "30743") #1
which(rownames(TrainSample.800.rd4) == "30749") #5
which(rownames(TrainSample.800.rd4) == "30748") #4


TrainSample.800.rd5 <- TrainSample.800.rd4[-c(1, 5, 4),]

#rd 5 MA
Melb.train.LM.5.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800.rd5)
Melb.train.LM.5.1<-lm.beta(Melb.train.LM.5.1)
summary(Melb.train.LM.5.1) 
#explanatory power has dropped away. #if predictors haven't changed revert to model 4
capture.output(summary(Melb.train.LM.5.1), file = "tram.MA.txt")

#5.2 remove Dest Score
Melb.train.LM.5.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800.rd5)
Melb.train.LM.5.2<-lm.beta(Melb.train.LM.5.2)
summary(Melb.train.LM.5.2) 

#5.3 remove PropComm
Melb.train.LM.5.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800.rd5)
Melb.train.LM.5.3<-lm.beta(Melb.train.LM.5.3)
summary(Melb.train.LM.5.3) 

#5.4 Remove PropOS
Melb.train.LM.5.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize, data =TrainSample.800.rd5)
Melb.train.LM.5.4<-lm.beta(Melb.train.LM.5.4)
summary(Melb.train.LM.5.4)

#5.5 Remove MeanSize
Melb.train.LM.5.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data =TrainSample.800.rd5)
Melb.train.LM.5.5<-lm.beta(Melb.train.LM.5.5)
summary(Melb.train.LM.5.5)

#5.6 Remove O_Tram
Melb.train.LM.5.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd5)
Melb.train.LM.5.6<-lm.beta(Melb.train.LM.5.6)
summary(Melb.train.LM.5.6)

#5.7 Remove Balance
Melb.train.LM.5.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd5)
Melb.train.LM.5.7<-lm.beta(Melb.train.LM.5.7)
summary(Melb.train.LM.5.7)

#5.8 ACCount
Melb.train.LM.5.8<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist +X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd5)
Melb.train.LM.5.8<-lm.beta(Melb.train.LM.5.8)
summary(Melb.train.LM.5.8)
summary(Melb.train.LM.4.8)

#slightly different make up of sig variables. Accept model 5. 
plot(Melb.train.LM.5.8)
#Also, now the model is much closer to adhering to normality; however now the outliers are more pronounced. Try removing and again compare the make up of variables for any significant changes. 
#outliers affecting assumptions are: 30747, 30744, 30752
which(rownames(TrainSample.800.rd5) == "30747") #2
which(rownames(TrainSample.800.rd5) == "30744") #1
which(rownames(TrainSample.800.rd5) == "30752") #5


TrainSample.800.rd6 <- TrainSample.800.rd5[-c(1, 2, 5),]

#rd 6 MA
Melb.train.LM.6.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize + X31_PropOS, data =TrainSample.800.rd6)
Melb.train.LM.6.1<-lm.beta(Melb.train.LM.6.1)
summary(Melb.train.LM.6.1) 
#explanatory power drtoped away only slightly. 
capture.output(summary(Melb.train.LM.6.1), file = "tram.MA.v2.txt")

#6.2 Remove propOS
Melb.train.LM.6.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize, data =TrainSample.800.rd6)
Melb.train.LM.6.2<-lm.beta(Melb.train.LM.6.2)
summary(Melb.train.LM.6.2) 

#6.3 Remove PropComm
Melb.train.LM.6.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize, data =TrainSample.800.rd6)
Melb.train.LM.6.3<-lm.beta(Melb.train.LM.6.3)
summary(Melb.train.LM.6.3) 

#6.4 Remove DestScore
Melb.train.LM.6.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X29_Censored_MeanSize, data =TrainSample.800.rd6)
Melb.train.LM.6.4<-lm.beta(Melb.train.LM.6.4)
summary(Melb.train.LM.6.4) 

#6.5 Remove MeanSize
Melb.train.LM.6.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data =TrainSample.800.rd6)
Melb.train.LM.6.5<-lm.beta(Melb.train.LM.6.5)
summary(Melb.train.LM.6.5) 

#6.6 Remove CBDDist
Melb.train.LM.6.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data =TrainSample.800.rd6)
Melb.train.LM.6.6<-lm.beta(Melb.train.LM.6.6)
summary(Melb.train.LM.6.6)

#6.7 Remove O_Tram
Melb.train.LM.6.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS, data =TrainSample.800.rd6)
Melb.train.LM.6.7<-lm.beta(Melb.train.LM.6.7)
summary(Melb.train.LM.6.7)

plot(Melb.train.LM.6.7)
#model adheres to assumptions, no outliers. 

capture.output(summary(Melb.train.LM.6.7), file = "trainCensus.PM.v2.txt")


Melb.train.LM.6.7.VIF<- vif(Melb.train.LM.6.7)
capture.output(Melb.train.LM.6.7.VIF, file = "Melb.trainCensus.LM.v2.VIF.txt")
Melb.train.LM.6.7.VIF

# Title:                  Melbourne BE-TR Stratifieded stops final models stepwise 
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   This script performs multivariate multiple linear regression of built environment and sociodemographic variables on transit ridership for the stratified sample of tram and bus locations in Melbourne. Refer to 3_MelbClusterLM for steps leading to the final models. The urpose of this script is ot exert more control over the stepwise removal of the variables, instead of using R's inbuilt regb function which finds a parsimonious model based on optimisation of AIC only. Instead, this model removes variables in a stepwisde fashion based on BOTH statistical significance, and practical significance, defined as having a standardized effect size >.1 (Pr>|t| must still be smaller than 0.25)
# Data:                   Ridership data includes average normal (school) weekday ridership 
#                         Train, Tram ridership, averaged for 2018, by Victorian Department of Transport. 
#                         Bus ridership, averaged for 4 months from August - November 2018, provided by Chris Loader from the bus planning team at the Victorian Department of Transport
#                         Refer to [insert doi for figshare] for ontology and reference for built environment.
#                         Copyright statement: This script is the product of Laura Aston

install.packages("tidyverse")  # data manipulation
library(tidyverse)  # data manipulation
library(gridExtra)
library(dplyr)# mutate function
library(car)#VIF function
library(Hmisc)#rcorr function
library(lm.beta)

options(max.print= 1000000)

setwd("C:/Users/lkast1/Google Drive/PhD/2.Analysis/2. Empirical Analysis/BE-TR_Multi Country Samples/Melbourne/Melb.All.Stops/Melb.AllStops.Repo/Data")

#Refer to sampling script
Melb_Data<- read.csv("BE-TR_AllStops_data.csv", header=TRUE, sep=",")
row.names(Melb_Data) <- Melb_Data[,c(2)]

Stratifiedsample.bus.400<- Melb_Data[which (Melb_Data$Mode=='bus'
                                                  & Melb_Data$Sample=='Yes'),]
row.names(Stratifiedsample.bus.400) <- Stratifiedsample.bus.400[,c(2)]
Stratifiedsample.tram.600<- Melb_Data[which (Melb_Data$Mode=='tram'
                                          & Melb_Data$Sample=='Yes'),]
row.names(Stratifiedsample.tram.600) <- Stratifiedsample.tram.600[,c(2)]
Stratifiedsample.train.800<- Melb_Data[which (Melb_Data$Mode=='train'
                                           &Melb_Data$Sample=='Yes'),]
row.names(Stratifiedsample.train.800) <- Stratifiedsample.train.800[,c(2)]

#Col headers: X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X27_O_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach

#Bus
options(max.print=1000000)

setwd("C:/Users/lkast1/Google Drive/PhD/2.Analysis/2. Empirical Analysis/BE-TR_Multi Country Samples/Melbourne/Melb.All.Stops/Melb.AllStops.Repo/Regression outputs/Stratified")
options(scipen = 999)
#Step 4 maximally adjusted model
Melb.busStratified.LM.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.1<-lm.beta(Melb.busStratified.LM.1)
summary(Melb.busStratified.LM.1)
capture.output(summary(Melb.busStratified.LM.1), file = "busStratified.MA.txt")

#2 remove O_train
Melb.busStratified.LM.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS +  X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.2<-lm.beta(Melb.busStratified.LM.2)
summary(Melb.busStratified.LM.2)

#3 remove O_tram
Melb.busStratified.LM.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.3<-lm.beta(Melb.busStratified.LM.3)
summary(Melb.busStratified.LM.3)

#4 remove DestScore
Melb.busStratified.LM.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X16_CBDDist + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.4<-lm.beta(Melb.busStratified.LM.4)
summary(Melb.busStratified.LM.4)

#5 remove AC Dist
Melb.busStratified.LM.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.5<-lm.beta(Melb.busStratified.LM.5)
summary(Melb.busStratified.LM.5)

#6 remove CBDDist
Melb.busStratified.LM.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.6<-lm.beta(Melb.busStratified.LM.6)
summary(Melb.busStratified.LM.6)

#7 remove EmpAccess
Melb.busStratified.LM.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.7<-lm.beta(Melb.busStratified.LM.7)
summary(Melb.busStratified.LM.7)

#8 remove Meansize
Melb.busStratified.LM.8<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS + X28_Censored_PropFTE+ X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.8<-lm.beta(Melb.busStratified.LM.8)
summary(Melb.busStratified.LM.8)

#9 remove AC COunt
Melb.busStratified.LM.9<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS + X28_Censored_PropFTE+ X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.9<-lm.beta(Melb.busStratified.LM.9)
summary(Melb.busStratified.LM.9)

#10 remove Int Density
Melb.busStratified.LM.10<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X12_CycleConnect + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS + X28_Censored_PropFTE+ X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.10<-lm.beta(Melb.busStratified.LM.10)
summary(Melb.busStratified.LM.10)

#11 remove Cycle Connectivity
Melb.busStratified.LM.11<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS + X28_Censored_PropFTE+ X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.11<-lm.beta(Melb.busStratified.LM.11)
summary(Melb.busStratified.LM.11)

#12 remove Prop OS
Melb.busStratified.LM.12<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS + X28_Censored_PropFTE + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.12<-lm.beta(Melb.busStratified.LM.12)
summary(Melb.busStratified.LM.12)
#All var have pr <0.25

#13 remove PropFTE
Melb.busStratified.LM.13<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS + X32_PropBach, data =Stratifiedsample.bus.400)

Melb.busStratified.LM.13<-lm.beta(Melb.busStratified.LM.13)
summary(Melb.busStratified.LM.13)

#NO change in explanatory factors; keep model after removal of PropOS (#12)


#'function for backward (starts with maximal model)
#'Melb.busStratified.LM.1.regb<-step(Melb.busStratified.LM.1,direction = "backward",trace = 0) #don't print steps
#'summary(Melb.busStratified.LM.1.regb)
#'Melb.busStratified.LM.1.regb<-lm.beta(Melb.busStratified.LM.1.regb)
capture.output(summary(Melb.busStratified.LM.12), file = "busStratified.PM.v2.txt")

#diagnostics
par(mfrow=c(2,2))
plot(Melb.busStratified.LM.12)
#866-bus approaching cooks distance, but generally fits assumptions
#1483 appears to be an outlier

#remove 866 and 1483
which(rownames(Stratifiedsample.bus.400) == "866-bus") #156
which(rownames(Stratifiedsample.bus.400) == "1483-bus") #194
Stratifiedsample.bus.400.rd2 <- Stratifiedsample.bus.400[-c(156,194),]

#Rd2 Step 4 maximally adjusted model
Melb.busStratified.LM.2.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.1<-lm.beta(Melb.busStratified.LM.2.1)
summary(Melb.busStratified.LM.2.1)
capture.output(summary(Melb.busStratified.LM.2.1), file = "busStratified.MA.rd2.txt")

#Rd2.2 Remove AC Dist
Melb.busStratified.LM.2.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.2<-lm.beta(Melb.busStratified.LM.2.2)
summary(Melb.busStratified.LM.2.2)

#Rd2.2 Remove O_tram
Melb.busStratified.LM.2.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS +  X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.3<-lm.beta(Melb.busStratified.LM.2.3)
summary(Melb.busStratified.LM.2.3)

#Rd2.3 Remove O_tram
Melb.busStratified.LM.2.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS +  X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.3<-lm.beta(Melb.busStratified.LM.2.3)
summary(Melb.busStratified.LM.2.3)

#Rd2.4 Remove mean size
Melb.busStratified.LM.2.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS +  X26_O_Train_LOS + X28_Censored_PropFTE	+ X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.4<-lm.beta(Melb.busStratified.LM.2.4)
summary(Melb.busStratified.LM.2.4)

#Rd2.5 Remove destscore
Melb.busStratified.LM.2.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS +  X26_O_Train_LOS + X28_Censored_PropFTE	+ X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.5<-lm.beta(Melb.busStratified.LM.2.5)
summary(Melb.busStratified.LM.2.5)

#Rd2.6 Remove empaccess
Melb.busStratified.LM.2.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS +  X26_O_Train_LOS + X28_Censored_PropFTE	+ X31_PropOS + X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.6<-lm.beta(Melb.busStratified.LM.2.6)
summary(Melb.busStratified.LM.2.6)

#Rd2.7 Remove PropOS
Melb.busStratified.LM.2.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS +  X26_O_Train_LOS + X28_Censored_PropFTE	+ X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.7<-lm.beta(Melb.busStratified.LM.2.7)
summary(Melb.busStratified.LM.2.7)

#Rd2.8 Remove CBDDist
Melb.busStratified.LM.2.8<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS +  X26_O_Train_LOS + X28_Censored_PropFTE	+ X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.8<-lm.beta(Melb.busStratified.LM.2.8)
summary(Melb.busStratified.LM.2.8)

#Rd2.9 Remove CBDDist
Melb.busStratified.LM.2.9<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X12_CycleConnect + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS +  X26_O_Train_LOS + X28_Censored_PropFTE	+ X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.9<-lm.beta(Melb.busStratified.LM.2.9)
summary(Melb.busStratified.LM.2.9)

#Rd2.10 Remove CBDDist
Melb.busStratified.LM.2.10<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS +  X26_O_Train_LOS + X28_Censored_PropFTE	+ X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.10<-lm.beta(Melb.busStratified.LM.2.10)
summary(Melb.busStratified.LM.2.10)

#Rd2.11 Remove O_Train
Melb.busStratified.LM.2.11<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X23_C_LOS + X28_Censored_PropFTE	+ X32_PropBach, data =Stratifiedsample.bus.400.rd2)

Melb.busStratified.LM.2.11<-lm.beta(Melb.busStratified.LM.2.11)
summary(Melb.busStratified.LM.2.11)

#diagnostics
par(mfrow=c(2,2))
plot(Melb.busStratified.LM.2.11)
#Some moderate outliers affecting assumptions but not obvious which are the consistent ones.
#retain as is

#Check multicollineairty
Bus.Stratified.PM.v2.VIF<- vif(Melb.busStratified.LM.2.11)
capture.output(Bus.Stratified.PM.v2.VIF, file = "busStratified.PM.v2.VIF.txt")
Bus.Stratified.PM.v2.VIF

capture.output(summary(Melb.busStratified.LM.2.11), file = "busStratified.PM.v2.txt")

#'Tram
#'VIF for emp access and CBD dist may be causing unexpected result for CBD dist. remove. 
#MA tram
Melb.tramStratified.LM.1.1<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop + X6_PropComm + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X18_ACCount + X19._FTZ + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X32_PropBach, data =Stratifiedsample.tram.600)
Melb.tramStratified.LM.1.1<-lm.beta(Melb.tramStratified.LM.1.1)
summary(Melb.tramStratified.LM.1.1)

capture.output(summary(Melb.tramStratified.LM.1.1), file = "tramStratified.MA_v2.txt")

#2 remove AC Count
Melb.tramStratified.LM.1.2<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop + X6_PropComm + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X19._FTZ + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X32_PropBach, data =Stratifiedsample.tram.600)
Melb.tramStratified.LM.1.2<-lm.beta(Melb.tramStratified.LM.1.2)
summary(Melb.tramStratified.LM.1.2)

#3 remove PropBach
Melb.tramStratified.LM.1.3<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop + X6_PropComm + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X19._FTZ + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize, data =Stratifiedsample.tram.600)
Melb.tramStratified.LM.1.3<-lm.beta(Melb.tramStratified.LM.1.3)
summary(Melb.tramStratified.LM.1.3)

#4 remove HousingDiv
Melb.tramStratified.LM.1.4<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop + X6_PropComm + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X19._FTZ + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize, data =Stratifiedsample.tram.600)
Melb.tramStratified.LM.1.4<-lm.beta(Melb.tramStratified.LM.1.4)
summary(Melb.tramStratified.LM.1.4)

#5 remove Int Density
Melb.tramStratified.LM.1.5<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop + X6_PropComm + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X19._FTZ + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize, data =Stratifiedsample.tram.600)
Melb.tramStratified.LM.1.5<-lm.beta(Melb.tramStratified.LM.1.5)
summary(Melb.tramStratified.LM.1.5)

#6 remove FTS
Melb.tramStratified.LM.1.6<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop + X6_PropComm + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize, data =Stratifiedsample.tram.600)
Melb.tramStratified.LM.1.6<-lm.beta(Melb.tramStratified.LM.1.6)
summary(Melb.tramStratified.LM.1.6)


#diagnostics
par(mfrow=c(2,2))
plot(Melb.tramStratified.LM.1.6)
#278, #2213 is influential outlier
#1913-tram is outside Cook's distance
which(rownames(Stratifiedsample.tram.600) == "278-tram") #81
which(rownames(Stratifiedsample.tram.600) == "2213-tram") #8
which(rownames(Stratifiedsample.tram.600) == "1913-tram") #62


Stratifiedsample.tram.600.rd2 <- Stratifiedsample.tram.600[-c(81, 8, 62),]

#MA tram rd 2
Melb.tramStratified.LM.2.1<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop+ X6_PropComm + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X18_ACCount + X19._FTZ +X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X32_PropBach, data =Stratifiedsample.tram.600.rd2)
Melb.tramStratified.LM.2.1<-lm.beta(Melb.tramStratified.LM.2.1)
summary(Melb.tramStratified.LM.2.1)
Tram.Stratified.LM.2.MA.VIF<- vif(Melb.tramStratified.LM.2.1)
Tram.Stratified.LM.2.MA.VIF
capture.output(summary(Melb.tramStratified.LM.2.1), file = "tramStratified.MA_v2.txt")

#2 remove AC Count
Melb.tramStratified.LM.2.2<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop+ X6_PropComm + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X19._FTZ +X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X32_PropBach, data =Stratifiedsample.tram.600.rd2)
Melb.tramStratified.LM.2.2<-lm.beta(Melb.tramStratified.LM.2.2)
summary(Melb.tramStratified.LM.2.2)

#3 remove HousingDiv
Melb.tramStratified.LM.2.3<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop+ X6_PropComm + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X19._FTZ +X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X32_PropBach, data =Stratifiedsample.tram.600.rd2)
Melb.tramStratified.LM.2.3<-lm.beta(Melb.tramStratified.LM.2.3)
summary(Melb.tramStratified.LM.2.3)

#4 remove PropBach
Melb.tramStratified.LM.2.4<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop+ X6_PropComm + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X19._FTZ +X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize, data =Stratifiedsample.tram.600.rd2)
Melb.tramStratified.LM.2.4<-lm.beta(Melb.tramStratified.LM.2.4)
summary(Melb.tramStratified.LM.2.4)

#5 remove FTZ
Melb.tramStratified.LM.2.5<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop+ X6_PropComm + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize, data =Stratifiedsample.tram.600.rd2)
Melb.tramStratified.LM.2.5<-lm.beta(Melb.tramStratified.LM.2.5)
summary(Melb.tramStratified.LM.2.5)

#6 remove mean size
Melb.tramStratified.LM.2.6<-lm(X1ln.ln_Patronage ~  X3ln.ln_Pop+ X6_PropComm + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE, data =Stratifiedsample.tram.600.rd2)
Melb.tramStratified.LM.2.6<-lm.beta(Melb.tramStratified.LM.2.6)
summary(Melb.tramStratified.LM.2.6)

#diagnostics
par(mfrow=c(2,2))
plot(Melb.tramStratified.LM.2.6)
#Fits assumption

capture.output(summary(Melb.tramStratified.LM.2.6), file = "tramStratified.PM.v2.txt")

#Check multicollinearity
Tram.Stratified.LM.2.VIF<- vif(Melb.tramStratified.LM.2.6)
capture.output(Tram.Stratified.LM.2.VIF, file = "tramStratified.PM.rd.2.v2.VIF.txt")
Tram.Stratified.LM.2.VIF

#CBD Dist and emp access may be multicollinear. Try removing EmpAccess
summary(Melb.tramStratified.LM.3.1.6)
#try removing CBD DIst
summary(Melb.tramStratified.LM.3.2.4)

#Make up of sig variables is the same --> conclude not having an impact on result; retain both CBD Dist and emp access since exlpanatory power is greater. 


#Train
#maxmially adjusted model
Melb.trainStratified.LM.1.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.1.1<-lm.beta(Melb.trainStratified.LM.1.1)
summary(Melb.trainStratified.LM.1.1)
capture.output(summary(Melb.trainStratified.LM.1.1), file = "trainStratified.MA.txt")

#2 remove Int Density
Melb.trainStratified.LM.1.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.1.2<-lm.beta(Melb.trainStratified.LM.1.2)
summary(Melb.trainStratified.LM.1.2)

#3 remove LUEntropy
Melb.trainStratified.LM.1.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.1.3<-lm.beta(Melb.trainStratified.LM.1.3)
summary(Melb.trainStratified.LM.1.3)

#4 remove PropComm
Melb.trainStratified.LM.1.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.1.4<-lm.beta(Melb.trainStratified.LM.1.4)
summary(Melb.trainStratified.LM.1.4)

#5 remove mean size
Melb.trainStratified.LM.1.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.1.5<-lm.beta(Melb.trainStratified.LM.1.5)
summary(Melb.trainStratified.LM.1.5)

#6 remove mean size
Melb.trainStratified.LM.1.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.1.6<-lm.beta(Melb.trainStratified.LM.1.6)
summary(Melb.trainStratified.LM.1.6)

#7 remove PropOS
Melb.trainStratified.LM.1.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.1.7<-lm.beta(Melb.trainStratified.LM.1.7)
summary(Melb.trainStratified.LM.1.7)

capture.output(summary(Melb.trainStratified.LM.1.7), file = "trainStratified.PM.v2.txt")
Melb.trainStratified.LM.1.7.VIF<- vif(Melb.trainStratified.LM.1.7)
capture.output(Melb.trainStratified.LM.1.7.VIF, file = "Melb.trainStratified.LM.v2.VIF.txt")
Melb.trainStratified.LM.1.7.VIF
#CBD Dist has high VIF score; remove


#diagnostic
plot(Melb.trainStratified.LM.1.7)
#signs of heteroskedasticity with most outlying 2, 4, 628, 609, 597
#explanatory power is significantly reduced if the outliers (2 and 4) are removed. Possibly retain model 1 but try a second iteration with 2 and 4 removed
#retain, because the outliers add significant explanatory power to the model without changing the make up of the isgnificant variables (magnitudes vary)
#' See: https://www.theanalysisfactor.com/outliers-to-drop-or-not-to-drop/

#maxmially adjusted model with CBD Dist removed
Melb.trainStratified.LM.2.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.2.1<-lm.beta(Melb.trainStratified.LM.2.1)
summary(Melb.trainStratified.LM.2.1)
capture.output(summary(Melb.trainStratified.LM.2.1), file = "trainStratified.MA.txt")

#removePropOS
Melb.trainStratified.LM.2.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.2.2<-lm.beta(Melb.trainStratified.LM.2.2)
summary(Melb.trainStratified.LM.2.2)

#remove HousingDiv
Melb.trainStratified.LM.2.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.2.3<-lm.beta(Melb.trainStratified.LM.2.3)
summary(Melb.trainStratified.LM.2.3)

#remove IntDensity
Melb.trainStratified.LM.2.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy +   X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.2.4<-lm.beta(Melb.trainStratified.LM.2.4)
summary(Melb.trainStratified.LM.2.4)

#remove LUEntropy
Melb.trainStratified.LM.2.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.2.5<-lm.beta(Melb.trainStratified.LM.2.5)
summary(Melb.trainStratified.LM.2.5)

#remove LUEntropy
Melb.trainStratified.LM.2.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.2.6<-lm.beta(Melb.trainStratified.LM.2.6)
summary(Melb.trainStratified.LM.2.6)

#remove Meansize
Melb.trainStratified.LM.2.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.2.7<-lm.beta(Melb.trainStratified.LM.2.7)
summary(Melb.trainStratified.LM.2.7)

#remove ACCount
Melb.trainStratified.LM.2.8<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X13_DestScore + X15_Parkiteer + X17_ACDist + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X32_PropBach, data = Stratifiedsample.train.800)
Melb.trainStratified.LM.2.8<-lm.beta(Melb.trainStratified.LM.2.8)
summary(Melb.trainStratified.LM.2.8)


par(mfrow=c(2,2))
plot(Melb.trainStratified.LM.2.8)
#2, 4, 628 are outliers
which(rownames(Stratifiedsample.train.800) == "2-train") #194
which(rownames(Stratifiedsample.train.800) == "4-train") #188
which(rownames(Stratifiedsample.train.800) == "628-train") #195
Stratifiedsample.train.800.rd2 <- Stratifiedsample.train.800[-c(194, 188, 195),]


#maxmially adjusted model with CBD Dist removed
#rd3
Melb.trainStratified.LM.3.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.1<-lm.beta(Melb.trainStratified.LM.3.1)
summary(Melb.trainStratified.LM.3.1)

#again, explanatory power drops away significantly with removal of these outliers. However, unavoidable as assumptions are not met this time. 
capture.output(summary(Melb.trainStratified.LM.3.1), file = "trainStratified.MA.txt")

Melb.trainStratified.LM.3.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.2<-lm.beta(Melb.trainStratified.LM.3.2)
summary(Melb.trainStratified.LM.3.2)

#removeLUEntropy
Melb.trainStratified.LM.3.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.2<-lm.beta(Melb.trainStratified.LM.3.2)
summary(Melb.trainStratified.LM.3.2)

#remove PropOS
Melb.trainStratified.LM.3.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.3<-lm.beta(Melb.trainStratified.LM.3.3)
summary(Melb.trainStratified.LM.3.3)

#remove PropOS
Melb.trainStratified.LM.3.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.3<-lm.beta(Melb.trainStratified.LM.3.3)
summary(Melb.trainStratified.LM.3.3)

#remove IntDensity
Melb.trainStratified.LM.3.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.4<-lm.beta(Melb.trainStratified.LM.3.4)
summary(Melb.trainStratified.LM.3.4)

#remove HousingDiv
Melb.trainStratified.LM.3.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.5<-lm.beta(Melb.trainStratified.LM.3.5)
summary(Melb.trainStratified.LM.3.5)

#remove PropComm
Melb.trainStratified.LM.3.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.6<-lm.beta(Melb.trainStratified.LM.3.6)
summary(Melb.trainStratified.LM.3.6)

#remove ACCount
Melb.trainStratified.LM.3.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X13_DestScore + X15_Parkiteer + X17_ACDist + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.7<-lm.beta(Melb.trainStratified.LM.3.7)
summary(Melb.trainStratified.LM.3.7)

#remove Balance
Melb.trainStratified.LM.3.8<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X13_DestScore + X15_Parkiteer + X17_ACDist + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.8<-lm.beta(Melb.trainStratified.LM.3.8)
summary(Melb.trainStratified.LM.3.8)

#remove MeanSize
Melb.trainStratified.LM.3.9<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X13_DestScore + X15_Parkiteer + X17_ACDist + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X32_PropBach, data = Stratifiedsample.train.800.rd2)
Melb.trainStratified.LM.3.9<-lm.beta(Melb.trainStratified.LM.3.9)
summary(Melb.trainStratified.LM.3.9)

plot(Melb.trainStratified.LM.3.9)

#621, 609 and 597 affecting assumptions
which(rownames(Stratifiedsample.train.800.rd2) == "621-train") #2
which(rownames(Stratifiedsample.train.800.rd2) == "609-train") #1
which(rownames(Stratifiedsample.train.800.rd2) == "597-train") #15
Stratifiedsample.train.800.rd3 <- Stratifiedsample.train.800.rd2[-c(2, 1, 15),]


#rd4
Melb.trainStratified.LM.4.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.1<-lm.beta(Melb.trainStratified.LM.4.1)
summary(Melb.trainStratified.LM.4.1)

#rd4
Melb.trainStratified.LM.4.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.1<-lm.beta(Melb.trainStratified.LM.4.1)
summary(Melb.trainStratified.LM.4.1)
capture.output(summary(Melb.trainStratified.LM.4.1), file = "trainStratified.MA.txt")

#removeInt Density
Melb.trainStratified.LM.4.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.2<-lm.beta(Melb.trainStratified.LM.4.2)
summary(Melb.trainStratified.LM.4.2)

#remove Mean Size
Melb.trainStratified.LM.4.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.3<-lm.beta(Melb.trainStratified.LM.4.3)
summary(Melb.trainStratified.LM.4.3)

#remove PropOS
Melb.trainStratified.LM.4.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X32_PropBach, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.4<-lm.beta(Melb.trainStratified.LM.4.4)
summary(Melb.trainStratified.LM.4.4)

#removePropBAch
Melb.trainStratified.LM.4.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.5<-lm.beta(Melb.trainStratified.LM.4.5)
summary(Melb.trainStratified.LM.4.5)

#remove LU Entropy
Melb.trainStratified.LM.4.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.6<-lm.beta(Melb.trainStratified.LM.4.6)
summary(Melb.trainStratified.LM.4.6)

#PropComm
Melb.trainStratified.LM.4.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.6<-lm.beta(Melb.trainStratified.LM.4.6)
summary(Melb.trainStratified.LM.4.6)

#Remove propcomm
Melb.trainStratified.LM.4.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv +  X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.7<-lm.beta(Melb.trainStratified.LM.4.7)
summary(Melb.trainStratified.LM.4.7)

#Remove DestScore
Melb.trainStratified.LM.4.8<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.8<-lm.beta(Melb.trainStratified.LM.4.8)
summary(Melb.trainStratified.LM.4.8)

#Remove ACDist
Melb.trainStratified.LM.4.9<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv + X15_Parkiteer + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.9<-lm.beta(Melb.trainStratified.LM.4.9)
summary(Melb.trainStratified.LM.4.9)

#Remove HousingDiv
Melb.trainStratified.LM.4.10<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X15_Parkiteer + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS, data = Stratifiedsample.train.800.rd3)
Melb.trainStratified.LM.4.10<-lm.beta(Melb.trainStratified.LM.4.10)
summary(Melb.trainStratified.LM.4.10)

Train.Stratified.PM.v2.VIF<- vif(Melb.trainStratified.LM.4.10)
capture.output(Bus.Stratified.PM.v2.VIF, file = "busStratified.PM.v2.VIF.txt")
Train.Stratified.PM.v2.VIF


plot(Melb.trainStratified.LM.4.10)

#627 is outlier - check if removing changes makeup of significant variables

which(rownames(Stratifiedsample.train.800.rd3) == "627-train") #1
Stratifiedsample.train.800.rd4 <- Stratifiedsample.train.800.rd3[-c(1),]


Melb.trainStratified.LM.5.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd4)
Melb.trainStratified.LM.5.1<-lm.beta(Melb.trainStratified.LM.5.1)
summary(Melb.trainStratified.LM.5.1)
#explanatory power significantly improved
capture.output(summary(Melb.trainStratified.LM.5.1), file = "trainStratified.MA.txt")

#remove mean size
Melb.trainStratified.LM.5.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd4)
Melb.trainStratified.LM.5.2<-lm.beta(Melb.trainStratified.LM.5.2)
summary(Melb.trainStratified.LM.5.2)

#prop comm
Melb.trainStratified.LM.5.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd4)
Melb.trainStratified.LM.5.3<-lm.beta(Melb.trainStratified.LM.5.3)
summary(Melb.trainStratified.LM.5.3)

#Dest Score
Melb.trainStratified.LM.5.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd4)
Melb.trainStratified.LM.5.4<-lm.beta(Melb.trainStratified.LM.5.4)
summary(Melb.trainStratified.LM.5.4)

#Int Density
Melb.trainStratified.LM.5.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv +X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd4)
Melb.trainStratified.LM.5.5<-lm.beta(Melb.trainStratified.LM.5.5)
summary(Melb.trainStratified.LM.5.5)

#LUEntropy
Melb.trainStratified.LM.5.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X10_HousingDiv +X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd4)
Melb.trainStratified.LM.5.6<-lm.beta(Melb.trainStratified.LM.5.6)
summary(Melb.trainStratified.LM.5.6)

#HousingDiv
Melb.trainStratified.LM.5.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance +X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X31_PropOS + X32_PropBach, data = Stratifiedsample.train.800.rd4)
Melb.trainStratified.LM.5.7<-lm.beta(Melb.trainStratified.LM.5.7)
summary(Melb.trainStratified.LM.5.7)

#PropOS
Melb.trainStratified.LM.5.8<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance +X15_Parkiteer + X17_ACDist + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X32_PropBach, data = Stratifiedsample.train.800.rd4)
Melb.trainStratified.LM.5.8<-lm.beta(Melb.trainStratified.LM.5.8)
summary(Melb.trainStratified.LM.5.8)

#ACDist
Melb.trainStratified.LM.5.9<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance +X15_Parkiteer + X18_ACCount + X19._FTZ +X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X32_PropBach, data = Stratifiedsample.train.800.rd4)
Melb.trainStratified.LM.5.9<-lm.beta(Melb.trainStratified.LM.5.9)
summary(Melb.trainStratified.LM.5.9)

capture.output(summary(Melb.trainStratified.LM.5.9), file = "trainStratified.PM.v2.txt")

#Same make up of significant variables; although population density closer to being significant. Suspect there is some interplay of distance to CBD with density. Yet model with CBD dist accommodates more observations (no outliers)
plot(Melb.trainStratified.LM.5.9)

Train.Stratified.PM.v2.VIF<- vif(Melb.trainStratified.LM.5.9)
capture.output(Train.Stratified.PM.v2.VIF, file = "trainStratified.PM.v2.VIF.txt")
Train.Stratified.PM.v2.VIF


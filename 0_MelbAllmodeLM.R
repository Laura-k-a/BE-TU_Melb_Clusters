library(Hmisc)#rcorr function

Melb_Data<- read.csv("BE-TR_AllStops_data.csv", header=TRUE, sep=",")
row.names(Melb_Data) <- Melb_Data[,c(2)]

Allmodes<- Melb_Data[which (Melb_Data$Standard. == 'yes'),]

Allmodes.VIF<-vif(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes))
Allmodes.VIF
#removed O_los VIF = 8.19

#step 4 Simple correlations
Corrdata.Allmodes<-Allmodes[,c(52, 54, 24, 26:31, 33:44, 49, 50, 55, 56)]

#Option 1 for Correlation matrices with p-values
Corrdata.Allmodes<-rcorr(as.matrix(Corrdata.Allmodes))

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

Corrdata.Allmodes<-flattenCorrMatrix(Corrdata.Allmodes$r,Corrdata.Allmodes$P)
capture.output(Corrdata.Allmodes,file="Corrdata.allmodes.csv")

#All Significant

#Step 4 maximally adjusted model
Allmodes.LM.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes)

Allmodes.LM.1<-lm.beta(Allmodes.LM.1)
summary(Allmodes.LM.1)
capture.output(summary(Allmodes.LM.1), file = "Allmodes.MA.txt")

#AcDist
Allmodes.LM.1.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes)

Allmodes.LM.1.2<-lm.beta(Allmodes.LM.1.2)
summary(Allmodes.LM.1.2)

#Intersection density
Allmodes.LM.1.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes)

Allmodes.LM.1.3<-lm.beta(Allmodes.LM.1.3)
summary(Allmodes.LM.1.3)

#FTZ
Allmodes.LM.1.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes)
Allmodes.LM.1.4<-lm.beta(Allmodes.LM.1.4)
summary(Allmodes.LM.1.4)

#cycleconnect
Allmodes.LM.1.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes)
Allmodes.LM.1.5<-lm.beta(Allmodes.LM.1.5)
summary(Allmodes.LM.1.5)

#PropComm
Allmodes.LM.1.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X16_CBDDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes)
Allmodes.LM.1.6<-lm.beta(Allmodes.LM.1.6)
summary(Allmodes.LM.1.6)

#CBDDist
Allmodes.LM.1.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X8_Balance + X9_LUEntropy + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes)
Allmodes.LM.1.7<-lm.beta(Allmodes.LM.1.7)
summary(Allmodes.LM.1.7)


par(mfrow=c(2,2))
plot(Allmodes.LM.1.7)
#1498-bus, 158-tram appears to be an outlier
#Very non-linear plot


which(rownames(Allmodes) == "1498-bus") #9474
which(rownames(Allmodes) == "158-tram") #10153
Allmodes.rd2 <- Allmodes[-c(9474,10153),]


#Round 2 MA
Allmodes.LM.2.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes.rd2)

Allmodes.LM.2.1<-lm.beta(Allmodes.LM.2.1)
summary(Allmodes.LM.2.1)
capture.output(summary(Allmodes.LM.2.1), file = "Allmodes.MA.txt")

#int density
Allmodes.LM.2.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes.rd2)
Allmodes.LM.2.2<-lm.beta(Allmodes.LM.2.2)
summary(Allmodes.LM.2.2)

#AC Dist
Allmodes.LM.2.3<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes.rd2)
Allmodes.LM.2.3<-lm.beta(Allmodes.LM.2.3)
summary(Allmodes.LM.2.3)

#Cycleconnect
Allmodes.LM.2.4<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X16_CBDDist + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes.rd2)
Allmodes.LM.2.4<-lm.beta(Allmodes.LM.2.4)
summary(Allmodes.LM.2.4)

#CBDDist
Allmodes.LM.2.5<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X18_ACCount + X19._FTZ + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes.rd2)
Allmodes.LM.2.5<-lm.beta(Allmodes.LM.2.5)
summary(Allmodes.LM.2.5)

#FTZ
Allmodes.LM.2.6<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes.rd2)
Allmodes.LM.2.6<-lm.beta(Allmodes.LM.2.6)
summary(Allmodes.LM.2.6)

#O_Train
Allmodes.LM.2.7<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X13_DestScore + X15_Parkiteer + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Allmodes.rd2)
Allmodes.LM.2.7<-lm.beta(Allmodes.LM.2.7)
summary(Allmodes.LM.2.7)


par(mfrow=c(2,2))
plot(Allmodes.LM.2.7)
#1498-bus, 158-tram appears to be an outlier
#Very non-linear plot

capture.output(summary(Allmodes.LM.2.7), file = "Allmodes.PM.v2.txt")
Allmodes.VIF<- vif(Allmodes.LM.2.7)
capture.output(Allmodes.VIF, file = "Allmodes.VIF.txt")
Allmodes.VIF

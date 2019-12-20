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
library(psych)
library(dplyr)

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
Melb.Trambus.noFTZ<-mutate(Melb.Trambus.noFTZ,
                  ln_Pop = log(Population_surrogate),
                  ln_Emp = log(EmpDen_surrogate),)
row.names(Melb.Trambus.noFTZ) <- Melb.Trambus.noFTZ[,c(2)]

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
Melb.Trambus.noFTZ.VIF<-vif(lm(ln_Centroid_Mode ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop + ln_Emp +X19_Parking, data =Melb.Trambus.noFTZ))
#removed  rural, overlapping level of service to get rid of singularity
#removed urban (high VIF)
Melb.Trambus.noFTZ.VIF

#step 4 Simple correlations
Corrdata.Trambus.noFTZ<-Melb.Trambus.noFTZ[,c(39,40,10,11,12,13,14,16,17,19,22,23,24,26,29,30,34,35,38,41,42)]

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
#X19_Parking

#exclude balance

#Step 4 maximally adjusted model

Melb.Trambus.noFTZ.MMLR.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Emp + ln_Pop, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.1)
Anova(Melb.Trambus.noFTZ.MMLR.1)

#AC Dist
Melb.Trambus.noFTZ.MMLR.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Emp + ln_Pop, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.2)
Anova(Melb.Trambus.noFTZ.MMLR.2)

#ln_EMp
Melb.Trambus.noFTZ.MMLR.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate + ln_Pop, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.3)
Anova(Melb.Trambus.noFTZ.MMLR.3)

#ACCount
Melb.Trambus.noFTZ.MMLR.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate + ln_Pop, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.4)
Anova(Melb.Trambus.noFTZ.MMLR.4)

#Meansize
Melb.Trambus.noFTZ.MMLR.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ DestScore_surrogate + ln_Pop, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.5)
Anova(Melb.Trambus.noFTZ.MMLR.5)

#pedconnect
Melb.Trambus.noFTZ.MMLR.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ DestScore_surrogate + ln_Pop, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.6)
Anova(Melb.Trambus.noFTZ.MMLR.6)

#destscore
Melb.Trambus.noFTZ.MMLR.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE + ln_Pop, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.7)
Anova(Melb.Trambus.noFTZ.MMLR.7)

#PBN
Melb.Trambus.noFTZ.MMLR.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+  X30_PropBach+ X34_censored_PropFTE + ln_Pop, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.8)
Anova(Melb.Trambus.noFTZ.MMLR.8)

#propcomm
Melb.Trambus.noFTZ.MMLR.9<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+  X30_PropBach+ X34_censored_PropFTE + ln_Pop, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.9)
Anova(Melb.Trambus.noFTZ.MMLR.9)
#makes a lot of sense

#diagnostics
plot(lm(ln_Centroid_Mode ~ X10_PropComm+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+  X30_PropBach+ X34_censored_PropFTE + ln_Pop, data =Melb.Trambus.noFTZ))
#influential outlier: 249
plot(lm(ln_Bus ~ X10_PropComm+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+  X30_PropBach+ X34_censored_PropFTE + ln_Pop, data =Melb.Trambus.noFTZ))
#looks ok

which(rownames(Melb.Trambus.noFTZ) == "249-tram") #151

#remove potentially influential outliers (just the ones close to cook's distance)
Melb.Trambus.noFTZ.rd2 <- Melb.Trambus.noFTZ[-c(151),]

#rerun maximally adjusted model
Melb.Trambus.noFTZ.MMLR.2.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Emp + ln_Pop, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.1)
Anova(Melb.Trambus.noFTZ.MMLR.2.1)

#ln_Emp
Melb.Trambus.noFTZ.MMLR.2.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate + ln_Pop, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.2)
Anova(Melb.Trambus.noFTZ.MMLR.2.2)

#ACDIst
Melb.Trambus.noFTZ.MMLR.2.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+  X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate + ln_Pop, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.3)
Anova(Melb.Trambus.noFTZ.MMLR.2.3)

#AC Count
Melb.Trambus.noFTZ.MMLR.2.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate + ln_Pop, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.4)
Anova(Melb.Trambus.noFTZ.MMLR.2.4)

#pedconnect
Melb.Trambus.noFTZ.MMLR.2.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate + ln_Pop, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.5)
Anova(Melb.Trambus.noFTZ.MMLR.2.5)

#meansize
Melb.Trambus.noFTZ.MMLR.2.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ DestScore_surrogate + ln_Pop, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.6)
Anova(Melb.Trambus.noFTZ.MMLR.2.6)

#destscore
Melb.Trambus.noFTZ.MMLR.2.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.7)
Anova(Melb.Trambus.noFTZ.MMLR.2.7)

#PBN
Melb.Trambus.noFTZ.MMLR.2.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.8)
Anova(Melb.Trambus.noFTZ.MMLR.2.8)
#makes sense
#diagnostics

plot(lm(ln_Bus~ X10_PropComm+ X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop, data =Melb.Trambus.noFTZ.rd2))
#shape does not fit assumptions. Try removing 258

plot(lm(ln_Centroid_Mode~ X10_PropComm+ X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop, data =Melb.Trambus.noFTZ.rd2))
#investigate 276 - not influential but most outlying. Otherwise fits assumptions

which(rownames(Melb.Trambus.noFTZ.rd2) == "258-tram") #151

#remove potentially influential outliers (just the ones close to cook's distance)
Melb.Trambus.noFTZ.rd3 <- Melb.Trambus.noFTZ.rd2[-c(151),]

#maximally adjusted model
Melb.Trambus.noFTZ.MMLR.3.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Emp + ln_Pop, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.1)
Anova(Melb.Trambus.noFTZ.MMLR.3.1)

#ACDist
Melb.Trambus.noFTZ.MMLR.3.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Emp + ln_Pop, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.2)
Anova(Melb.Trambus.noFTZ.MMLR.3.2)


#ln_Emp
Melb.Trambus.noFTZ.MMLR.3.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.3)
Anova(Melb.Trambus.noFTZ.MMLR.3.3)

#ACCount
Melb.Trambus.noFTZ.MMLR.3.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.4)
Anova(Melb.Trambus.noFTZ.MMLR.3.4)

#meansize
Melb.Trambus.noFTZ.MMLR.3.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.5)
Anova(Melb.Trambus.noFTZ.MMLR.3.5)

#pedconnect
Melb.Trambus.noFTZ.MMLR.3.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+  X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.6)
Anova(Melb.Trambus.noFTZ.MMLR.3.6)

#destscore
Melb.Trambus.noFTZ.MMLR.3.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+  X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ln_Pop, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.7)
Anova(Melb.Trambus.noFTZ.MMLR.3.7)

#PBN
Melb.Trambus.noFTZ.MMLR.3.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+  X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ln_Pop, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.8)
Anova(Melb.Trambus.noFTZ.MMLR.3.8)
#makes sense

#diagnostics
plot(lm(ln_Bus~X10_PropComm+ X12_LUEntropy+  X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ln_Pop, data =Melb.Trambus.noFTZ.rd3))
#still does not meet linearitty assumptions.  main outlier is 2198

plot(lm(ln_Centroid_Mode~X10_PropComm+ X12_LUEntropy+  X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ln_Pop, data =Melb.Trambus.noFTZ.rd3))
#meets assumptions

#try removing 2198
which(rownames(Melb.Trambus.noFTZ.rd3) == "2198-tram") #148

#remove potentially influential outliers (just the ones close to cook's distance)
Melb.Trambus.noFTZ.rd4 <- Melb.Trambus.noFTZ.rd3[-c(148),]

#maximally adjusted model
Melb.Trambus.noFTZ.MMLR.4.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Emp + ln_Pop, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.1)
Anova(Melb.Trambus.noFTZ.MMLR.4.1)

#ACDist
Melb.Trambus.noFTZ.MMLR.4.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Emp + ln_Pop, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.2)
Anova(Melb.Trambus.noFTZ.MMLR.4.2)

#ln_Emp
Melb.Trambus.noFTZ.MMLR.4.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.3)
Anova(Melb.Trambus.noFTZ.MMLR.4.3)

#ACCount
Melb.Trambus.noFTZ.MMLR.4.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.4)
Anova(Melb.Trambus.noFTZ.MMLR.4.4)

#meansize
Melb.Trambus.noFTZ.MMLR.4.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.5)
Anova(Melb.Trambus.noFTZ.MMLR.4.5)

#pedconnect
Melb.Trambus.noFTZ.MMLR.4.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.6)
Anova(Melb.Trambus.noFTZ.MMLR.4.6)

#Destscore
Melb.Trambus.noFTZ.MMLR.4.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X14_PBN+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE +ln_Pop, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.7)
Anova(Melb.Trambus.noFTZ.MMLR.4.7)

#PBN
Melb.Trambus.noFTZ.MMLR.4.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+ X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE +ln_Pop, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.8)
Anova(Melb.Trambus.noFTZ.MMLR.4.8)
#makes sense, resembles all prior solutions

#diagnostics
plot(lm(ln_Bus ~ X10_PropComm+ X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE +ln_Pop, data =Melb.Trambus.noFTZ.rd4))

#fits assumptions much better, but 260 still impacting shape (investigate ,but accept solutio)

plot(lm(ln_Centroid_Mode ~ X10_PropComm+ X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE +ln_Pop, data =Melb.Trambus.noFTZ.rd4))

#could see if taking 260 out impacts the mix of variables

capture.output(summary(Melb.Trambus.noFTZ.MMLR.4.8), file = "Melb.Trambus.noFTZ.MMLR.4.8.txt")

MMLR.trambus.tram<-lm(ln_Centroid_Mode ~ X10_PropComm+ X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE +ln_Pop, data =Melb.Trambus.noFTZ.rd4)

MMLR.trambus.bus<-lm(ln_Bus ~ X10_PropComm+ X12_LUEntropy+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE +ln_Pop, data =Melb.Trambus.noFTZ.rd4)

MMLR.trambus.tram<-lm.beta(MMLR.trambus.tram)
MMLR.trambus.bus<-lm.beta(MMLR.trambus.bus)

options(scipen = 999)
capture.output(summary(MMLR.trambus.bus), file = "MMLR.trambus.bus.txt")
capture.output(summary(MMLR.trambus.tram), file = "MMLR.trambus.tram.txt")

#Trainbus

Melb.Trainbus<-mutate(Melb.Trainbus,
                  ln_Pop = log(Population_surrogate),
                  ln_Emp = log(EmpDen_surrogate),)

row.names(Melb.Trainbus) <- Melb.Trainbus[,c(2)]
#step 3 Check for multicolinearity

Melb.Trainbus.VIF<-vif(lm(ln_Centroid_Mode ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X17_ACCount+ X19_Parking+ X21_PropRural + X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X25_O_Tram_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop , data =Melb.Trainbus))
#removed FTZ, overlapping level of service to get rid of singularity
#removed ln_Emp, ln_Urban due to collinearity
Melb.Trainbus.VIF
#no colinearty

#step 4 Simple correlations
Corrdata.Trainbus<-Melb.Trainbus[,c(39,40,10,11,12,13,14,15,16,17,19, 21,22,23,24,25,29,30,34,35,38, 41)]

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
#ln_Pop

#ln_Tram
#X21_PropRural
#X25_O_Tram_LOS
#X35_censored_MeanSize


#exclude PropRural, Tram LOS and Meansize

#Step 4 maximally adjusted model

Melb.Trainbus.MMLR.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop+ DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.1)
Anova(Melb.Trainbus.MMLR.1)

Melb.Trainbus.MMLR.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop+ DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.1)
Anova(Melb.Trainbus.MMLR.1)

#remove LUEntropy
Melb.Trainbus.MMLR.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop+ DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.2)
Anova(Melb.Trainbus.MMLR.2)

#remove propFTE
Melb.Trainbus.MMLR.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ ln_Pop+ DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.3)
Anova(Melb.Trainbus.MMLR.3)

#remove propcommercial
Melb.Trainbus.MMLR.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ ln_Pop+ DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.4)
Anova(Melb.Trainbus.MMLR.4)

#removeEmpAccess
Melb.Trainbus.MMLR.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ ln_Pop+ DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.5)
Anova(Melb.Trainbus.MMLR.5)

#remove PropOS
Melb.Trainbus.MMLR.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach+ ln_Pop+ DestScore_surrogate, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.6)
Anova(Melb.Trainbus.MMLR.6)

#remove DestScore
Melb.Trainbus.MMLR.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach+ ln_Pop, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.7)
Anova(Melb.Trainbus.MMLR.7)

#remove pedconnect
Melb.Trainbus.MMLR.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach+ ln_Pop, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.8)
Anova(Melb.Trainbus.MMLR.8)

#remove ln_Pop
Melb.Trainbus.MMLR.9<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.9)
Anova(Melb.Trainbus.MMLR.9)

#remove parking
Melb.Trainbus.MMLR.10<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+  +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus)
summary(Melb.Trainbus.MMLR.10)
Anova(Melb.Trainbus.MMLR.10)

#diagnostics
par(mfrow=c(2,2))
plot(lm(ln_Bus ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+  +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus))

#outlier (not influential): 572, 451

plot(lm(ln_Centroid_Mode ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+  +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus))

#outlier (not influential): rows 553, 636, 632
which(rownames(Melb.Trainbus) == "572-train") #14
which(rownames(Melb.Trainbus) == "451-train") #15
which(rownames(Melb.Trainbus) == "553-train") #37
which(rownames(Melb.Trainbus) == "636-train") #1
which(rownames(Melb.Trainbus) == "632-train") #2

#remove potentially influential outliers (just the ones close to cook's distance)
Melb.Trainbus.rd2 <- Melb.Trainbus[-c(1, 14, 15),]

#rerun maximally adjusted
Melb.Trainbus.MMLR.2.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop+ DestScore_surrogate, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.1)
Anova(Melb.Trainbus.MMLR.2.1)

#remove LUEntropy
Melb.Trainbus.MMLR.2.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop+ DestScore_surrogate, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.2)
Anova(Melb.Trainbus.MMLR.2.2)

#remove destscore
Melb.Trainbus.MMLR.2.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.3)
Anova(Melb.Trainbus.MMLR.2.3)

#propFTE
Melb.Trainbus.MMLR.2.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach +ln_Pop, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.4)
Anova(Melb.Trainbus.MMLR.2.4)

#remove lnPop
Melb.Trainbus.MMLR.2.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.5)
Anova(Melb.Trainbus.MMLR.2.5)

#pedconnect
Melb.Trainbus.MMLR.2.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.6)
Anova(Melb.Trainbus.MMLR.2.6)

#propcomm
Melb.Trainbus.MMLR.2.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.7)
Anova(Melb.Trainbus.MMLR.2.7)

#empaccess
Melb.Trainbus.MMLR.2.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.8)
Anova(Melb.Trainbus.MMLR.2.8)

#propOS
Melb.Trainbus.MMLR.2.9<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.9)
Anova(Melb.Trainbus.MMLR.2.9)

#parking
Melb.Trainbus.MMLR.2.10<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus.rd2)
summary(Melb.Trainbus.MMLR.2.10)
Anova(Melb.Trainbus.MMLR.2.10)
#LOOKS GOOOD!!! MAKES SENSE!!

#diagnostics
plot(lm(ln_Bus ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus.rd2))
#conforms with expected patterns, investigate 455, 553 and 547 but not influential

plot(lm(ln_Centroid_Mode ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X16_ACDist +X17_ACCount+ X23_C_LOS+ X24_O_Bus_LOS+ X30_PropBach, data =Melb.Trainbus.rd2))
#remove 615 and 632


which(rownames(Melb.Trainbus.rd2) == "632-train") #1
which(rownames(Melb.Trainbus.rd2) == "615-train") #2

#remove potentially influential outliers (just the ones close to cook's distance)
Melb.Trainbus.rd3 <- Melb.Trainbus.rd2[-c(1, 2),]

Melb.Trainbus.MMLR.3.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop+ DestScore_surrogate, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.1)
Anova(Melb.Trainbus.MMLR.3.1)

#remove destscore surrogate
Melb.Trainbus.MMLR.3.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ X34_censored_PropFTE+ ln_Pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.2)
Anova(Melb.Trainbus.MMLR.3.2)

#propFTE
Melb.Trainbus.MMLR.3.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ X30_PropBach+ ln_Pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.3)
Anova(Melb.Trainbus.MMLR.3.3)

#propBach
Melb.Trainbus.MMLR.3.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ ln_Pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.4)
Anova(Melb.Trainbus.MMLR.3.4)

#LUEntropy
Melb.Trainbus.MMLR.3.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+  X13_PedConnect+ X14_PBN+ X15_Parkiteer + X16_ACDist+ X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ ln_Pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.5)
Anova(Melb.Trainbus.MMLR.3.5)

#ACDist
Melb.Trainbus.MMLR.3.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+  X13_PedConnect+ X14_PBN+ X15_Parkiteer + X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X29_PropOS+ ln_Pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.6)
Anova(Melb.Trainbus.MMLR.3.6)

#propOS
Melb.Trainbus.MMLR.3.7<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X11_Balance+  X13_PedConnect+ X14_PBN+ X15_Parkiteer + X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ ln_Pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.7)
Anova(Melb.Trainbus.MMLR.3.7)

#propComm
Melb.Trainbus.MMLR.3.8<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+  X13_PedConnect+ X14_PBN+ X15_Parkiteer + X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ ln_Pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.8)
Anova(Melb.Trainbus.MMLR.3.8)

#pedconnect
Melb.Trainbus.MMLR.3.9<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ ln_Pop, data =Melb.Trainbus.rd3)
summary(Melb.Trainbus.MMLR.3.9)
Anova(Melb.Trainbus.MMLR.3.9)
#ALso makes sense; notice that density and access to employment are now significant. This is important

#diagnostics
plot(lm(ln_Bus ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ ln_Pop, data =Melb.Trainbus.rd3))
#outliers, but not influential: 455, 638, 547

plot(lm(ln_Centroid_Mode ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ ln_Pop, data =Melb.Trainbus.rd3))
#outliers, but not infulential: 455, 531, 493

capture.output(summary(Melb.Trainbus.MMLR.3.9), file = "Melb.Trainbus.MMLR.3.9.txt")

MMLR.trainbus.train<-lm(ln_Centroid_Mode ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ ln_Pop, data =Melb.Trainbus.rd3)

MMLR.trainbus.bus<-lm(ln_Bus ~ X11_Balance+ X14_PBN+ X15_Parkiteer + X19_Parking +X17_ACCount+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ ln_Pop, data =Melb.Trainbus.rd3)

MMLR.trainbus.train<-lm.beta(MMLR.trainbus.train)
MMLR.trainbus.bus<-lm.beta(MMLR.trainbus.bus)

options(scipen = 999)
capture.output(summary(MMLR.trainbus.bus), file = "MMLR.trainbus.bus.txt")
capture.output(summary(MMLR.trainbus.train), file = "MMLR.trainbus.train.txt")

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

#Set working directory
setwd("C:/Users/lkast1/Google Drive/PhD/2.Analysis/2. Empirical Analysis/BE-TR_Multi Country Samples/Melbourne/Melb.All.Stops/Melb.AllStops.Repo/30Dec19")
#read in data

Colocated_Data<-read.csv(file="BE-TR_Co-Located_data.csv")

#optional - assign the Sample ID as the row names
row.names(Colocated_Data) <- Colocated_Data[,c(2)]

#subsetting for mode in the census of all eligible stops

Melb.Trambus.noFTZ<- Colocated_Data[ which(Colocated_Data$Mode=='tram'
                                & Colocated_Data$FTZ=='0'),]
Melb.Trainbus<- Colocated_Data[ which(Colocated_Data$Mode=='train'),]

#Step 2 estimate covariance of the outcome variables
cov_Trambus.noFTZ<-cor.test(Melb.Trambus.noFTZ$ln_bus, Melb.Trambus.noFTZ$ln_centroid, method = "pearson", conf.level = 0.95)
cov_Trambus.noFTZ #0.12  p < 0.02

cov_Trambus_ln.noFTZ #0.29 p = <0.000

capture.output(cov_Trambus_ln.noFTZ,file="cov_Trambus_ln.noFTZ.txt")

cov_Trainbus<-cor.test(Melb.Trainbus$ln_bus, Melb.Trainbus$ln_centroid, method = "pearson", conf.level = 0.95)
cov_Trainbus #0.76, 

capture.output(cov_Trainbus_ln,file="cov_Trainbus_ln.txt")

#step 3 Check for multicolinearity
Melb.Trambus.noFTZ.VIF<-vif(lm(ln_centroid ~ PropComm+Balance+LUEntropy+PedConnect+PBN+ ACDist+ ACCount+EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate + ln_Emp_surrogate +Parking, data =Melb.Trambus.noFTZ))
#removed  rural, overlapping level of service to get rid of singularity
#removed urban (high VIF)
#remove parkiteer (no variance)
Melb.Trambus.noFTZ.VIF

#step 4 Simple correlations
Corrdata.Trambus.noFTZ<-Melb.Trambus.noFTZ[,c(11:17, 19, 20, 22, 25:35, 37, 38)]

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
#Balance
#LUEntropy
#C_LOS
#PropOS
#PropBach
#X35_censored_MeanSize


#ln_Tram
#Balance
#Parking
#O_Tram_LOS
#MedInc


#exclude balance

#Step 4 maximally adjusted model

Melb.Trambus.noFTZ.MMLR.1<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACDist+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Tram_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate + ln_Emp_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.1)
#was signular until parking removed
Anova(Melb.Trambus.noFTZ.MMLR.1)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.1), file = "Trambus.MA.summary.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.1), file = "Trambus.MA.anova.txt")

#Remove Tram_O_LOS
Melb.Trambus.noFTZ.MMLR.2<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACDist+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate + ln_Emp_surrogate, data =Melb.Trambus.noFTZ)

summary(Melb.Trambus.noFTZ.MMLR.2)
Anova(Melb.Trambus.noFTZ.MMLR.2)

#ACDIst
Melb.Trambus.noFTZ.MMLR.3<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+  ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate + ln_Emp_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.3)
Anova(Melb.Trambus.noFTZ.MMLR.3)

#ln_Emp_surrogate
Melb.Trambus.noFTZ.MMLR.4<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+  ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.4)
Anova(Melb.Trambus.noFTZ.MMLR.4)

#ACCount
Melb.Trambus.noFTZ.MMLR.5<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+   EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.5)
Anova(Melb.Trambus.noFTZ.MMLR.5)

#meansize
Melb.Trambus.noFTZ.MMLR.6<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+   EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.6)
Anova(Melb.Trambus.noFTZ.MMLR.6)

#ped connect
Melb.Trambus.noFTZ.MMLR.7<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+   EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.7)
Anova(Melb.Trambus.noFTZ.MMLR.7)

#destscore
Melb.Trambus.noFTZ.MMLR.8<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+   EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ ln_Pop_surrogate, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.8)
Anova(Melb.Trambus.noFTZ.MMLR.8)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.8), file = "Trambus.PM.summary.rd1.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.8), file = "Trambus.PM.anova.rd1.txt")

#diagnostics
par(mfrow=c(2,2))

plot(lm(ln_centroid ~ PropComm + LUEntropy+ PBN+   EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ ln_Pop_surrogate, data =Melb.Trambus.noFTZ))

#249 is influential outlier

plot(lm(ln_bus ~ PropComm + LUEntropy+ PBN+   EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ ln_Pop_surrogate, data =Melb.Trambus.noFTZ))

#258 is outlier, but don't remove

which(rownames(Melb.Trambus.noFTZ) == "249-tram") #151

#remove potentially influential outliers (just the ones close to cook's distance)
Melb.Trambus.noFTZ.rd2 <- Melb.Trambus.noFTZ[-c(151),]


#Round 2 maximally adjusted model
Melb.Trambus.noFTZ.MMLR.2.1<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACDist+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate + ln_Emp_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.1)
Anova(Melb.Trambus.noFTZ.MMLR.2.1)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.2.1), file = "trambus.MA.summary.rd2.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.2.1), file = "trambus.MA.anova.rd2.txt")

#remove ln_Emp
Melb.Trambus.noFTZ.MMLR.2.2<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACDist+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.2)
Anova(Melb.Trambus.noFTZ.MMLR.2.2)

#AC Dist
Melb.Trambus.noFTZ.MMLR.2.3<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.3)
Anova(Melb.Trambus.noFTZ.MMLR.2.3)

#ACCount
Melb.Trambus.noFTZ.MMLR.2.3<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.3)
Anova(Melb.Trambus.noFTZ.MMLR.2.3)

#removepedconnect
Melb.Trambus.noFTZ.MMLR.2.4<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.4)
Anova(Melb.Trambus.noFTZ.MMLR.2.4)

#meansize
Melb.Trambus.noFTZ.MMLR.2.5<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.5)
Anova(Melb.Trambus.noFTZ.MMLR.2.5)

#destscore
Melb.Trambus.noFTZ.MMLR.2.6<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.6)
Anova(Melb.Trambus.noFTZ.MMLR.2.6)

#PBN
Melb.Trambus.noFTZ.MMLR.2.7<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+  EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.7)
Anova(Melb.Trambus.noFTZ.MMLR.2.7)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.2.7), file = "trambus.PM.summary.rd.2.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.2.7), file = "trambus.PM.anova.rd.2.txt")

#dianostics
plot(lm(ln_centroid ~ PropComm + LUEntropy+  EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd2))
#no distinctive pattern, although 210, 276 and 1942 deviate fom the residual plot and are outliers on all graphs. Investigate graphically

plot(lm(ln_bus ~ PropComm + LUEntropy+  EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd2))
#deviates from normality assumption. 
#Remove 258 tram

which(rownames(Melb.Trambus.noFTZ.rd2) == "258-tram") #151

#remove potentially influential outliers (just the ones close to cook's distance)
Melb.Trambus.noFTZ.rd3 <- Melb.Trambus.noFTZ.rd2[-c(151),]

#Round 3 maximally adjusted model
Melb.Trambus.noFTZ.MMLR.3.1<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACDist+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate + ln_Emp_surrogate, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.1)
Anova(Melb.Trambus.noFTZ.MMLR.3.1)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.3.1), file = "trambus.MA.summary.rd3.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.3.1), file = "trambus.MA.anova.rd3.txt")

#remove ln_emp
Melb.Trambus.noFTZ.MMLR.3.2<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACDist+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.2)
Anova(Melb.Trambus.noFTZ.MMLR.3.2)

#ACDist
Melb.Trambus.noFTZ.MMLR.3.3<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.3)
Anova(Melb.Trambus.noFTZ.MMLR.3.3)

#ACCount
Melb.Trambus.noFTZ.MMLR.3.4<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.4)
Anova(Melb.Trambus.noFTZ.MMLR.3.4)

#meansize
Melb.Trambus.noFTZ.MMLR.3.5<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.5)
Anova(Melb.Trambus.noFTZ.MMLR.3.5)

#pedconnect
Melb.Trambus.noFTZ.MMLR.3.6<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.6)
Anova(Melb.Trambus.noFTZ.MMLR.3.6)

#destscore
Melb.Trambus.noFTZ.MMLR.3.7<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.7)
Anova(Melb.Trambus.noFTZ.MMLR.3.7)

#PBN
Melb.Trambus.noFTZ.MMLR.3.8<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.8)
Anova(Melb.Trambus.noFTZ.MMLR.3.8)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.3.8), file = "trambus.PM.summary.rd.3.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.3.8), file = "trambus.PM.anova.rd.3.txt")

#diagnostics
plot(lm(ln_centroid ~ PropComm + LUEntropy+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd3))
#appears as per rd 2

plot(lm(ln_bus ~ PropComm + LUEntropy+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd3))
#2198 impacts normaliy assumption

which(rownames(Melb.Trambus.noFTZ.rd3) == "2198-tram") #148
Melb.Trambus.noFTZ.rd4 <- Melb.Trambus.noFTZ.rd3[-c(148),]

#rd 4 maximally adjusted model
Melb.Trambus.noFTZ.MMLR.4.1<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACDist+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate + ln_Emp_surrogate, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.1)
Anova(Melb.Trambus.noFTZ.MMLR.4.1)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.4.1), file = "trambus.MA.summary.rd4.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.4.1), file = "trambus.MA.anova.rd4.txt")

#AcDist
Melb.Trambus.noFTZ.MMLR.4.2<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate + ln_Emp_surrogate, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.2)
Anova(Melb.Trambus.noFTZ.MMLR.4.2)

#ln_Emp
Melb.Trambus.noFTZ.MMLR.4.3<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.3)
Anova(Melb.Trambus.noFTZ.MMLR.4.3)

#AcCount
Melb.Trambus.noFTZ.MMLR.4.4<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.4)
Anova(Melb.Trambus.noFTZ.MMLR.4.4)

#meansize
Melb.Trambus.noFTZ.MMLR.4.5<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.5)
Anova(Melb.Trambus.noFTZ.MMLR.4.5)

#pedconnect
Melb.Trambus.noFTZ.MMLR.4.6<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.6)
Anova(Melb.Trambus.noFTZ.MMLR.4.6)

#destscore
Melb.Trambus.noFTZ.MMLR.4.7<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.7)
Anova(Melb.Trambus.noFTZ.MMLR.4.7)

#PBN
Melb.Trambus.noFTZ.MMLR.4.8<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd4)
summary(Melb.Trambus.noFTZ.MMLR.4.8)
Anova(Melb.Trambus.noFTZ.MMLR.4.8)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.4.8), file = "trambus.PM.summary.rd.4.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.4.8), file = "trambus.PM.anova.rd.4.txt")

#diagnostics
plot(lm(ln_bus ~ PropComm + LUEntropy+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd4))
#260 affecting normality assumption

plot(lm(ln_centroid ~ PropComm + LUEntropy+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd4))

which(rownames(Melb.Trambus.noFTZ.rd4) == "260-tram") #148
Melb.Trambus.noFTZ.rd5 <- Melb.Trambus.noFTZ.rd4[-c(148),]

#rd 5 maximally adjusted model
Melb.Trambus.noFTZ.MMLR.5.1<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACDist+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate + ln_Emp_surrogate, data =Melb.Trambus.noFTZ.rd5)
summary(Melb.Trambus.noFTZ.MMLR.5.1)
Anova(Melb.Trambus.noFTZ.MMLR.5.1)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.5.1), file = "trambus.MA.summary.rd5.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.5.1), file = "trambus.MA.anova.rd5.txt")

#ln_EMp
Melb.Trambus.noFTZ.MMLR.5.2<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACDist+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd5)
summary(Melb.Trambus.noFTZ.MMLR.5.2)
Anova(Melb.Trambus.noFTZ.MMLR.5.2)

#AC Dist
Melb.Trambus.noFTZ.MMLR.5.3<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ ACCount+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd5)
summary(Melb.Trambus.noFTZ.MMLR.5.3)
Anova(Melb.Trambus.noFTZ.MMLR.5.3)

#ACCount
Melb.Trambus.noFTZ.MMLR.5.4<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ X35_censored_MeanSize+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd5)
summary(Melb.Trambus.noFTZ.MMLR.5.4)
Anova(Melb.Trambus.noFTZ.MMLR.5.4)

#MeanSize
Melb.Trambus.noFTZ.MMLR.5.5<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PedConnect+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd5)
summary(Melb.Trambus.noFTZ.MMLR.5.5)
Anova(Melb.Trambus.noFTZ.MMLR.5.5)

#pedconnect
Melb.Trambus.noFTZ.MMLR.5.6<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE+ DestScore_surrogate +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd5)
summary(Melb.Trambus.noFTZ.MMLR.5.6)
Anova(Melb.Trambus.noFTZ.MMLR.5.6)

#destscore
Melb.Trambus.noFTZ.MMLR.5.7<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ PBN+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd5)
summary(Melb.Trambus.noFTZ.MMLR.5.7)
Anova(Melb.Trambus.noFTZ.MMLR.5.7)

#PBN
Melb.Trambus.noFTZ.MMLR.5.8<-lm(cbind(ln_bus,ln_centroid) ~ PropComm + LUEntropy+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd5)
summary(Melb.Trambus.noFTZ.MMLR.5.8)
Anova(Melb.Trambus.noFTZ.MMLR.5.8)

capture.output(summary(Melb.Trambus.noFTZ.MMLR.5.8), file = "trambus.PM.summary.rd5.txt")
capture.output(Anova(Melb.Trambus.noFTZ.MMLR.5.8), file = "trambus.PM.anova.rd5.txt")

#diagnostics
plot(lm(ln_centroid ~ PropComm + LUEntropy+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd5))

plot(lm(ln_bus ~ PropComm + LUEntropy+ EmpAccess+C_LOS+O_Bus_LOS+O_Train_LOS+ PropOS+ PropBach+ X34_censored_PropFTE +ln_Pop_surrogate, data =Melb.Trambus.noFTZ.rd5))
#still violating normality assumption due to some outliers. leave as is. 